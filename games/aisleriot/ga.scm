(define-module (aisleriot ga)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 futures)
  #:use-module (ice-9 local-eval)
  #:use-module (ice-9 match) ;; TEMP
  #:use-module (ice-9 popen)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module ((srfi srfi-18) #:prefix srfi:)
  #:use-module ((srfi srfi-19) #:prefix srfi:)
  #:use-module (srfi srfi-26)
  #:use-module (oop goops)
  #:use-module (rnrs base)
  #:use-module (rnrs bytevectors)
  #:use-module (aisleriot formatt)
  #:use-module (aisleriot math)
  #:use-module (aisleriot nn)
  #:use-module (aisleriot permute)
  #:use-module (aisleriot queue-mp)
  #:use-module (aisleriot serialize)
  #:use-module (aisleriot time)
  #:use-module (statprof)
  #:re-export (serialize)
  #:export (<ga-automation-run>
			<ga-genome>
			<ga-state>

			genome-get
			name-get
			seed-get
			subject-get

			ga-load
			ga-population-size
			ga-training-set-size
			ga-evolve
			
			ga-evaluate
			ga-client-receive-automation-runs
			ga-is-automation)
  )

;;; Observations:
;;; Each population member has new game.
;;; Successive generations bred by two fittest.
;;; 1 hidden layer having 6 nodes
;;;  Max moves 100
;;;   Population size 5
;;;    Mutation 0.1, combine 0.5, did not improve fitness after 1000 generations. 
;;;    Mutation 0.1, combine 0.3333, did not improve fitness after 1000 generations. 
;;;    Mutation 0.01, combine 0.5, did not improve fitness after 1000 generations. 
;;;   Population size 100
;;;    Mutation 0.01, combine 0.5, after 13 generations best fitness was 40.
;;;   Population size 50
;;;    Mutation 0.001, combine 0.5, after 13 generations best fitness was 30. Highest seen was 50.
;;;    Mutation 0.001, combine 0.5, after 269 generations best fitness was 30. Highest seen was 50.
;;; New parent selection, aiming for greater diversity.
;;;    Mutation 0.001, combine 0.5, pop size 1000, after 73 generations best fitness was 60,
;;;     average was 13.89 (top 14.65)
;;; Experiment modification: use same game for entire generation.
;;;  Mutation 0.001, combine 0.5, pop size 100, after 45 generations best fitness was 45, avg 10
;;; 2 hidden layers having 6 nodes
;;;  Mutation 0.001, combine 0.5, pop size 100, after 10 generations avg fitness was 10
;;; 2 hidden layers having 24 nodes
;;;  Mutation 0.001, combine 0.5, pop size 100, after 10 generations avg fitness was 14.8
;;;
;;; Change game state and move encoding. One input group per slot with length, top suit, top value. One input group
;;;  for move cards, one for deal.
;;;  Mutation 0.001, combine 0.5, pop size 100, after 22 generations avg fitness was 5. Best 10. Poor result.
;;;  Mutation 0.001, combine 0.5, pop size 10, after 233 generations avg fitness was 5. Best 25.
;;;  Mutation 0.01, combine 0.5, pop size 10, after 40 generations avg fitness was 5. Best 10.
;;;  Note: Bug. Gamestate was not being set in deal case, bad caching.
;;; Old parent seleciton
;;;  Not a big difference from using old parent selection with pop size 10 and mutation 0.001.
;;; Changed possible weight range from 0.0->1.0 to -999999999.0->999999999.0.
;;;  A far greater range of behaviours was observed (i.e. networks were behaving much more differently.)
;;; Note: previous conclusions about the number of hidden layers should be discounted due to a bug.
;;; Trying to win a single same game for now. Think one problem is that when all solutions are equal then some are being
;;; needlessly conserved. Need to change parent calcuation. PS: This was probably due to total bug with
;;; caching/parallel.
;;; 

(define (par-mapper mapper cons)
  (lambda (proc . lists)
    (let loop ((lists lists))
      (match lists
        (((heads tails ...) ...)
         (let* ((tail (future (loop tails)))
				(head (apply proc heads)))
           (cons head (touch tail))))
        (_
         '())))))

(define par-map (par-mapper map cons))

(define ga-mutex-complete (make-mutex))
(define ga-cond-complete (make-condition-variable))
(define queue-runs (make <queue-mp>))

;; CSV
(define (list->csv list)
  (string-join (map object->string list) ",")
  )

;; Name generation
(define vowels (string->char-set "aeiou"))
(define consonants (char-set-difference
					(char-set-intersection char-set:lower-case char-set:ascii)
					  vowels))
(define (charset-random-char charset) (list-ref (char-set->list charset) (random (char-set-size charset))))
(define (generate-name nclusters)
  (cond ((= 0 nclusters) "")
		(else (string-append
			   (string (charset-random-char vowels) (charset-random-char consonants))
			   (generate-name (1- nclusters))))))

;; Distributed execution
(define-class <ga-state> (<serializable>)
  (-n-generation #:init-keyword #:n-generation #:init-value 0 #:getter n-generation-get)
  (-genomes #:init-keyword #:genomes #:getter genomes-get)
  (-seeds #:init-keyword #:seeds #:getter seeds-get)
  (-stats #:init-keyword #:stats #:init-value '() #:getter stats-get)
  )

(define-method (first? (state <ga-state>))
  (= (n-generation-get state) 0)
  )

(define-method (next (state <ga-state>) genomes-new stats-new)
  (make <ga-state>
	#:genomes genomes-new #:seeds (seeds-get state) #:stats stats-new #:n-generation (1+ (n-generation-get state)))
  )

(define-method (serialize-slots-get (self <ga-state>))
  (append (next-method) '(-n-generation -genomes -seeds -stats))
  )

(define-class <ga-automation-run> (<serializable>)
  (-seed #:init-keyword #:seed #:getter seed-get)
  (-genome #:init-keyword #:genome #:getter genome-get)
  (-callback-on-complete #:init-keyword #:on-complete)
  (-time-start #:getter time-start-get)
  (-time-end #:getter time-end-get)
  (-serialized)
  )

(define (prepare-automation-run run)
  (slot-set! run '-serialized (format #f "~s" (serialize run)))
  run
  )

(define (serialized-get! run)
  (let ((result (slot-ref run '-serialized)))
	(slot-set! run '-serialized 'cleared)
	result
	)
  )

(define-method (inspect (self <ga-automation-run>))
  (list (class-name (class-of self)) (name-get (genome-get self)) (seed-get self))
  )

(define-method (serialize-slots-get (self <ga-automation-run>))
  (append (next-method) '(-seed -genome))
  )

(define-method (on-start (self <ga-automation-run>))
  (slot-set! self '-time-start (srfi:current-time))
  )

(define-method (on-complete (self <ga-automation-run>) data)
  (slot-set! self '-time-end (srfi:current-time))
  ((slot-ref self '-callback-on-complete) self data)
  (slot-set! self '-callback-on-complete #nil)
  )

(define-method (duration-elapsed (self <ga-automation-run>))
  (assert (slot-ref self '-time-end))
  (srfi:time-difference (time-end-get self) (time-start-get self))
  )

;; Genomes

(define-class <ga-genome> (<serializable>)
  (-name-first #:init-form (generate-name (+ 1 (random 4)))
			   #:init-keyword #:name-first #:getter name-first-get #:setter name-first-set!)
  (-name-last #:init-form (generate-name (+ 1 (random 4)))
			  #:init-keyword #:name-last #:getter name-last-get #:setter name-last-set!)
  (-fitness #:init-value 0.0)
  (-is-elite #:init-value #f #:getter elite? #:setter is-elite-set!)
  (-subject #:init-keyword #:subject #:getter subject-get)
  (-parents #:init-value '() #:init-keyword #:parents #:getter parents-get)
  (-fitness-mutex #:init-thunk srfi:make-mutex)
  (-cache-mutation-factor #:init-value #nil)
  )

(define-method (inspect (self <ga-genome>))
  (formattpps
   "<ga-genome ~a ~a>"
   (name-get self)
   (subject-get self)
   )
  )

(define-method (serialize-slots-get (self <ga-genome>))
  (append (next-method) '(-name-first -name-last -fitness -is-elite -subject))
  )
  
(define-method (name-get (self <ga-genome>))
  (string-append (name-first-get self) " " (name-last-get self)))

(define-method (fitness-set! (self <ga-genome>) (fitness <number>))
  (srfi:mutex-lock! (slot-ref self '-fitness-mutex))
  (slot-set! self '-fitness fitness)
  (srfi:mutex-unlock! (slot-ref self '-fitness-mutex))
  )

(define-method (fitness-get (self <ga-genome>))
  (srfi:mutex-lock! (slot-ref self '-fitness-mutex))
  (let ((result (slot-ref self '-fitness)))
	(srfi:mutex-unlock! (slot-ref self '-fitness-mutex))
	result)
  )

(define-method (fitness-add! (self <ga-genome>) (fitness <number>))
  (srfi:mutex-lock! (slot-ref self '-fitness-mutex))
  (slot-set! self '-fitness (+ (slot-ref self '-fitness) fitness))
  (srfi:mutex-unlock! (slot-ref self '-fitness-mutex))
  )

(define-method (mutation-factor (genome <ga-genome>))
  (assert (not (null? (parents-get genome))))
  (let ((result (slot-ref genome '-cache-mutation-factor)))
  	(if result result
  		(let ((result (mutation-factor (car (parents-get genome))
  									   (cadr (parents-get genome))
  									   genome)))
  		  (slot-set! genome '-cache-mutation-factor result)
  		  result
  		  ))
  	)
  )

(define-method (cleanup! (genome <ga-genome>))
  (slot-set! genome '-parents 'cleaned)
  )

;; GA methods

(define-method (ga-combine (genome0 <ga-genome>) (genome1 <ga-genome>))
  (let ((result (make <ga-genome> #:subject (ga-combine (subject-get genome0) (subject-get genome1))
					  #:parents (list genome0 genome1))))
	;; Give the genome the last name of its best parent. If it was mutated, give it a new last name.
	(if (= 0 (mutation-factor result))
		(name-last-set! result (name-last-get genome0)))
	result
	)
  )

;; Factor: the higher the factor, the more likely that a bit will be flipped depending on its significance.
;; Chance decreases from lowest to highest bit with a power relationship.
;; 1.0 - ((i - 64) / 65) ** factor
(define-method (ga-mutate (double <number>) (factor <number>))
  (let ((bv (make-bytevector 8)))
	(bytevector-ieee-double-set! bv 0 (exact->inexact double) (endianness big))
	(let* ((mutated-bits
			(map (lambda (i bit)
				   (let ((mutate-chance (- 1.0 (expt (/ (- 64 i) 65) factor))))
					 (if (< (random 1.0) mutate-chance) (logxor bit 1) bit)))
				 (iota 64)
				 (bytevector->bitlist bv)))
		   (result (bytevector-ieee-double-ref (bitlist->bytevector mutated-bits) 0 (endianness big)))
		   )
	  (if (finite? result) result (ga-mutate double factor))
	  )
	)
  )


(define-method (ga-generate-next-population evaluated-genomes)
  (let* ((elites (ga-make-elites! evaluated-genomes ga-elite-preserve-count))
		 (evolved (ga-evolve-population evaluated-genomes ga-elite-preserve-count))
		 )
	(formatt "EVOLVED: ~a\n" evolved)
	(for-each (lambda (genome)
				(formatt "Bred ~a to produce ~a with mutation factor ~a\n"
						 (string-join (map name-get (parents-get genome)) " X ")
						 (name-get genome)
						 (mutation-factor genome)))
			  evolved)
	(format #t "ELITES: ~a\n" (string-join (map name-get elites) ", "))
	(append elites evolved)
	)
  )

;; Return the normalized amount of mutation compared to the parents.
;; A genome whose genes are entirely found in the parents should return a factor of 0.
(define-method (mutation-factor (p0 <ga-genome>) (p1 <ga-genome>) (c <ga-genome>))
  (mutation-factor (subject-get p0) (subject-get p1) (subject-get c))
  )

;; GA for Neural Networks

(define-method (mutation-factor (p0 <number>) (p1 <number>) (c <number>))
  (let ((d0 (bitlist-factor-diff (double->bitlist p0) (double->bitlist c)))
		(d1 (bitlist-factor-diff (double->bitlist p1) (double->bitlist c))))
	(cond ((or (= d0 0) (= d1 0)) 0)
		  ((and (= d0 0) (not (= d1 0))) d1)
		  ((and (not (= d0 0)) (= d1 0)) d0)
		  (else (avg (list d0 d1))))
	)
  )

(define-method (mutation-factor (p0 <nn-link>) (p1 <nn-link>) (c <nn-link>))
  (mutation-factor (slot-ref p0 '-weight) (slot-ref p1 '-weight) (slot-ref c '-weight)))

(define-method (mutation-factor (p0 <nn-node>) (p1 <nn-node>) (c <nn-node>))
  0.0)

(define-method (mutation-factor (p0 <nn-node-consumer>) (p1 <nn-node-consumer>) (c <nn-node-consumer>))
  (if (null? (links-in-get p0)) 0.0
	  (avg (map mutation-factor (links-in-get p0) (links-in-get p1) (links-in-get c))))
  )

(define-method (mutation-factor (p0 <nn-layer>) (p1 <nn-layer>) (c <nn-layer>))
  (avg (map mutation-factor (nodes-get p0) (nodes-get p1) (nodes-get c)))
  )

;;; Get a normalized value expressing the degree to which network c differs from either of its parents p0 and p1.
(define-method (mutation-factor (p0 <nn-network>) (p1 <nn-network>) (c <nn-network>))
  (avg (map mutation-factor (layers-all-get p0) (layers-all-get p1) (layers-all-get c)))
  )

(define-method (ga-combine (n0 <number>) (n1 <number>))
  (let* ((combined (if (<= (random 1.0) ga-combine-chance) n1 n0))
		 (chance (random 1.0))
		 (factor (cond ((<= chance ga-mutation-chance-minor) ga-mutation-factor-minor)
					   ((<= chance ga-mutation-chance-major) ga-mutation-factor-major)
					   (else 0.0)))
		 )
	(ga-mutate combined factor)
	)
  )

(define-method (ga-combine (link0 <nn-link>) (link1 <nn-link>))
  (assert (equal? (class-of link0) (class-of link1)))
  (assert (equal? (slot-ref link0 '-node-source-id) (slot-ref link1 '-node-source-id)))
  (make (class-of link0)
	#:node-source-id (slot-ref link0 '-node-source-id)
	#:weight (ga-combine (slot-ref link0 '-weight) (slot-ref link1 '-weight)))
  )

(define-method (ga-combine (node0 <nn-node>) (node1 <nn-node>))
  (assert (equal? (id-get node0) (id-get node1)))
  (assert (equal? (class-of node0) (class-of node1)))
  (make (class-of node0) #:id (id-get node0))
  )

(define-method (ga-combine (node0 <nn-node-consumer>) (node1 <nn-node-consumer>))
  (let ((node (next-method)))
	(slot-set! node '-links-in (map ga-combine (links-in-get node0) (links-in-get node1)))
	node
	)
  )

(define-method (ga-combine (layer0 <nn-layer>) (layer1 <nn-layer>))
  (assert (equal? (class-of layer0) (class-of layer1)))
  (assert (equal? (length (nodes-get layer0)) (length (nodes-get layer1))))
  (make (class-of layer0)
	#:nodes (map ga-combine (nodes-get layer0) (nodes-get layer1)))
  )

;;; The better network should be passed to network0
(define-method (ga-combine (network0 <nn-network>) (network1 <nn-network>))
  (assert (equal? (class-of network0) (class-of network1)))
  (make (class-of network0)
	#:layer-input (ga-combine (layer-input-get network0) (layer-input-get network1))
	#:layers-hidden (map ga-combine (layers-hidden-get network0) (layers-hidden-get network1))
	#:layer-output (ga-combine (layer-output-get network0) (layer-output-get network1))
	)
  )


(define (ga-by-best genomes)
  (sort genomes (lambda (a b) (> (fitness-get a) (fitness-get b)))))

;; GENOMES must be sorted by fitness
(define (ga-select-parents-fitness-proportionate genomes ndesired)
  (let ((max-best (apply max (map fitness-get genomes))))
	(let f ((out '()) (ln genomes))
	  (cond ((= (length out) ndesired) (reverse out))
			((null? ln) (list-head genomes ndesired))
			((<= (random 1.0)
				 (* ga-factor-parent-best (if (= 0 max-best) 1.0 (/ (fitness-get (car ln)) max-best))))
			 (f (cons (car ln) out) (cdr ln)))
			(else (f out (cdr ln))))
	  )
	)
  )

;; GENOMES must be sorted by fitness
(define (ga-select-parents-tournament genomes ndesired)
  (assert (<= ndesired (length genomes)))

  (let f ((out '()) (remaining genomes))
	(cond
	 ((= (length out) ndesired) (reverse out))
	 ((= (length remaining) 1) (reverse (cons remaining out)))
	 (else
	  (let* ((p0 (list-ref remaining (random (length remaining))))
			 (p1 (list-ref (delete p0 remaining) (random (1- (length remaining))))))
		(let ((chosen (if (> (fitness-get p0) (fitness-get p1)) p0 p1)))
		  (f (cons chosen out) (delete chosen remaining))
		  )
		)
	  )
	 )
	)
  )

;; Notes: number of genomes may not equal number of runs due to skipping eval of elites.
(define (ga-calc-generation-stats generation genomes runs stats)
  (let ((best (ga-by-best genomes)))
	(let ((f (open-file "best.txt" "a")))
	  (format f
			  "GENERATION ~a:\n~a\n\n~a\n\n"
			  generation
			  (pretty-string (serialize (car best)))
			  (pretty-string (serialize (cadr best))))
	  (close f)
	  )
	(formatt "Final 25 fitnesses: ~a\n"
			 (list-head (map (lambda (nn) (cons (name-get nn) (fitness-get nn))) best) (min (length best) 25))
			 )
	(let ((no-elites (map fitness-get (filter (negate elite?) best))))
;;; "generation,max-fitness-with-elites,max-fitness-no-elites,min-fitness-no-elites,average,duration-real,duration-agg,duration-run-min,duration-run-max,duration-run-avg,heap-size\n"
	  (list generation
			(apply max (map fitness-get best))
			(apply max no-elites)
			(apply min no-elites)
			(avg (map fitness-get best))
			(srfi-time->seconds (srfi:time-difference
								 (car (sort (map time-end-get runs) srfi:time>?))
								 (car (sort (map time-start-get runs) srfi:time<=?))))
			(srfi-time->seconds (reduce srfi:add-duration (duration-elapsed (car runs)) (map duration-elapsed runs)))
			(apply min (map srfi-time->seconds (map duration-elapsed runs)))
			(apply max (map srfi-time->seconds (map duration-elapsed runs)))
			(avg (map srfi-time->seconds (map duration-elapsed runs)))
			(/ (assoc-ref (gc-stats) 'heap-size) 1048576.0)
			)
	  )
	)
  )

;; Returns descendants for the next generation, generated by evolution and mutation.
;;
;; Note: returns (- length-genomes length-elites) descendants as elites will subsequently be placed in the next
;; generation unmodified.
(define (ga-evolve-population evaluated-genomes num-elites)
  (let ((best (ga-by-best evaluated-genomes)))
	(assert (>= (length best) 2))
	(n-par-map 9
			   (lambda (_) (apply ga-combine (ga-select-parents best 2)))
			   (iota (- (length best) num-elites))))
  )

;; Set 'elite' flag on those genomes that should be considered elite according to their current fitnesses.
;; Note: clears 'elite' flag on all then sets flag on the returned genomes
;; Returns list of elites
(define (ga-make-elites! evaluated-genomes count)
  (for-each (cut is-elite-set! <> #f) evaluated-genomes)
  
  (let ((best (ga-by-best evaluated-genomes)))
	(assert (<= count (length best)))
	(let ((result (list-head best count)))
	  (for-each (cut is-elite-set! <> #t) result)
	  result
	  )
	)
  )

;; Generation evaluation
(define (ga-on-evaluated run steps fitness-func)
  (fitness-set! (genome-get run) (fitness-func steps ga-max-moves))
  (broadcast-condition-variable ga-cond-complete)
  (if ga-profile (begin (statprof-stop) (statprof-display)))
  )

;; Run a single genome so that fitness can be evaluated
(define-method (ga-evaluate (run <ga-automation-run>) steps generation-start-func game-won-func
							game-step-func fitness-func)
  (if (= steps 0)
	  (begin
		(if ga-profile (statprof-start))
		(generation-start-func (seed-get run))
		))
  (let ((genome (genome-get run)))
	(assert (not (elite? genome))) ; elite fitness should already be known
	(cond
	 ((game-won-func)
	  (format #t "~a: Game won\n" (inspect run))
	  (ga-on-evaluated run steps fitness-func)
	  )
	 ((= ga-max-moves steps)
	  (format #t "~a: Remaining steps expired\n" (inspect run))
	  (ga-on-evaluated run steps fitness-func)
	  )
	 (else
	  (game-step-func genome)
	  (ga-evaluate run (1+ steps) generation-start-func game-won-func game-step-func fitness-func)
	  )
	 )
	)
  )

(define spawned #f)

(define (thread-pre-unwind-handler key . args)
  (formatt "Thread terminated, error: ~a ~a\n~a\n"
		   key
		   args
		   (call-with-output-string (cut display-backtrace (make-stack #t 5) <>)))
  )

;(set! %thread-pre-unwind-handler thread-pre-unwind-handler)

(define (guarded-thread thunk)
  (srfi:make-thread
   (lambda ()
	 (catch #t
			thunk
			identity
			thread-pre-unwind-handler)
	 ))
  )

(define (on-complete-run run result)
  (formatt "Completed ~a with result ~a (~a remaining)\n"
		   (inspect run)
		   result
		   (1- (apply + (lengths queue-runs)))
		   )
  (fitness-add! (genome-get run) result)
  )

;; Begin repeatedly evaluting and evolving the given state
;; LAMBDA-GA-STATE-INITIAL should return a ga-state object to be used if no state is serialized.
(define (ga-evolve lambda-ga-state-initial)
  (let loop ((state (or (and (not ga-test) (ga-load (the-environment)))
						(lambda-ga-state-initial)))
			 (first #t)
			 )
	(unless spawned (set! spawned #t)
			(aisleriot-server ga-n-child-spawns (the-environment)))

	(let* ((best
			(if (first? state) '() (ga-by-best (genomes-get state))))
		   (non-elite
			(filter (negate elite?) (genomes-get state)))
		   (runs
			(apply
			 append
			 (n-par-map 9
						(lambda (genome)
						  (let ((genome-new (if (null? best) genome (apply ga-combine (ga-select-parents best 2)))))
							(unless (null? (parents-get genome-new))
									(formatt "Bred ~a to produce ~a with mutation factor ~a\n"
											 (string-join (map name-get (parents-get genome-new)) " X ")
											 (name-get genome-new)
											 (mutation-factor genome-new)))
							
							(map
							 (lambda (seed)
							   (queue! queue-runs
									   (prepare-automation-run
										(make <ga-automation-run>
										  #:genome genome-new
										  #:seed seed
										  #:on-complete on-complete-run)
										))
							   )
							 (seeds-get state))
							)
						  )
						non-elite)
			 )
			))

	  (unless (first? state) (for-each cleanup! (genomes-get state)))
	  (formatt "Waiting to finish processing ~a runs\n" (apply + (lengths queue-runs)))
	  (let ((thread-write-state
			 (begin-thread
			  (unless (or ga-test first)
					  (formatt "Serializing state\n")
					  (call-with-output-file "state.scm.out" (cut serialize-stream <> state))
					  (rename-file "state.scm.out" "state.scm")
					  (formatt "Serializing state done\n")
					  )
			  )))
		(drain queue-runs)
		(formatt "Completed ~a runs\n" (length runs))
		
		(let* ((genomes-new (append (filter elite? (genomes-get state)) (delete-duplicates (map genome-get runs) eq?)))
			   (stats-new
				(append (stats-get state)
						(list (ga-calc-generation-stats (n-generation-get state) genomes-new runs (stats-get state)))))
			   )
		  
		  (begin-thread
		   (call-with-output-file
			   "stats.txt"
			 (lambda (file)
			   (for-each
				(lambda (port)
				  ;; Write config that was used prepended with comment character
				  (format port (string-join (map (cut string-append "#" <>)
												 (string-split (pretty-string ga-config) #\newline)) "\n"))
				  (format port "\n")
				  (format port "generation,max-fitness-with-elites,max-fitness-no-elites,min-fitness-no-elites,average,duration-real,duration-agg,duration-run-min,duration-run-max,duration-run-avg,heap-size\n")
				  (format port "~a\n" (string-join (map list->csv stats-new) "\n"))
				  )
				(list #t file)
				)
			   ))
		   )
		  
		  (ga-make-elites! genomes-new ga-elite-preserve-count)
		  (formattpp "~a\n" (gc-stats))
		  (join-thread thread-write-state)
		  (loop (next state genomes-new stats-new) #f)
		  )
		)
	  )
	)
  )


(define cmd "prefix/bin/sol") ; use argv[0] instead?

(define (ga-is-automation)
  (and (member "--automate" (program-arguments)) (member "--host" (program-arguments))))

(define (bind-with-free-port socket port)
  (assert (<= port 65536))
  (assert (> port 0))
  (if (= port 65536)
	  #f
	  (catch 'system-error
			 (lambda ()
			   (bind socket AF_INET INADDR_ANY port)
			   port
			   )
			 (lambda (key . args) (bind-with-free-port socket (1+ port)))
	  ))
  )

(define (child-spawn port i)
  (formatt "Spawning child ~a\n" i)
  (let ((pipe (open-pipe* OPEN_READ cmd "--automate" "--host" (format #f "127.0.0.1:~a"  port))))
	(formatt "Opened pipe ~a\n" i)
	(setvbuf pipe 'line)
	(let f ()
	  (let ((t (get-line pipe)))
		(formatt "CHILD ~a: ~a\n" i t)
		(unless (eof-object? t) (f))
		)
	  )
	)
  (formatt "Child ~a died\n" i)
  )

(define (server-worker-connection sock local-env)
  (let ((run (wait-next! queue-runs)))
	(on-start run)
  
	(catch
	 #t
	 (lambda ()
;	   (formatt "~a: serializing\n" (inspect run))
	   (let* ((to-send (serialized-get! run)))
;		 (formatt "~a: sending to client\n" (inspect run))
		 ;; Send data length then data instead of triggering data end with a socket write shutdown, to avoid TIME_WAIT
		 ;; on server.
		 (format sock "~a\n~a" (string-length to-send) to-send)
		 (force-output sock)
		 )
;	   (formatt "~a: waiting for client to complete\n" (inspect run))

	   (let keepalive ()
		 (unless (equal? (car (select (list sock) '() '() 10)) (list sock))
				 (throw 'ga-error "no client data received before timeout"))

		 (let ((in-size (get-line sock)))
		   (unless (eof-object? in-size)
				   (let ((result (get-string-n sock (string->number in-size))))
					 (let ((deserialized (deserialize-from-string result local-env)))
					   (cond ((number? deserialized)
							  (on-complete run deserialized)
							  (complete! queue-runs run)
							  )
							 ;; Receipt of #t is a keepalive signal
							 ((boolean? deserialized) (formatt "~a: keepalive\n" (inspect run)) (keepalive))
							 (else (throw 'ga-error "Expected number or bool, got" deserialized)))
					   )
					 )
				   )
		   )
		 )
	   )
	 (lambda (key . args)
	   (formatt "~a: Error, retrying run ~a ~a\n" (inspect run) key args)
	   (requeue! queue-runs (prepare-automation-run run))
	   (close sock)
	   )
	 )
	)

  (server-worker-connection sock local-env)
  )

(define (aisleriot-server nchildren local-env)
  (let* ((sock-listen (socket PF_INET SOCK_STREAM 0))
		 (port ga-port))
	
	(let f ((i 0))
	  (catch 'system-error
			 (cut bind sock-listen AF_INET INADDR_ANY port)
			 (lambda (tag . args)
			   (formatt "Retrying server start: ~a ~a\n" tag args)
			   (if (= i 60) (apply throw tag args))
			   (sleep 2)
			   (f (1+ i))))
	  )
	
	(formatt "Bound to port ~a\n" port)
	(listen sock-listen 128)

	(call-with-output-file "best.txt" identity)

	(for-each (lambda (i) (srfi:thread-start! (guarded-thread (lambda () (child-spawn port i))))) (iota nchildren))

	(srfi:thread-start!
	 (guarded-thread
	  (lambda () 
		(let f ()
		  (let ((sock-worker (car (accept sock-listen))))
			(srfi:thread-start! (guarded-thread (lambda () (server-worker-connection sock-worker local-env))))
			)
		  (f)
		  )
		(close sock-listen)
		)))
	)
  )

;; Starts a thread that processes automation runs forever.
(define (ga-client-receive-automation-runs local-env run-func complete-func error-func)
  (srfi:thread-start!
   (guarded-thread
	(lambda ()
	  (let ((host-param (member "--host" (program-arguments))))
		(if (not host-param)
			(error "No host given")
			(let* ((split-param (string-split (cadr host-param) #\:))
				   (host (car split-param))
				   (port (string->number (cadr split-param))))
			  (let ((sock (socket PF_INET SOCK_STREAM 0)))
				(let retry-connect ()
				  (catch #t
						 (lambda ()
						   (formatt "Waiting to get automation data from port ~a\n" port)
						   (connect sock (addrinfo:addr (car (getaddrinfo host (object->string port) AI_NUMERICSERV))))
						   )
						 (lambda (key . args) (formatt "~a ~a\n" key args) (sleep 5) (retry-connect)))
				  )
				(catch
				 #t
				 (lambda ()
				   (let receive-next-run ()
					 (format #t "Client waiting to receive\n")
					 (select (list sock) '() '())
					 (format #t "Client receiving\n")
					 (let* ((input (get-string-n sock (string->number (get-line sock))))
							(_ (format #t "Client deserializing\n"))
							(automation-run (deserialize-from-string input local-env)))
					   (fitness-set! (genome-get automation-run) 0.0)
					   (format #t "~a: Running\n" (inspect automation-run))

					   (letrec ((keepalive
								 (guarded-thread
								  (lambda ()
									(let f ()
									  (lock-mutex ga-mutex-complete)
									  (unless (wait-condition-variable ga-cond-complete ga-mutex-complete
																	   (+ (current-time) 5))
											  (unlock-mutex ga-mutex-complete)
											  (let ((to-send (format #f "~s" #t)))
												(format sock "~a\n~a" (string-length to-send) to-send)
												)
											  (unless (srfi:thread-specific keepalive)
													  (f)
													  )
											  )
									  )
									(unlock-mutex ga-mutex-complete)
									)
								  ))
								)
						 
						 (srfi:thread-start! keepalive)
						 (run-func automation-run)
						 (lock-mutex ga-mutex-complete)
						 (wait-condition-variable ga-cond-complete ga-mutex-complete)
						 (srfi:thread-specific-set! keepalive #t)
						 (unlock-mutex ga-mutex-complete)
						 (signal-condition-variable ga-cond-complete)
						 (srfi:thread-join! keepalive)
						 )
					   
					   (let ((to-send (format #f "~s" (fitness-get (genome-get automation-run)))))
						 (format sock "~a\n~a" (string-length to-send) to-send)
						 )
					   
					   (complete-func)
					   (receive-next-run)
					   )
					 )
				   )
				 (lambda (key . args)
				   (formatt "Error: ~a ~a\n" key args)
				   (close sock)
				   (error-func)
				   (ga-client-receive-automation-runs local-env run-func complete-func error-func)))
				)
			  )
			))
	  )
	)
   )
  )

(define (ga-load local-env)
  (catch 'system-error
		 (lambda ()
		   (formatt "Restoring state.scm\n")
		   (call-with-input-file "state.scm" (cut deserialize-stream-from-port <> local-env)))
		 (lambda (tag . args) (formatt "ga-load: ~a ~a\n" tag args) #nil)
		 )
  )

;;; GA setup
(define ga-test #t)
(define ga-profile #f)

(define ga-elite-preserve-count 2)

(define ga-n-child-spawns (if ga-test 3 0))

(define ga-population-size (if ga-test 6 1000))
(define ga-training-set-size (if ga-test 1 1))

(define ga-combine-chance 0.5)
(define ga-mutation-chance-minor 0.6)
(define ga-mutation-factor-minor 0.5)
(define ga-mutation-chance-major 0.25)
(define ga-mutation-factor-major 2.0)
(define ga-max-moves 100)


;; The degree to which genomes with better scores are favoured for breeding (proportionate only)
(define ga-factor-parent-best 0.5)

(define ga-select-parents ga-select-parents-fitness-proportionate)
;;(define ga-select-parents ga-select-parents-tournament)

(define ga-port (if ga-test 12346 12345))

;; Config variables that are useful for recording per-run
(define ga-config
  `(
   (ga-test . ,ga-test)
   (ga-population-size . ,ga-population-size)
   (ga-training-set-size . ,ga-training-set-size)
   (ga-combine-chance . ,ga-combine-chance)
   (ga-mutation-chance-minor . ,ga-mutation-chance-minor)
   (ga-mutation-factor-minor . ,ga-mutation-factor-minor)
   (ga-mutation-chance-major . ,ga-mutation-chance-major)
   (ga-mutation-factor-major . ,ga-mutation-factor-major)
   (ga-max-moves . ,ga-max-moves)
   (ga-elite-preserve-count . ,ga-elite-preserve-count)
   (ga-factor-parent-best . ,ga-factor-parent-best)
   (ga-select-parents . ,ga-select-parents)
   )
  )

(assert (< ga-elite-preserve-count ga-population-size))
(assert (>= ga-population-size 2))
