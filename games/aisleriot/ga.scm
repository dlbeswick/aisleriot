(define-module (aisleriot ga)
  #:use-module (ice-9 futures)
  #:use-module (ice-9 local-eval)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (rnrs base)
  #:use-module (aisleriot formatt)
  #:use-module (aisleriot serialize)
  #:use-module (aisleriot nn)
  #:use-module (aisleriot interface)
  #:re-export (serialize)
  #:export (<ga-automation-run>
			<ga-genome>

			genome-get
			name-get
			seed-get
			subject-get
			
			ga-population-size
			ga-training-set-size
			ga-evolve
			
			aisleriot-spawn
			fitness-eval
			ga-automate
			ga-automation-run
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

(define ga-mutex-complete (make-mutex))
(define ga-cond-complete (make-condition-variable))
(define ga-futures '())

(define (avg list)
  (exact->inexact (/ (apply + list) (length list)))
  )

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

(define-class <ga-genome> (<serializable>)
  (-name-first #:init-form (generate-name (+ 1 (random 4)))
			   #:init-keyword #:name-first #:getter name-first-get #:setter name-first-set!)
  (-name-last #:init-form (generate-name (+ 1 (random 4)))
			  #:init-keyword #:name-last #:getter name-last-get #:setter name-last-set!)
  (-fitness #:init-value 0.0)
  (-is-elite #:init-value #f #:getter is-elite #:setter is-elite-set!)
  (-subject #:init-keyword #:subject #:getter subject-get)
  (-fitness-mutex #:init-thunk make-mutex)
  )

(define-method (inspect (self <ga-genome>))
  (formattpps
   "<ga-genome ~a ~a>"
   (name-get self)
   (subject-get self)
   )
  )

(define-method (serialize (self <ga-genome>))
  (append (next-method)
		  (serialize-slots self
						   '-name-first
						   '-name-last
						   '-fitness
						   '-is-elite
						   '-subject
						   ))
  )
  
(define-method (name-get (self <ga-genome>))
  (string-append (name-first-get self) " " (name-last-get self)))

(define-method (fitness-set! (self <ga-genome>) (fitness <number>))
  (with-mutex (slot-ref self '-fitness-mutex)
			  (slot-set! self '-fitness fitness)
			  )
  )

(define-method (fitness-get (self <ga-genome>))
  (with-mutex (slot-ref self '-fitness-mutex)
			  (slot-ref self '-fitness)
			  )
  )

(define-method (fitness-add! (self <ga-genome>) (fitness <number>))
  (with-mutex (slot-ref self '-fitness-mutex)
			  (slot-set! self '-fitness (+ (slot-ref self '-fitness) fitness))
			  )
  )

;; GA methods

(define-method (ga-combine (genome0 <ga-genome>) (genome1 <ga-genome>))
  (let ((subject-new (ga-combine (subject-get genome0) (subject-get genome1))))

	(let* ((result (make <ga-genome> #:subject subject-new))
		   (mfactor (mutation-factor genome0 genome1 result))
		   )
	  ;; Give the genome the last name of its best parent. If it was mutated, give it a new last name.
	  (if (= 0 mfactor)
		  (name-last-set! result (name-last-get genome0)))
	  (format #t "Bred ~a X ~a to produce ~a with mutation factor ~a\n"
			  (name-get genome0) (name-get genome1) (name-get result) mfactor)
	  result
	  )
	)
  )

(define-method (ga-mutate (self <number>))
  (* (+ -1.0 (random 2.0)) ga-value-max)
  )

(define-method (ga-generate-next-population evaluated-genomes)
  (let* ((elites (if (> ga-elite-preserve-count 0)
						(ga-make-elites! evaluated-genomes ga-elite-preserve-count)
						'()))
		 (evolved (ga-evolve-population evaluated-genomes))
		 )
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
  (if (or (= p0 c) (= p1 c)) 0.0 1.0))

(define-method (mutation-factor (p0 <nn-link>) (p1 <nn-link>) (c <nn-link>))
  (mutation-factor (slot-ref p0 '-weight) (slot-ref p1 '-weight) (slot-ref c '-weight)))

(define-method (mutation-factor (p0 <nn-node>) (p1 <nn-node>) (c <nn-node>))
  0.0)

(define-method (mutation-factor (p0 <nn-node-consumer>) (p1 <nn-node-consumer>) (c <nn-node-consumer>))
  (if (null? (links-in-get p0)) 0.0
	  (/ (fold (lambda (ep0 ep1 ec acc) (+ acc (mutation-factor ep0 ep1 ec)))
			   0.0
			   (links-in-get p0)
			   (links-in-get p1)
			   (links-in-get c))
		 (length (links-in-get p0)))))

(define-method (mutation-factor (p0 <nn-layer>) (p1 <nn-layer>) (c <nn-layer>))
  (/ (fold (lambda (ep0 ep1 ec acc) (+ acc (mutation-factor ep0 ep1 ec)))
		   0.0
		   (nodes-get p0)
		   (nodes-get p1)
		   (nodes-get c))
	 (length (nodes-get p0))))

;;; Get a normalized value describing the degree to which network c differs from either of its parents p0 and p1.
(define-method (mutation-factor (p0 <nn-network>) (p1 <nn-network>) (c <nn-network>))
  (/ (fold (lambda (ep0 ep1 ec acc) (+ acc (mutation-factor ep0 ep1 ec)))
		   0.0
		   (layers-all-get p0)
		   (layers-all-get p1)
		   (layers-all-get c))
	 (length (layers-all-get p0))))

(define-method (ga-combine (n0 <number>) (n1 <number>))
  (if (<= (random 1.0) ga-mutation-chance) (ga-mutate n0)
	  (if (<= (random 1.0) ga-combine-chance) n1 n0)))

(define-method (ga-combine (link0 <nn-link>) (link1 <nn-link>))
  (assert (equal? (class-of link0) (class-of link1)))
  (assert (equal? (slot-ref link0 '-node-source-id) (slot-ref link1 '-node-source-id)))
  (assert (equal? (slot-ref link0 '-node-dest-id) (slot-ref link1 '-node-dest-id)))
  (make (class-of link0)
	#:node-source-id (slot-ref link0 '-node-source-id)
	#:node-dest-id (slot-ref link0 '-node-dest-id)
	#:weight (ga-combine (slot-ref link0 '-weight) (slot-ref link1 '-weight)))
  )

(define-method (ga-combine (node0 <nn-node>) (node1 <nn-node>))
  (assert (equal? (id-get node0) (id-get node1)))
  (assert (equal? (class-of node0) (class-of node1)))
  (make (class-of node0)
	#:id (id-get node0))
  )

(define-method (ga-combine (node0 <nn-node-consumer>) (node1 <nn-node-consumer>))
  (let ((node (next-method)))
	(slot-set! node '-links-in (reverse (fold (lambda (a b r) (cons (ga-combine a b) r)) '() (links-in-get node0) (links-in-get node1))))
	node))

(define-method (ga-combine (layer0 <nn-layer>) (layer1 <nn-layer>))
  (assert (equal? (class-of layer0) (class-of layer1)))
  (assert (equal? (length (nodes-get layer0)) (length (nodes-get layer1))))
  (make (class-of layer0)
	#:nodes (reverse (fold (lambda (a b r) (cons (ga-combine a b) r)) '() (nodes-get layer0) (nodes-get layer1))))
  )

;;; The better network should be passed to network0
(define-method (ga-combine (network0 <nn-network>) (network1 <nn-network>))
  (assert (equal? (class-of network0) (class-of network1)))
  (make (class-of network0)
	#:layer-input (ga-combine (layer-input-get network0) (layer-input-get network1))
	#:layers-hidden (reverse (fold (lambda (a b r) (cons (ga-combine a b) r)) '() (layers-hidden-get network0) (layers-hidden-get network1)))
	#:layer-output (ga-combine (layer-output-get network0) (layer-output-get network1))
	)
  )


(define (ga-by-best genomes)
  (sort genomes (lambda (a b) (> (fitness-get a) (fitness-get b)))))

(define (ga-select-parents-fitness-proportionate genomes ndesired)
  (let ((max-best (apply max (map fitness-get (ga-by-best genomes)))))
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

(define (ga-select-parents-tournament genomes ndesired)
  (assert (< ndesired (1- (length genomes))))
  
  (let f ((out '()) (remaining genomes))
	(if (= (length out) ndesired)
		(reverse out)
		(let* ((p0 (list-ref remaining (random (length remaining))))
			   (p1 (list-ref (delete p0 remaining) (random (1- (length remaining))))))
		  (let ((chosen (if (> (fitness-get p0) (fitness-get p1)) p0 p1)))
			(f (cons chosen out) (delete chosen remaining))
			)
		  )
		)
	)
  )

(define (ga-calc-generation-stats generation evaluated-genomes stats)
  (let ((best (ga-by-best evaluated-genomes)))
	(formattpp "INSPECTION OF BEST:\n~a\n\n~a\n\n" (serialize (car best)) (serialize (cadr best)))
	(formatt "EVOLVE POPULATION:\n Final 25 fitnesses: ~a\n"
			 (list-head (map (lambda (nn) (cons (name-get nn) (fitness-get nn))) best) (min (length best) 25))
			 )
	(let ((no-elites (map fitness-get (filter (negate is-elite) best))))
	  (list generation
			(apply max (map fitness-get best))
			(apply max no-elites)
			(apply min no-elites)
			(avg no-elites)
			)
	  )
	)
  )

(define-method (ga-evolve-population evaluated-genomes)
  ;; Note: don't evolve elites
  (let* ((best (ga-by-best evaluated-genomes))
		 (no-elites (filter (lambda (nn) (not (is-elite nn))) best)))
	(assert (>= (length best) 2))
	(map (lambda (n) (apply ga-combine (ga-select-parents best 2))) no-elites)))

;; Note: clears 'elite' flag then sets flag on the returned genomes
;; Returns list of elites
(define-method (ga-make-elites! evaluated-genomes count)
  (for-each (lambda (nn) (is-elite-set! nn #f)) evaluated-genomes)
  
  (let ((best (ga-by-best evaluated-genomes)))
	(assert (>= (length best) count))
	(let ((result (list-head best count)))
	  (for-each (lambda (nn) (is-elite-set! nn #t)) result)
	  result
	  )
	)
  )

;; Generation evaluation
(define-method (fitness-eval subject game steps steps-max)
  (error "Unimplemented" subject)
  )

(define (evaluate-next game evaluated-genomes pending-genomes evaluated-seeds pending-seeds steps generation stats
					   generation-start-func game-won-func game-step-func)
  (let* ((genome (car pending-genomes))
		 (seed (car pending-seeds))
		 (fitness-seed (fitness-eval (subject-get genome) game steps ga-max-moves)))

	(if (ga-is-automation)
		(if (is-elite genome)
			(if (null? evaluated-seeds) (format #t "Elite genome ~a: fitness already known\n" (name-get genome)))
			(begin
			  (fitness-add! genome fitness-seed)
			  (format #t "GENERATION: ~a game seed: ~a candidates remaining: ~a tested: '~a' had fitness ~a for total ~a\n"
					  generation seed (length pending-genomes) (name-get genome) fitness-seed (fitness-get genome))
			  
			  )
			)
		)

	(if (ga-is-automation)
		(signal-condition-variable ga-cond-complete)
	
		(if (null? (cdr pending-seeds))
			(if (null? (cdr pending-genomes))
				(begin
				  (for-each touch ga-futures)
				  
				  (let ((stats-new
						 (cons (ga-calc-generation-stats generation (cons genome evaluated-genomes) stats) stats))
						(next-generation
						 (sort (ga-generate-next-population (cons genome evaluated-genomes))
							   (lambda (a b) (string<? (name-last-get a) (name-last-get b)))))
						)
					(format #t "Stats history:\n")
					(format #t "generation,max-fitness-with-elites,max-fitness-no-elites,min-fitness-no-elites,average\n")
					(format #t "~a\n" (string-join (map list->csv (reverse stats-new)) "\n"))
					(ga-evolve game
							   '()
							   next-generation
							   '()
							   (reverse (cons seed evaluated-seeds))
							   0
							   (1+ generation)
							   stats-new
							   generation-start-func
							   game-won-func
							   game-step-func)
					)
				  )
				(ga-evolve game (cons genome evaluated-genomes) (cdr pending-genomes) '()
						   (reverse (cons seed evaluated-seeds)) 0 generation stats generation-start-func game-won-func
						   game-step-func))
			(ga-evolve game evaluated-genomes pending-genomes (cons seed evaluated-seeds) (cdr pending-seeds) 0
					   generation stats generation-start-func game-won-func game-step-func))))
  )

(define (ga-evolve game evaluated-genomes pending-genomes evaluated-seeds pending-seeds steps generation stats
				   generation-start-func game-won-func game-step-func)
  (if (= steps 0) (generation-start-func (car pending-seeds)))
  (let ((genome (car pending-genomes)))
	(cond
	 ((is-elite genome)
	  (evaluate-next game evaluated-genomes pending-genomes evaluated-seeds pending-seeds steps generation stats
					 generation-start-func game-won-func game-step-func)
	  )
	 ((game-won-func)
	  (display "Won!\n")
	  (format #t "\nGENOME:\n~a\n\n" (inspect genome))
	  (evaluate-next game evaluated-genomes pending-genomes evaluated-seeds pending-seeds steps generation stats
					 generation-start-func game-won-func game-step-func)
	  )
	 ((= ga-max-moves steps)
	  (display "Remaining steps expired\n")
	  (evaluate-next game evaluated-genomes pending-genomes evaluated-seeds pending-seeds steps generation stats
					 generation-start-func game-won-func game-step-func)
	  )
	 (else
	  (if (ga-is-automation)
		  (begin
			(game-step-func genome game)
			(idle-call
			 (lambda () (ga-evolve game evaluated-genomes pending-genomes evaluated-seeds pending-seeds
								   (1+ steps) generation stats generation-start-func game-won-func game-step-func))))
		  (begin
			(set!
			 ga-futures
			 (cons
			  (make-future (lambda ()
							 (fitness-add!
							  genome
							  (aisleriot-spawn
							   (make <ga-automation-run> #:seed (car pending-seeds) #:genome genome)
							   (the-environment)))
							 ))
			  ga-futures)
			 )
			
			(evaluate-next game evaluated-genomes pending-genomes evaluated-seeds pending-seeds steps generation stats
					 generation-start-func game-won-func game-step-func))
		  )
	  )
	 )
	)
  )


;; Distributed execution

(define-class <ga-automation-run> (<serializable>)
  (-seed #:init-keyword #:seed #:getter seed-get)
  (-genome #:init-keyword #:genome #:getter genome-get)
  )

(define-method (serialize (self <ga-automation-run>))
  (append (next-method) (serialize-slots self '-seed '-genome)))

;(define cmdline "XDG_DATA_DIRS="/home/david/devel/aisleriot/build/prefix/share:$XDG_DATA_DIRS" AR_CARD_THEME_PATH_SVG=cards AR_DEBUG=all GUILE_LOAD_COMPILED_PATH="/home/david/devel/aisleriot/build/prefix/lib/aisleriot/guile/2.0" prefix/bin/sol")
(define cmd "prefix/bin/sol") ; use argv[0] instead?

(define (ga-is-automation)
  (member "--automate" (program-arguments)))

(define (bind-with-free-port socket port)
  (assert (<= port 65536))
  (assert (> port 0))
  (if (= port 65536)
	  #f
	  (catch 'system-error
			 (lambda ()
			   (bind socket AF_INET INADDR_LOOPBACK port)
			   port
			   )
			 (lambda (key . args) (bind-with-free-port socket (1+ port)))
	  ))
  )

(define (aisleriot-spawn input local-env)
  (let* ((sock-listen (socket PF_INET SOCK_STREAM 0))
		 (port (bind-with-free-port sock-listen 12345)))
	(unless port (error "Couldn't bind to a port"))
	(formatt "Bound to port ~a\n" port)
	(listen sock-listen 1)
	(begin-thread
	 (formatt "Spawning child\n")
	 (let ((pipe (open-pipe* OPEN_READ cmd "--automate" "--port" (object->string port))))
	   (while
		#t
		(let ((t (read-line pipe 'split)))
		  (formatt "CHILD ~a: ~a\n" port (car t))
		  (if (eof-object? (cdr t)) (break)))
	   )
	 )
	 (formatt "Child ~a died\n" port)
	 )

	(formatt "Waiting for connection\n")
	(let* ((sock-io (car (accept sock-listen)))
		   (serialized (serialize input)))
	  (formatt "Connection accepted. Sending to port ~a\n" port)
	  
	  (display "(quote" sock-io)
	  (pretty-print serialized sock-io)
	  (display ")\n" sock-io)
	  
	  (force-output sock-io)
	  (shutdown sock-io 1)
	  
	  (formatt "Reading from port ~a\n" port)
	  (let ((result (get-string-all sock-io)))
		(formatt "From child ~a server got: ~a\n" port result)
		(close sock-io)
		(close sock-listen)
		(deserialize-from-string result local-env)))
	)
  )

;; Returns ga-automation-run
(define (ga-automation-run local-env run-func)
  (formatt "ga-automation-run-get\n")
  (let ((port-param (member "--port" (program-arguments))))
	(if (not port-param)
		(error "No port given")
		(let ((port (string->number (cadr port-param))))
		  (formatt "Getting automation data from port ~a\n" port)
		  (let ((sock (socket PF_INET SOCK_STREAM 0)))
			(connect sock AF_INET INADDR_LOOPBACK port)
			(let* ((read (get-string-all sock))
				   (result (deserialize-from-string read local-env)))
;;;			  (format (current-error-port) "Client got: ~a\n" read)
;;;			  (format (current-error-port) "Client got: ~a\n" (inspect (subject-get (genome-get result))))
;;;			  (format (current-error-port) "Client got: ~a\n" (nodes-get (layer-input-get (subject-get (genome-get result)))))
			  (shutdown sock 0)
			  (fitness-set! (genome-get result) 0.0)
			  (run-func result)
			  (begin-thread
			   (with-mutex ga-mutex-complete
						   (wait-condition-variable ga-cond-complete ga-mutex-complete))
			   (display (fitness-get (genome-get result)) sock)
			   (close sock)
			   (system* "/bin/kill" (object->string (getpid)))
			  )
			)
		  )
		))
	)
  )

;;; GA setup
(define ga-test #f)

(define ga-population-size 24)
(define ga-training-set-size 10)

(define ga-combine-chance 0.5)
(define ga-mutation-chance 0.01)
(define ga-max-moves 100)

;; The degree to which genomes with better scores are favoured for breeding
(define ga-factor-parent-best 0.5)
(define ga-elite-preserve-count 2)

(define ga-value-max 3.402823466e+38)

;(define ga-select-parents ga-select-parents-fitness-proportionate)
(define ga-select-parents ga-select-parents-tournament)

(if ga-test
	(begin
	  (set! ga-population-size 3)
	  (set! ga-training-set-size 1)))

(assert (< ga-elite-preserve-count ga-population-size))
(assert (>= ga-population-size 2))
