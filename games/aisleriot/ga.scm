(define-module (aisleriot ga)
  #:use-module (ice-9 atomic)
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
  #:use-module (aisleriot permute)
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
			
			fitness-eval
			ga-evaluate
			ga-client-receive-automation-run
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

;; Distributed execution

(define-class <ga-automation-run> (<serializable>)
  (-seed #:init-keyword #:seed #:getter seed-get)
  (-genome #:init-keyword #:genome #:getter genome-get)
  (-state #:init-form (make-atomic-box 'ready))
  )

(define-method (state-get (self <ga-automation-run>))
  (atomic-box-ref (slot-ref self '-state))
  )

(define-method (inspect (self <ga-automation-run>))
  (list (class-name (class-of self)) (name-get (genome-get self)) (seed-get self))
  )

(define-method (serialize (self <ga-automation-run>))
  (append (next-method) (serialize-slots self '-seed '-genome)))

;; Genomes

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
	(let ((f (open-file "best.txt" "a")))
	  (format f
			  "INSPECTION OF BEST:\n~a\n\n~a\n\n"
			  (pretty-string (serialize (car best)))
			  (pretty-string (serialize (cadr best))))
	  (close f)
	  )
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

(define (ga-evolve-population evaluated-genomes)
  ;; Note: don't evolve elites
  (let* ((best (ga-by-best evaluated-genomes))
		 (no-elites (filter (lambda (nn) (not (is-elite nn))) best)))
	(assert (>= (length best) 2))
	(map (lambda (n) (apply ga-combine (ga-select-parents best 2))) no-elites)))

;; Note: clears 'elite' flag then sets flag on the returned genomes
;; Returns list of elites
(define (ga-make-elites! evaluated-genomes count)
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
(define (ga-on-evaluated genome steps fitness-func)
  (fitness-set! genome (fitness-func steps ga-max-moves))
  (signal-condition-variable ga-cond-complete)
  )

;; Run a single genome so that fitness can be evaluated
(define-method (ga-evaluate (run <ga-automation-run>) steps generation-start-func game-won-func
							game-step-func fitness-func)
  (if (= steps 0) (generation-start-func (seed-get run)))
  (let ((genome (genome-get run)))
	(assert (not (is-elite genome))) ; elite fitness should already be known
	(cond
	 ((game-won-func)
	  (display "Game won\n")
	  (ga-on-evaluated genome steps fitness-func)
	  #f
	  )
	 ((= ga-max-moves steps)
	  (display "Remaining steps expired\n")
	  (ga-on-evaluated genome steps fitness-func)
	  #f
	  )
	 (else
	  (game-step-func genome)
	  (idle-call
	   (lambda () (ga-evaluate run (1+ steps) generation-start-func game-won-func game-step-func fitness-func)))
	  #t
	  )
	 )
	)
  )

(define spawned #f)

(define (ga-evolve genomes seeds stats generation)
  (unless spawned (set! spawned #t) (begin-thread (aisleriot-server ga-n-child-spawns (the-environment))))
  
  (permute-it (lambda (t)
				(let ((run (make <ga-automation-run> #:genome (car t) #:seed (cdr t))))
				  (ga-queue! run (lambda (result)
								  (formatt "Completed ~a with result ~a (~a remaining)\n" (inspect run) result
										   (ga-queue-remaining))
								  (fitness-add! (genome-get run) result)
								  ))
				  )
				)
			  (filter (negate is-elite) genomes)
			  seeds
			  )

  (formatt "Waiting to process ~a runs\n" (ga-queue-remaining))
  (ga-queue-drain)
  
  (let ((stats-new
		 (cons (ga-calc-generation-stats generation genomes stats) stats))
		(next-generation
		 (sort (ga-generate-next-population genomes)
			   (lambda (a b) (string<? (name-last-get a) (name-last-get b)))))
		)
	(call-with-output-file
		"stats.txt"
		(lambda (file)
		  (for-each
		   (lambda (port)
			 (format port (pretty-string ga-config))
			 (format port "generation,max-fitness-with-elites,max-fitness-no-elites,min-fitness-no-elites,average\n")
			 (format port "~a\n" (string-join (map list->csv (reverse stats-new)) "\n"))
			 )
		   (list #t file)
		   )
		  ))
	(ga-evolve next-generation seeds stats-new (1+ generation))
	)
  )


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
			   (bind socket AF_INET INADDR_ANY port)
			   port
			   )
			 (lambda (key . args) (bind-with-free-port socket (1+ port)))
	  ))
  )

(define ga-queue-mutex (make-mutex))
(define ga-queue-change-next-mutex (make-recursive-mutex))
(define ga-queue-change-drain-mutex (make-recursive-mutex))
(define ga-queue-change-cond (make-condition-variable))
(define ga-queue '())

(define-method (ga-queue! (run <ga-automation-run>) on-complete)
  (with-mutex
   ga-queue-mutex
   (assert (not (memq run (map car ga-queue))))
   (set! ga-queue (append ga-queue (list (cons run on-complete))))
   )
  (ga-retry! run)
  )

(define-method (ga-retry! (run <ga-automation-run>))
  (assert (with-mutex ga-queue-mutex (memq run (map car ga-queue))))
  (atomic-box-set! (slot-ref run '-state) 'ready)
  (broadcast-condition-variable ga-queue-change-cond)
  )

(define (ga-queue-clear!)
  (assert (= (ga-queue-remaining) 0))
  (with-mutex ga-queue-mutex (set! ga-queue '()))
  (broadcast-condition-variable ga-queue-change-cond)
  )

(define (ga-queue-next-try)
  (with-mutex
   ga-queue-mutex
   (let ((result (find (lambda (t) (eq? (state-get (car t)) 'ready)) ga-queue)))
	 (if result (atomic-box-set! (slot-ref (car result) '-state) 'processing))
	 result
	 )
   )
  )

(define (ga-queue-next)
  (or (ga-queue-next-try)
	  (begin
		(with-mutex ga-queue-change-next-mutex
					(wait-condition-variable ga-queue-change-cond ga-queue-change-next-mutex))
		(ga-queue-next)))
  )

(define-method (ga-queue-complete! (run <ga-automation-run>))
  (atomic-box-set! (slot-ref run '-state) 'complete)
  (broadcast-condition-variable ga-queue-change-cond)
  )

(define (ga-queue-remaining)
  (with-mutex ga-queue-mutex (apply + (map (lambda (x) (if (eq? (state-get (car x)) 'complete) 0 1)) ga-queue)))
  )

(define (ga-queue-drain)
  (while (> (ga-queue-remaining) 0)
		 (with-mutex ga-queue-change-drain-mutex
					 (wait-condition-variable ga-queue-change-cond ga-queue-change-drain-mutex)
					 )
		 )
  (ga-queue-clear!)
  )

(define (aisleriot-server nchildren local-env)
  (let* ((sock-listen (socket PF_INET SOCK_STREAM 0))
		 (port (bind-with-free-port sock-listen 12345)))
	(unless port (error "Couldn't bind to a port"))
	(formatt "Bound to port ~a\n" port)
	(listen sock-listen 64)
	
	(for-each (lambda (i)
				(begin-thread
				 (formatt "Spawning child ~a\n" i)
				 (let ((pipe (open-pipe* OPEN_READ cmd "--automate" "--host" (format #f "127.0.0.1:~a"  port))))
				   (while
					#t
					(let ((t (read-line pipe 'split)))
					  (formatt "CHILD ~a: ~a\n" i (car t))
					  (if (eof-object? (cdr t)) (break)))
					)
				   )
				 (formatt "Child ~a died\n" port)
				 )
				)
			  (iota nchildren)
			  )

	(let f ((run-t-func (ga-queue-next)))
;;;			(formatt "Waiting for connection\n")
	  (let* ((sock-io (car (accept sock-listen)))
			 (serialized (serialize (car run-t-func))))
		(begin-thread
;;;				(formatt "Connection accepted. Sending to port ~a\n" port)

		 (catch
		  #t
		  (lambda ()
			(format sock-io "(quote ~s)" serialized)
			
			(force-output sock-io)
			(shutdown sock-io 1)
			
;;;				(formatt "Reading from port ~a\n" port)
			(let ((result (get-string-all sock-io)))
;;;				  (formatt "From child ~a server got: ~a\n" port result)
			  (close sock-io)
			  (let ((deserialized (deserialize-from-string result local-env)))
				(if (number? deserialized) ((cdr run-t-func) deserialized) (throw 'ga-error deserialized)))
			  (ga-queue-complete! (car run-t-func))
			  )
			)
		  (lambda (key . args)
			(if (memq key '(system-error ga-error))
				(begin
				  (formatt "Error: ~a ~a (retrying run)\n" key args)
				  (ga-retry! (car run-t-func))
				  )
				(apply throw key args))
			)
		  )
		 )
		)
	  (f (ga-queue-next))
	  )
	(close sock-listen)
	)
  )

;; Returns ga-automation-run
(define (ga-client-receive-automation-run local-env run-func complete-func error-func)
  (let ((host-param (member "--host" (program-arguments))))
	(if (not host-param)
		(error "No host given")
		(let* ((split-param (string-split (cadr host-param) #\:))
			   (host (car split-param))
			   (port (string->number (cadr split-param))))
		  (let ((sock (socket PF_INET SOCK_STREAM 0)))
			(let f ()
				   (catch 'system-error
						  (lambda ()
							(formatt "Waiting to get automation data from port ~a\n" port)
							(connect sock (addrinfo:addr (car (getaddrinfo host (object->string port) AI_NUMERICSERV))))
							)
						  (lambda (key . args) (formatt "~a ~a\n" key args) (sleep 1) (f)))
				   )
			(catch 'system-error
				   (lambda ()
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
						(write (fitness-get (genome-get result)) sock)
						(close sock)
						(complete-func)
						)
					   )
					 )
				   (lambda (key . args) (formatt "Error: ~a ~a\n" key args) (error-func))
				   )
		  )
		))
	)
  )

;;; GA setup
(define ga-test #f)
(define ga-n-child-spawns 0)

(define ga-population-size 48)
(define ga-training-set-size 10)

(define ga-combine-chance 0.5)
(define ga-mutation-chance 0.01)
(define ga-max-moves 100)

(define ga-elite-preserve-count 2)

(define ga-value-max 3.402823466e+38)

;; The degree to which genomes with better scores are favoured for breeding (proportionate only)
(define ga-factor-parent-best 0.5)

;;(define ga-select-parents ga-select-parents-fitness-proportionate)
(define ga-select-parents ga-select-parents-tournament)

(define ga-config
  `(
   (ga-test . ,ga-test)
   (ga-population-size . ,ga-population-size)
   (ga-training-set-size . ,ga-training-set-size)
   (ga-combine-chance . ,ga-combine-chance)
   (ga-mutation-chance . ,ga-mutation-chance)
   (ga-max-moves . ,ga-max-moves)
   (ga-elite-preserve-count . ,ga-elite-preserve-count)
   (ga-value-max . ,ga-value-max)
   (ga-factor-parent-best . ,ga-factor-parent-best)
   (ga-select-parents . ,ga-select-parents)
   )
  )

(if ga-test
	(begin
	  (set! ga-population-size 3)
	  (set! ga-training-set-size 1)))

(assert (< ga-elite-preserve-count ga-population-size))
(assert (>= ga-population-size 2))
