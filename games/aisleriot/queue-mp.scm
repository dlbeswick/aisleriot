(define-module (aisleriot queue-mp)
  #:use-module (ice-9 threads)
  #:use-module (oop goops)
  #:use-module (rnrs base)
  #:use-module (aisleriot formatt)
  #:export (<queue-mp>

			queue!
			requeue!
			wait-next!
			complete!
			lengths
			drain
			)
  )

(define-class <queue-mp> (<object>)
  (-mutex #:init-thunk make-mutex)
  (-cond-change #:init-thunk make-condition-variable)
  (-pending #:init-value '())
  (-processing #:init-value '())
  )

;; 'obj' must have not been supplied before.
(define-method (queue! (queue <queue-mp>) obj)
  (with-mutex
   (slot-ref queue '-mutex)
   (assert (and (not (memq obj (slot-ref queue '-pending)))
				(not (memq obj (slot-ref queue '-processing)))
				))
   (slot-set! queue '-pending (append (slot-ref queue '-pending) (list obj)))
   )
  (broadcast-condition-variable (slot-ref queue '-cond-change))
  obj
  )

;; Can be called when item processing has failed and is recoverable.
(define-method (requeue! (queue <queue-mp>) obj)
  (with-mutex
   (slot-ref queue '-mutex)
   (assert (and (not (memq obj (slot-ref queue '-pending)))
				(memq obj (slot-ref queue '-processing))
				))
   (slot-set! queue '-processing (delq obj (slot-ref queue '-processing)))
   (slot-set! queue '-pending (append (slot-ref queue '-pending) (list obj)))
   )
  (broadcast-condition-variable (slot-ref queue '-cond-change))
  obj
  )

;; Blocks until an item is available.
(define-method (wait-next! (queue <queue-mp>))
  (let ((next
		 (with-mutex
		  (slot-ref queue '-mutex)
		  (if (null? (slot-ref queue '-pending))
			  (begin
				(wait-condition-variable (slot-ref queue '-cond-change) (slot-ref queue '-mutex))
				#nil)
			  (begin
				(let ((next (car (slot-ref queue '-pending))))
				  (slot-set! queue '-processing (append (slot-ref queue '-processing) (list next)))
				  (slot-set! queue '-pending (cdr (slot-ref queue '-pending)))
				  next
				  )
				)
			  )
		  )))
	(if next
		(begin
		  (broadcast-condition-variable (slot-ref queue '-cond-change))
		  next
		  )
		(wait-next! queue)
		)
	)
  )

(define-method (complete! (queue <queue-mp>) obj)
  (with-mutex
   (slot-ref queue '-mutex)
   (assert (and (not (memq obj (slot-ref queue '-pending)))
				(memq obj (slot-ref queue '-processing))
				))
   (slot-set! queue '-processing (delq obj (slot-ref queue '-processing)))
   )
  (broadcast-condition-variable (slot-ref queue '-cond-change))
  )

;; Returns (n-pending n-processing)
(define-method (lengths (queue <queue-mp>))
  (with-mutex
   (slot-ref queue '-mutex)
   (list (length (slot-ref queue '-pending))
		 (length (slot-ref queue '-processing)))
   )
  )

;; Blocks until queue is empty
(define-method (drain (queue <queue-mp>))
  (unless (with-mutex
		   (slot-ref queue '-mutex)
		   (if (and (null? (slot-ref queue '-pending)) (null? (slot-ref queue '-processing)))
			   #t
			   (begin
				 (wait-condition-variable (slot-ref queue '-cond-change) (slot-ref queue '-mutex))
				 #f))
		   )
	  (drain queue))
  )
