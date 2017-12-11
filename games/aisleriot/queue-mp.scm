(define-module (aisleriot queue-mp)
  #:use-module (aisleriot formatt)
  #:use-module (oop goops)
  #:use-module (srfi srfi-18)
  #:use-module (rnrs base)
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
  (mutex-lock! (slot-ref queue '-mutex))
  (assert (and (not (memq obj (slot-ref queue '-pending)))
			   (not (memq obj (slot-ref queue '-processing)))
			   ))
  (slot-set! queue '-pending (append (slot-ref queue '-pending) (list obj)))
  (mutex-unlock! (slot-ref queue '-mutex))
  (condition-variable-broadcast! (slot-ref queue '-cond-change))
  )

;; Can be called when item processing has failed and is recoverable.
(define-method (requeue! (queue <queue-mp>) obj)
  (mutex-lock! (slot-ref queue '-mutex))
  (assert (and (not (memq obj (slot-ref queue '-pending)))
			   (memq obj (slot-ref queue '-processing))
			   ))
  (slot-set! queue '-processing (delq obj (slot-ref queue '-processing)))
  (slot-set! queue '-pending (append (slot-ref queue '-pending) (list obj)))
  (mutex-unlock! (slot-ref queue '-mutex))
  (condition-variable-broadcast! (slot-ref queue '-cond-change))
  )

;; Blocks until an item is available.
(define-method (wait-next! (queue <queue-mp>))
  (mutex-lock! (slot-ref queue '-mutex))
  (if (null? (slot-ref queue '-pending))
	  (begin
		(mutex-unlock! (slot-ref queue '-mutex) (slot-ref queue '-cond-change))
		(wait-next! queue))
	  (let ((next (car (slot-ref queue '-pending))))
		(slot-set! queue '-processing (append (slot-ref queue '-processing) (list next)))
		(slot-set! queue '-pending (cdr (slot-ref queue '-pending)))
		(mutex-unlock! (slot-ref queue '-mutex))
		(condition-variable-broadcast! (slot-ref queue '-cond-change))
		next
		)
	  )
  )

(define-method (complete! (queue <queue-mp>) obj)
  (mutex-lock! (slot-ref queue '-mutex))
  (assert (and (not (memq obj (slot-ref queue '-pending)))
			   (memq obj (slot-ref queue '-processing))
			   ))
  (slot-set! queue '-processing (delq obj (slot-ref queue '-processing)))
  (mutex-unlock! (slot-ref queue '-mutex))
  (condition-variable-broadcast! (slot-ref queue '-cond-change))
  )

;; Returns (n-pending n-processing)
(define-method (lengths (queue <queue-mp>))
  (mutex-lock! (slot-ref queue '-mutex))
  (let ((result (list (length (slot-ref queue '-pending))
					  (length (slot-ref queue '-processing)))))
	(mutex-unlock! (slot-ref queue '-mutex))
	result
	)
  )

;; Blocks until queue is empty
(define-method (drain (queue <queue-mp>))
  (mutex-lock! (slot-ref queue '-mutex))
  (if (and (null? (slot-ref queue '-pending)) (null? (slot-ref queue '-processing)))
	  (begin
		(mutex-unlock! (slot-ref queue '-mutex))
		)
	  (begin
		(mutex-unlock! (slot-ref queue '-mutex) (slot-ref queue '-cond-change))
		(drain queue)
		)
	  )
  )
