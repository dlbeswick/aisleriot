(define-module (aisleriot math)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:export (avg
			bitlist->bytevector
			bytevector->bitlist
			)
  )

(define (avg list)
  (exact->inexact (/ (apply + list) (length list)))
  )

(define (bytevector->bitlist bv)
  (apply append
		 (map (lambda (byte)
				(map (lambda (i)
					   (if (= (logand byte (ash 1 i)) 0) 0 1))
					 (reverse (iota 8))))
			  (array->list bv)))
  )

(define (bitlist->bytevector bits)
  (u8-list->bytevector
   (let f ((result '()) (head (take bits 8)) (tail (drop bits 8)))
	 (let ((result-new (append result
							   (list (apply + (map (lambda (i b) (* b (ash 1 i))) (reverse (iota 8)) head))))))
	   (if (null? tail) result-new (f result-new (take tail 8) (drop tail 8))))
	 ))
  )

