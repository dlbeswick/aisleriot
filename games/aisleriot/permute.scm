(define-module (aisleriot permute)
  #:use-module (srfi srfi-1))

;; Return the next list of lists in the sequence:
;; ((1 2 3) (a b))
;; ((2 3) (a b))
;; ((3) (a b))
;; ((1 2 3) (b))
;; ...
;; ((3) (b))
;; ()
;;
;; llo: original list of lists
;; ll: result of last iteration, or llo
(define (llists-permute-iterate llo ll)
  (cond
   ;; Terminating condition: all input lists have one element
   ((let done ((lli ll))
	  (or (null? lli) (and (null? (cdar lli)) (done (cdr lli))))) '())
   (else
	(let f ((out '())
			(lloi llo)
			;; If the first item is empty then init lli to ll, else remove an element from ll's head and set lli to it. 
			(lli (if (null? (car ll)) ll (cons (cdar ll) (cdr ll)))))
	  (let*
		  ;; Record if the first list is empty.
		  ((exhausted (null? (car lli)))
		   ;; If the head is exhausted then replenish it from the head of the original list.
		   (newhead (list-head (if exhausted lloi lli) 1)))
		(cond
		 ;; If no more elements following, then append new head and return.
		 ((null? (cdr lli)) (append out newhead))
		 (else (f (append out newhead)
				  (cdr lloi)
				  ;; If the head of lli is exhausted, then remove the first item from the list that follows.
				  ;; I.e. (() (1 2 3) (a b)) will become ((2 3) (a b))
				  (if exhausted (cons (cdadr lli) (cddr lli)) (cdr lli))))))))))

;; Depth-first permutation of the given lists.
;; 'proc' is called with an alist of each permutation.
;; Returns the total number of permutations.
(define-public (permute-it proc . l)
  (cond ((null? l) '())
		((null? (cdr l)) l)
		(else (let f ((r l) (cpermutations 0))
				(cond ((null? r) cpermutations)
					  (else
					   (proc (fold (lambda (e p) (cons p (car e))) (caar r) (cdr r)))
					   (f (llists-permute-iterate l r) (1+ cpermutations))))))))

(define-public (permute . l)
  (let ((result '()))
	(apply permute-it (lambda (alist) (set! result (cons alist result))) l)
	result
	)
  )
