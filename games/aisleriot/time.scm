(define-module (aisleriot time)
  #:use-module ((srfi srfi-19) #:prefix srfi:)
  #:export (srfi-time->seconds
			)
  )

(define (srfi-time->seconds duration)
  (exact->inexact (+ (srfi:time-second duration) (/ (srfi:time-nanosecond duration) 1000000000)))
  )
