;; Extended format functions. "The 't' is for 'threadsafe'"
;; Prevents interleaving of output by multiple threads.
(define-module (aisleriot formatt)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 threads)
  #:export (formatt
			formatts
			formattpp
			formattpps
			pretty-string)
  )

(define stdout-mutex (make-mutex))

;; Note: unlike format, always prints to stdout
(define (formatt . args)
  (with-mutex stdout-mutex (apply format #t args))
  )

;; Returns a string
(define (formatts . args)
  (apply format #f args)
  )

;; formatt with pretty-printed args
(define (formattpp . args)
  (apply formatt (map pretty-string args))
  )

;; Returns a string
(define (formattpps . args)
  (apply formatts (map pretty-string args))
  )

;; Pretty-print to string
(define (pretty-string obj)
  (call-with-output-string (lambda (port) (pretty-print obj port)))
  )
