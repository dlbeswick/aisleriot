(define-module (aisleriot serialize)
 #:use-module (oop goops)
 #:use-module (ice-9 eval-string)
 #:use-module (ice-9 local-eval)
 #:use-module (ice-9 pretty-print)
 #:use-module (aisleriot formatt)
 #:export (<serializable>
		   deserialize
		   deserialize-from-string
		   serialize
		   serialize-slot
		   serialize-slots)
 )

(define cache-class (make-hash-table 31))

(define (class-get name local-env)
  (let ((result (hashq-ref cache-class name)))
	(if result result (hashq-set! cache-class name (local-eval name local-env)))
	)
  )

(define-class <serializable> (<object>)
  )

;; This restriction ensures that only those classes that have serialization explicitly defined will be serialized.
(define-method (serialize (self <object>))
  (error "Objects must be derived from serializable to be serialized" self)
  )

(define-method (serialize (self <serializable>))
  (list (class-name (class-of self)))
  )

(define-method (serialize (self <list>))
  self
  )

(define-method (serialize (self <string>))
  self
  )

(define-method (serialize (self <number>))
  self
  )

(define-method (serialize (self <boolean>))
  self
  )

(define-method (serialize (self <list>))
  (map serialize self)
  )
									 
(define-method (serialize-slot (obj <object>) slot)
  (cons slot (serialize (slot-ref obj slot)))
  )

(define-method (serialize-slots (obj <object>) . slots)
  (map (lambda (slot) (serialize-slot obj slot)) slots)
  )

(define-method (deserialize object local-env)
  object
  )

(define-method (deserialize (list <list>) local-env)
  (if (equal? (string-ref (object->string (car list)) 0) #\<)
	  (let ((class (class-get (car list) local-env)))
		(deserialize-slots! (make class) (cdr list) local-env)
		)
	  (map (lambda (e) (deserialize e local-env)) list))
  )

(define-method (deserialize-slot! (obj <object>) slot value local-env)
  (slot-set! obj slot (deserialize value local-env))
  obj
  )

(define-method (deserialize-slots! (obj <object>) (slots <list>) local-env)
  (for-each (lambda (t) (deserialize-slot! obj (car t) (cdr t) local-env)) slots)
  obj
  )

(define-method (deserialize-from-string (str <string>) local-env)
  (catch
   'read-error 
   (lambda ()
	 (let ((serialized (call-with-input-string str read)))
	   (deserialize serialized local-env)
	   )
	 )
   (lambda (key . args) (formatt "Read error on input: ~a\n" str) (throw key)))
)
