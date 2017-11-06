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

(define-class <serializable> (<object>)
  )

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
;;;  (formattpp "LIST CAR ~a CDR ~a\n" (car list) (cdr list)) 
  (if (equal? (string-ref (object->string (car list)) 0) #\<)
	  (let ((class (local-eval (car list) local-env)))
;;;		(formattpp "MAKE ~a\n" class) 
		(deserialize-slots! (make class) (cdr list) local-env)
		)
	  (map (lambda (e) (deserialize e local-env)) list))
  )

(define-method (deserialize-slot! (obj <object>) slot value local-env)
;;;  (formattpp "SLOT ~a VAL ~a\n" slot value) 
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
	 (let ((serialized (eval-string str)))
	   ;;	(pretty-print serialized)
	   (deserialize serialized local-env)
	   )
	 )
   (lambda (key . args) (formatt "Read error on input: ~a\n" str) (throw key)))
)
