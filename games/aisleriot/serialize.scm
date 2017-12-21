(define-module (aisleriot serialize)
 #:use-module (oop goops)
 #:use-module (ice-9 eval-string)
 #:use-module (ice-9 local-eval)
 #:use-module (ice-9 pretty-print)
 #:use-module (aisleriot formatt)
 #:export (<serializable>
		   deserialize
		   deserialize-from-port
		   deserialize-from-string
		   deserialize-stream-from-port
		   serialize
		   serialize-slots
		   serialize-slots-get
		   serialize-stream
		   serialize-stream-slots)
 )

(define (ref-class? obj)
  (equal? (string-ref (object->string obj) 0) #\<)
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

;; This method can be overriden to perform custom serialization, but typically it should only be necessary to override
;; serialize-slots-get.
(define-method (serialize (self <serializable>))
  (append (list (class-name (class-of self))) (apply serialize-slots self (serialize-slots-get self)))
  )

(define-method (serialize (self <string>)) self)

(define-method (serialize (self <symbol>)) self)

(define-method (serialize (self <number>)) self)

(define-method (serialize (self <boolean>)) self)

(define-method (serialize (list <list>)) (map serialize list))

(define-method (serialize-stream port (self <serializable>))
  (write (class-name (class-of self)) port)
  (newline port)
  (apply serialize-stream-slots port self (serialize-slots-get self))
  )

(define (serialize-stream-default port obj)
  (write obj port)
  (newline port)
  )

(define-method (serialize-stream port (self <string>)) (serialize-stream-default port self))
(define-method (serialize-stream port (self <symbol>)) (serialize-stream-default port self))
(define-method (serialize-stream port (self <number>)) (serialize-stream-default port self))
(define-method (serialize-stream port (self <boolean>)) (serialize-stream-default port self))

(define-method (serialize-stream port (list <list>))
  (write '<list> port)
  (newline port)
  (write (length list) port)
  (newline port)
  (for-each (lambda (e) (serialize-stream port e)) list)
  )

(define-method (serialize-stream-slot port (obj <object>) (slot <symbol>))
  (write slot port)
  (newline port)
  (serialize-stream port (slot-ref obj slot))
  )

(define-method (serialize-stream-slots port (obj <object>) . slots)
  (write (length slots) port)
  (newline port)
  (for-each (lambda (slot) (serialize-stream-slot port obj slot)) slots)
  )

;; These slots will be serialized by default for an object. No other slots will be serialized.
;;
;; Example:
;;
;; (define-method (serialize-slots-get (d <derived>))
;;   (append (next-method) '(-slot1 -slot2)))
(define-method (serialize-slots-get (self <serializable>))
  '())

(define-method (serialize-slot (obj <object>) (slot <symbol>))
  (cons slot (serialize (slot-ref obj slot)))
  )

(define-method (serialize-slots (obj <object>) . slots)
  (map (lambda (slot) (serialize-slot obj slot)) slots)
  )

(define-method (deserialize-stream-slot! port (obj <object>) slot value local-env)
  (slot-set! obj slot (deserialize-stream port value local-env))
  obj
  )

(define-method (deserialize-stream-slots! port (obj <object>) local-env)
  (let f ((count (read port)) (result '()))
	(if (= 0 count)
		(reverse result)
		(f (1- count) (cons (deserialize-stream-slot! port obj (read port) (read port) local-env) result)))
	)
  obj
  )

(define (deserialize-stream port obj local-env)
  (cond ((equal? obj '<list>)
		 (let f ((count (read port)) (result '()))
		   (if (= 0 count)
			   (reverse result)
			   (f (1- count) (cons (deserialize-stream port (read port) local-env) result)))))
		((ref-class? obj)
		 (let ((class (class-get obj local-env)))
		   (deserialize-stream-slots! port (make class) local-env)
		   ))
		(else obj)
	  )
  )

(define-method (deserialize object local-env)
  object
  )

(define-method (deserialize (list <list>) local-env)
  (cond ((null? list) list)
		((ref-class? (car list))
		 (let ((class (class-get (car list) local-env)))
		   (deserialize-slots! (make class) (cdr list) local-env)
		   ))
		(else (map (lambda (e) (deserialize e local-env)) list)))
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
  (call-with-input-string str (lambda (port) (deserialize-from-port port local-env)))
)

(define (deserialize-from-port port local-env)
  (catch
   'read-error 
   (lambda () (deserialize (read port) local-env))
   (lambda (key . args) (formatt "Read error on deserialize: ~a\n" args) (throw key)))
)

(define (deserialize-stream-from-port port local-env)
  (deserialize-stream port (read port) local-env)
  )
