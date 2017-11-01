(define-module (aisleriot serialize))

(use-modules
  (oop goops)
  )

(define-class <serializable> (<object>)
  )

(define-method (from-serialized (self <serializable>))
  (error "Please implement this method")
  )

(define-method (serialize (self <object>))
  self
  )

(define-method (serialize (self <serializable>))
  (class-name (class-of self))
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

(define-method (deserialize-slot (obj <object>) slot value)
  (slot-set! obj slot (deserialize value))
  )

(define-method (deserialize-slots (obj <object>) (list <list>))
  (for-each (lambda (t) (deserialize-slot obj (car t) (cdr t))) slots)
  )

(export
 <serializable>

 serialize
 serialize-slot
 serialize-slots
 )
