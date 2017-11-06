(define-module (aisleriot nn)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (rnrs base)
  #:use-module (aisleriot serialize)
  #:use-module (aisleriot permute)
  #:use-module (aisleriot formatt)
  #:re-export (serialize)
  #:export (<nn-layer>
			<nn-link>
			<nn-network>
			<nn-node>
			<nn-node-consumer>
			<nn-node-input>
			<nn-node-input-real>
			<nn-node-output>
			<nn-node-output-real>
			<nn-layer-input>
			
			cache-key
			cache-output-value!
			cached-output
			id-get
			inspect
			layer-input-get
			layer-input-set!
			layer-output-get
			layers-all-get
			layers-hidden-get
			links-in-get
			nn-network-fully-connected
			nodes-get
			randomize!
			value-get
			value-set!
			)
  )

;(define nn-value-max 3.402823466e+38)
(define nn-value-max 1.0)

(define-method (hash-djb (input <bytevector>) (mod <integer>))
  (let f ((hash 5381) (iinput (bytevector->u8-list input)))
	(cond ((null? iinput) (logand hash mod))
		  (else (f (+ (logand (ash hash 5) mod) hash (car iinput)) (cdr iinput))))))

(define-method (hash-djb (input <bytevector>))
  (hash-djb input #xFFFFFFFF))
  
;(hash-djb (uint-list->bytevector '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20) (native-endianness) 4))

;;; Neural network
(define-class <nn-network> (<serializable>)
  (-layer-input #:init-keyword #:layer-input #:getter layer-input-get #:setter layer-input-set!)
  (-layers-hidden #:init-keyword #:layers-hidden #:getter layers-hidden-get)
  (-layer-output #:init-keyword #:layer-output  #:getter layer-output-get)
  (-value-cache #:init-value '())
  (-value-cache-mutex #:init-thunk make-mutex)
  )

(define-method (serialize (self <nn-network>))
  (append (next-method)
		(serialize-slots self
						 '-layer-input
						 '-layers-hidden
						 '-layer-output
						 ))
  )

(define-method (cache-output-value! (self <nn-network>) cache-key value)
  (with-mutex
   (slot-ref self '-value-cache-mutex)
   (slot-set! self '-value-cache (cons (cons cache-key value) (slot-ref self '-value-cache)))
   )
  )

(define-method (cached-output (self <nn-network>) cache-key)
  (with-mutex
   (slot-ref self '-value-cache-mutex)
   (assoc cache-key (slot-ref self '-value-cache))
   )
  )

(define-method (cache-key (self <nn-network>))
  (map value-get (nodes-get (layer-input-get self)))
  )

(define-method (inspect (self <nn-network>))
  (formattpps
   "<nn-network \n input: ~a\n hidden: ~a\n output: ~a>"
   (inspect (slot-ref self '-layer-input) self)
   (map (lambda (l) (inspect l self)) (layers-hidden-get self))
   (inspect (layer-output-get self) self)
   )
  )

;;; Connection between neural network nodes.
(define-class <nn-link> (<serializable>)
  (-node-source-id #:init-keyword #:node-source-id)
  (-node-dest-id #:init-keyword #:node-dest-id)
  (-weight #:init-value 1.0 #:init-keyword #:weight) ; <real>
  )

(define-method (serialize (self <nn-link>))
  (append (next-method) (serialize-slots self '-node-source-id '-node-dest-id '-weight)))

(define-method (inspect (self <nn-link>))
  (formattpps
   "src: ~a dst: ~a weight: ~a"
   (slot-ref self '-node-source-id)
   (slot-ref self '-node-dest-id)
   (slot-ref self '-weight))
  )

(define-method (randomize! (self <nn-link>))
  (slot-set! self '-weight (* (+ -1.0 (random 2.0)) nn-value-max))
  self)

(define-method (contribution (self <nn-link>) (network <nn-network>))
  (* (value-get (node-by-id network (slot-ref self '-node-source-id)) network) (slot-ref self '-weight)))

;;; Node in a neural network.
(define-class <nn-node> (<serializable>)
  (-id #:init-keyword #:id #:getter id-get)
  )

(define-method (serialize (self <nn-node>))
  (append (next-method) (serialize-slots self '-id)))

(define-method (inspect (self <nn-node>) (network <nn-network>))
  (formattpps
   "<~a ~a>"
   (class-name (class-of self))
   (-display self network))
  )

(define-method (-display (self <nn-node>) (network <nn-network>))
  (format #f "~a" (slot-ref self '-id)))

;;; Node that accepts links.
(define-class <nn-node-consumer> (<nn-node>)
  (-links-in #:init-keyword #:links-in #:getter links-in-get #:setter links-in-set!) ; <list <nn-link>>
  )

(define-method (serialize (self <nn-node-consumer>))
  (append (next-method) (serialize-slots self '-links-in)))

(define-method (-display (self <nn-node-consumer>) (network <nn-network>))
  (formattpps
   "~a links-in: ~a"
   (next-method)
   (links-in-get self))
  )

(define-method (randomize! (self <nn-node-consumer>))
  (let each-link ((ilinks (links-in-get self)))
	(cond ((null? ilinks) '())
		  (else (randomize! (car ilinks))
				(each-link (cdr ilinks)))))
  self)

(define-method (value-get (self <nn-node-consumer>) (network <nn-network>))
  (apply + (map (lambda (l) (contribution l network)) (links-in-get self))))


;;; Input node.
(define-class <nn-node-input> (<nn-node>)
  )

(define-method (value-get (self <nn-node-input>) (network <nn-network>))
  (error "Implementation required"))

(define-method (clear! (self <nn-node-input>))
  (error "Implementation required"))

(define-class <nn-node-input-real> (<nn-node>)
  (-value #:init-keyword #:value #:init-value 0.0 #:getter value-get #:setter value-set!)
  )

(define-method (value-get (self <nn-node-input-real>) (network <nn-network>))
  (value-get self))

(define-method (clear! (self <nn-node-input-real>))
  (value-set! self 0.0))

;;; Output node.
(define-class <nn-node-output> (<nn-node-consumer>)
  )

;;; Terminating node with real value output.
(define-class <nn-node-output-real> (<nn-node-output>)
  )

;;; Network layer.
(define-class <nn-layer> (<serializable>)
  (-nodes #:init-keyword #:nodes #:getter nodes-get)
  )

(define-method (serialize (self <nn-layer>))
  (append (next-method) (serialize-slots self '-nodes)))

(define-method (clear! (self <nn-layer>))
  (for-each (lambda (n) (clear! n)) (nodes-get self)))

(define-method (inspect (self <nn-layer>) (network <nn-network>))
  (formattpps
		  "<nn-layer nodes: ~a>"
		  (map (lambda (n) (inspect n network)) (nodes-get self)))
  )

(define-method (node-by-id (self <nn-layer>) id)
  (find (lambda (n) (equal? (id-get n) id)) (nodes-get self))
  )

;; Make new links between each node
;; layer1 should be a layer full of node-consumers
(define-method (fully-connect! (layer0 <nn-layer>) (layer1 <nn-layer>))
  (permute-it (lambda (anodes)
				(links-in-set! (cdr anodes) (cons (make <nn-link> #:node-source-id (id-get (car anodes)) #:node-dest-id (id-get (cdr anodes)))
												  (links-in-get (cdr anodes)))))
			  (nodes-get layer0)
			  (nodes-get layer1))
  )

(define-method (randomize! (self <nn-layer>))
  (let each-node ((inodes (slot-ref self '-nodes)))
	(cond ((null? inodes) '())
	(else (randomize! (car inodes))
		  (each-node (cdr inodes)))))
  self)

(define-class <nn-layer-input> (<nn-layer>)
  )

;;; Neural network
(define-method (node-by-id (self <nn-network>) id)
  (let f ((llayers (layers-all-get self)))
	(cond ((null? llayers) (throw '-nn-layer-no-node id))
		  (else (or (node-by-id (car llayers) id) (f (cdr llayers)))))))

(define-method (layers-all-get (self <nn-network>))
  (append (list (layer-input-get self)) (layers-hidden-get self) (list (layer-output-get self))))

(define-method (fully-connect! (self <nn-network>))
  (let connect ((li (layers-all-get self)))
	(cond ((null? (cdr li)) self)
		  (else (fully-connect! (car li) (cadr li))
				(connect (cdr li)))))
  self
  )

(define (nn-network-fully-connected class layer-input nhiddenlayers nhiddennodes nodes-outputs)
  (let* ((layer-output
		  (make <nn-layer> #:nodes nodes-outputs))
		 (layers-hidden
		  (let make-hlayers ((out '()) (il nhiddenlayers))
			(if (= 0 il) out
				(make-hlayers
				 (cons
				  (make <nn-layer>
					#:nodes (let make-nodes ((out '()) (i nhiddennodes))
							  (if (= 0 i) out
								  (make-nodes (cons (make <nn-node-consumer>
													  #:id (format #f "h~a~a" il i)
													  #:links-in '())
													out)
											  (1- i))
								  )))
				  out)
				 (1- il)))))
		 )
	(fully-connect!
	 (make class
	   #:layer-input layer-input
	   #:layers-hidden layers-hidden
	   #:layer-output layer-output)
	 )
	))

(define-method (randomize! (self <nn-network>))
  (let each-layer ((ilayers (append (layers-hidden-get self) (list (slot-ref self '-layer-output)))))
	(cond ((null? ilayers) '())
		  (else (randomize! (car ilayers))
				(each-layer (cdr ilayers)))))
  self)
