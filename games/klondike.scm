; AisleRiot - klondike.scm
; Copyright (C) 1998, 2003 Jonathan Blandford <jrb@mit.edu>
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(use-modules (aisleriot interface)
			 (aisleriot api)
			 (oop goops)
			 (ice-9 format)
			 (srfi srfi-1)
			 (rnrs base)
			 (rnrs bytevectors)
			 )

(define-method (hash-djb (input <bytevector>) (mod <integer>))
  (let f ((hash 5381) (iinput (bytevector->u8-list input)))
	(cond ((null? iinput) (logand hash mod))
		  (else (f (+ (logand (ash hash 5) mod) hash (car iinput)) (cdr iinput))))))

(define-method (hash-djb (input <bytevector>))
  (hash-djb input #xFFFFFFFF))
  
;(hash-djb (uint-list->bytevector '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20) (native-endianness) 4))


(define deal-one #t)    ;deal one card at a time from stock to waste, 2 redeals
(define deal-three #f)  ;deal three cards at a time from stock to waste
(define no-redeal #f)   ;deal one card at a time from stock to waste, no redeals
(define unl-redeal #f)  ;deal one card at a time from stock to waste, unlimited redeals
(define kings-only #t)  ;only allow kings to be moved to empty slots

(define max-redeal 2)   ;number of redeals, -1 for unlimited

; The set up:

(define tableau '(6 7 8 9 10 11 12))
(define foundation '(2 3 4 5))
(define stock 0)
(define waste 1)

;;; NN setup

(define nn-population-size 100)
(define nn-combine-chance 0.5)
(define nn-mutation-chance 0.001)
(define nn-max-moves 100)

(define vowels (string->char-set "aeiou"))
(define consonants (char-set-difference
					(char-set-intersection char-set:lower-case char-set:ascii)
					  vowels))
(define (charset-random-char charset) (list-ref (char-set->list charset) (random (char-set-size charset))))
(define (generate-name nclusters)
  (cond ((= 0 nclusters) "")
		(else (string-append
			   (string (charset-random-char vowels) (charset-random-char consonants))
			   (generate-name (1- nclusters))))))

;;; Observations:
;;; Each population member has new game.
;;; Successive generations bred by two fittest.
;;; 1 hidden layer having 6 nodes
;;;  Max moves 100
;;;   Population size 5
;;;    Mutation 0.1, combine 0.5, did not improve fitness after 1000 generations. 
;;;    Mutation 0.1, combine 0.3333, did not improve fitness after 1000 generations. 
;;;    Mutation 0.01, combine 0.5, did not improve fitness after 1000 generations. 
;;;   Population size 100
;;;    Mutation 0.01, combine 0.5, after 13 generations best fitness was 40.
;;;   Population size 50
;;;    Mutation 0.001, combine 0.5, after 13 generations best fitness was 30. Highest seen was 50.
;;;    Mutation 0.001, combine 0.5, after 269 generations best fitness was 30. Highest seen was 50.
;;; New parent selection, aiming for greater diversity.
;;;    Mutation 0.001, combine 0.5, pop size 1000, after 73 generations best fitness was 60,
;;;     average was 13.89 (top 14.65)
;;; Experiment modification: use same game for entire generation.
;;;  Mutation 0.001, combine 0.5, pop size 100, after 45 generations best fitness was 45, avg 10
;;; 2 hidden layers having 6 nodes
;;;  Mutation 0.001, combine 0.5, pop size 100, after 10 generations avg fitness was 10
;;; 2 hidden layers having 24 nodes
;;;  Mutation 0.001, combine 0.5, pop size 100, after 10 generations avg fitness was 14.8
;;;
;;; Change game state and move encoding. One input group per slot with length, top suit, top value. One input group
;;;  for move cards, one for deal.
;;;  Mutation 0.001, combine 0.5, pop size 100, after 22 generations avg fitness was 5.0. Best 10.0. Poor result.

(assert (>= nn-population-size 2))

(define (evaluate-next game evaluated-networks pending-networks steps generation stats)
  (let ((network (car pending-networks)))
	(format #t "GENERATION: ~a candidates remaining: ~a tested: '~a' had fitness ~a\n"
			generation (length pending-networks) (name-get network) (fitness-get network))
	(if (null? (cdr pending-networks))
		(begin
		  (meta-new-game)
		  (let ((stats-new (cons (nn-calc-generation-stats evaluated-networks stats) stats)))
			(format #t "Stats history: ~a\n" (string-join (map object->string stats-new) "\n"))
			(evaluate game
					  '()
					  (nn-evolve-population (cons network evaluated-networks))
					  0
					  (1+ generation)
					  stats-new
					  '())
			)
		  )
		(begin
		  (meta-restart-game)
		  (evaluate game (cons network evaluated-networks) (cdr pending-networks) 0 generation stats '()))))
)

(define (evaluate game evaluated-networks pending-networks steps generation stats move-cache)
  (let ((network (car pending-networks)))
	(let ((fitness (fitness-eval network game steps)))
;;;	  (format #t " fitness: ~a\n" fitness)
	  (fitness-set! network fitness)
	  )
	(cond
;;;	 ((game-over)
;;;	  (display "Game lost\n")
;;;	  (evaluate-next game evaluated-networks pending-networks steps generation)
;;;	  #t)
	 ((game-won)
	  (display "Won!\n")
	  (format #t "\nNETWORK:\n~a\n\n" (inspect network)) #t)
	 ((= nn-max-moves steps)
	  (display "Remaining steps expired\n")
	  (evaluate-next game evaluated-networks pending-networks steps generation stats))
	 (else
	  (let* ((gs (make <game-state> #:game game)))
;;;		(format #t "Step ~a\n" steps)
;;;		(display "\nALL CARDS\n")
		
;;;		(let f ((slots (slots-all game)))
;;;		  (unless (null? slots)
;;;			(format #t "Slot ~a: ~a\n" (car slots) (get-slot (car slots)))
;;;			(f (cdr slots))))
		
;;;		(format #t "\nALL VALID MOVES\n")
		(let ((moves '()))
		  (moves-valid
		   gs
		   (lambda (m)
;;;			 (format #t "~a\n" (inspect m))
			 (set! moves (cons m moves))))
		  
		  (unless (null? moves)
				  ;; Try all moves on network
				  (let ((evals
						 (map (lambda (m)
								;;(format #t "Try move on network: ~a\n" (inspect m))
								;;(format #t "\nNETWORK:\n~a\n\n" (inspect network))
								(let ((prob-result (success-probability! network m game move-cache)))
								  (set! move-cache (cdr prob-result)) ; solve side-effect?
								  (cons m (car prob-result))
								  )
								)
							  (reverse moves))))
					(let ((by-score (sort evals (lambda (ala alb) (> (cdr ala) (cdr alb))))))
;;;				(for-each (lambda (al) (format #t "Success chance for move ~a: ~a\n" (inspect (car al)) (cdr al))) by-score)
					  (execute gs (caar by-score))
					  )
					)
				  )
		  )
			
		(delayed-call (lambda () (evaluate game evaluated-networks pending-networks (1+ steps) generation stats
										   move-cache)))
		)
	  )
	 )
	)
  )

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
(define (permute-it proc . l)
  (cond ((null? l) '())
		((null? (cdr l)) l)
		(else (let f ((r l) (cpermutations 0))
				(cond ((null? r) cpermutations)
					  (else
					   (proc (fold (lambda (e p) (cons p (car e))) (caar r) (cdr r)))
					   (f (llists-permute-iterate l r) (1+ cpermutations))))))))

;;; Neural network
(define-class <nn-network> (<object>)
  (-layer-input #:init-keyword #:layer-input #:getter layer-input-get)
  (-layers-hidden #:init-keyword #:layers-hidden #:getter layers-hidden-get)
  (-layer-output #:init-keyword #:layer-output  #:getter layer-output-get)
  (-fitness #:init-value 0.0 #:getter fitness-get #:setter fitness-set!)
  (-name-first #:init-form (generate-name (+ 1 (random 4)))
			   #:init-keyword #:name-first #:getter name-first-get #:setter name-first-set!)
  (-name-last #:init-form (generate-name (+ 1 (random 4)))
			  #:init-keyword #:name-last #:getter name-last-get #:setter name-last-set!)
  )

(define-method (name-get (self <nn-network>))
  (string-append (name-first-get self) " " (name-last-get self)))

(define-method (inspect (self <nn-network>))
  (format #f
		  "<nn-network ~a\n input: ~a\n hidden: ~a\n output: ~a>"
		  (name-get self)
		  (inspect (slot-ref self '-layer-input) self)
		  (map (lambda (l) (inspect l self)) (layers-hidden-get self))
		  (inspect (layer-output-get self) self)
		  )
  )

;;; Connection between neural network nodes.
(define-class <nn-link> (<object>)
  (-node-source-id #:init-keyword #:node-source-id)
  (-node-dest-id #:init-keyword #:node-dest-id)
  (-weight #:init-value 1.0 #:init-keyword #:weight) ; <real>
  )

(define-method (inspect (self <nn-link>))
  (format #f
		  "src: ~a dst: ~a weight: ~a"
		  (slot-ref self '-node-source-id)
		  (slot-ref self '-node-dest-id)
		  (slot-ref self '-weight)))

(define-method (randomize! (self <nn-link>))
  (slot-set! self '-weight (random 1.0))
  self)

(define-method (contribution (self <nn-link>) (network <nn-network>))
  (* (value-get (node-by-id network (slot-ref self '-node-source-id)) network) (slot-ref self '-weight)))

;;; Node in a neural network.
(define-class <nn-node> (<object>)
  (-id #:init-keyword #:id #:getter id-get)
  )

(define-method (inspect (self <nn-node>) (network <nn-network>))
  (format #f
		  "<~a ~a>"
		  (class-name (class-of self))
		  (-display self network)))

(define-method (-display (self <nn-node>) (network <nn-network>))
  (format #f "~a" (slot-ref self '-id)))

;;; Node that accepts links.
(define-class <nn-node-consumer> (<nn-node>)
  (-links-in #:init-keyword #:links-in #:getter links-in-get #:setter links-in-set!) ; <list <nn-link>>
  )

(define-method (-display (self <nn-node-consumer>) (network <nn-network>))
  (format #f
		  "~a links-in: ~a"
		  (next-method)
		  (string-join (map inspect (links-in-get self)) "\n")))

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

;;; Hidden layer node.
(define-class <nn-node-hidden> (<nn-node-consumer>)
  )

;;; Network layer.
(define-class <nn-layer> (<object>)
  (-nodes #:init-keyword #:nodes #:getter nodes-get)
  )

(define-method (inspect (self <nn-layer>) (network <nn-network>))
  (format #f
		  "<nn-layer nodes: ~a>"
		  (map (lambda (n) (inspect n network)) (nodes-get self))))

(define-method (node-by-id (self <nn-layer>) id)
  (find (lambda (n) (equal? (id-get n) id)) (nodes-get self)))

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

(define-method (clear! (self <nn-layer>))
  (for-each (lambda (n) (clear! n)) (nodes-get self)))

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
		  (let make-hlayers ((out '()) (i nhiddenlayers))
			(if (= 0 i) out
				(make-hlayers
				 (cons
				  (make <nn-layer>
					#:nodes (let make-nodes ((out '()) (i nhiddennodes))
							  (if (= 0 i) out
								  (make-nodes (cons (make <nn-node-hidden>
													  #:id (format #f "h~a" i)
													  #:links-in '())
													out)
											  (1- i))
								  )))
				  out)
				 (1- i)))))
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


;;; A potential game move. It need not be valid.
(define-class <move> (<object>)
  -game #:init-keyword #:game
  )

(define-method (id-get (self <move>))
  (assert #f))

(define-method (valid? (self <move>))
  (assert #f))

(define-method (nn-encode (self <move>))
  (assert #f))

(define-method (inspect (self <move>))
  (format #f "#<~a ~a>" (class-name (class-of self)) (-inspect self)))

(define-method (-inspect (self <move>))
  (format #f "id: ~a" (id-get self)))

;;; Move by dealing a card from the deck.
(define-class <move-deal> (<move>)
  )

(define-method (equal? (a <move-deal>) (b <move-deal>))
  #t)

(define-method (id-get (self <move-deal>))
  999)

(define-method (valid? (self <move-deal>))
  (dealable?))

(define-method (nn-encode (self <move-deal>))
  (assert (valid? self))
  1
  )

;;; Move by shifting a stack of cards of size 1 or greater.
(define-class <move-cards> (<move>)
  (-slot-source #:init-keyword #:slot-source #:getter slot-source-get)
  (-slot-dest #:init-keyword #:slot-dest #:getter slot-dest-get)
  (-slot-source-card-idx #:init-keyword #:slot-source-card-idx #:getter slot-source-card-idx-get)
  )

(define-method (equal? (a <move-cards>) (b <move-cards>))
  (and (equal? (slot-source-get a) (slot-source-get b))
	   (equal? (slot-dest-get a) (slot-dest-get b))
	   (equal? (slot-source-card-idx-get a) (slot-source-card-idx-get b))
	   )
  )

(define-method (-inspect (self <move-cards>))
  (format #f
		  "~a src: ~a src-card: ~a dst: ~a"
		  (next-method)
		  (inspect (slot-source-get self))
		  (slot-source-card-idx-get self)
		  (inspect (slot-dest-get self))))

(define-method (id-get (self <move-cards>))
  (format #f "~a-~a-~a"
		  (id-get (slot-source-get self))
		  (slot-source-card-idx-get self)
		  (id-get (slot-dest-get self))))

;;; Get the list of cards that would be picked up by the player for this move.
;;; '() if no cards could be picked up.
;;; Order is lowest to highest.
(define-method (card-list-get (self <move-cards>))
  (let ((top-build (get-top-build (map legacy-get (cards-get (slot-source-get self))) '())))
	(if (>= (slot-source-card-idx-get self) (length top-build))
		'()
		(map (lambda (c) (make <card> #:card c)) (take (reverse top-build) (1+ (slot-source-card-idx-get self))))))
  )

;;; Card at source position and slot
(define-method (card-source-get (self <move-cards>))
  (list-ref (cards-get (slot-source-get self)) (slot-source-card-idx-get self)))

;;; True if move is valid
(define-method (valid? (self <move-cards>))
  (and (visible? (card-source-get self))
	   ;; Only the top cards of non-expanded slots (i.e. foundation) can be played.
	   (or (expanded? (slot-source-get self)) (= (slot-source-card-idx-get self) 0))
	   (not (empty? (slot-source-get self)))
	   (droppable? (id-get (slot-source-get self)) (map legacy-get (card-list-get self)) (id-get (slot-dest-get self)))
	   )
  )

;;; Information relating to game rules
(define-class <card> (<object>)
  (-card #:init-keyword #:card #:getter legacy-get)
  )

(define-method (visible? (self <card>))
  (is-visible? (slot-ref self '-card)))

(define-method (suit-get (self <card>))
  (get-suit (slot-ref self '-card)))

(define-method (color-get (self <card>))
  (get-color (slot-ref self '-card)))

(define-method (inspect (self <card>))
  (format #f "#<card ~a>" (legacy-get self)))

(define-class <slot> (<object>)
  (-id #:init-keyword #:id #:getter id-get)
  )

(define-method (equal? (a <slot>) (b <slot>))
  (equal? (id-get a) (id-get b)))

(define-method (cards-get (self <slot>))
  (map (lambda (c) (make <card> #:card c)) (get-cards (id-get self))))

(define-method (empty? (self <slot>))
  (empty-slot? (id-get self)))

(define-method (expanded? (self <slot>))
  (is-slot-expanded? (id-get self)))

(define-method (card-top-make-visible (self <slot>))
  (make-visible-top-card (id-get self)))

(define-method (inspect (slot <slot>))
  (format #f "#<slot id: ~a>" (id-get slot)))

(define-class <game> (<object>)
  (-tableau #:init-keyword #:tableau #:getter tableau-get)
  (-foundation #:init-keyword #:foundation #:getter foundation-get)
  (-waste #:init-keyword #:waste #:getter waste-get))

(define-method (slots-all (self <game>))
  (append (tableau-get self)
		  (foundation-get self)
		  (list (waste-get self))))

;;; A node in the current game state tree
(define-class <game-state> (<object>)
  (-game #:init-keyword #:game #:getter game-get)
  )

;; All potential moves at this node.
(define-method (moves-all-it (self <game-state>) proc)
  ;; Get alist of slots and indicies with cards ((slot . card-idx) (#<slot> . 1) ... )
  ;; Used to make a move from every card in the stack that could be played from a slot.
  (let ((slots-with-idxs
		 (append-map (lambda (slot)
					   (map (lambda (idx) (cons slot idx)) (iota (length (cards-get slot)))))
					 (slots-all (game-get self)))))
	;; Get all combinations of moves among the slots and each card in the slots
	;; (((slot-src . card-idx) . slot-dst) ((#<slot> . 0) . #<slot>) ... )
	(permute-it (lambda (p)
				  (proc
				   (make <move-cards>
					 #:slot-source (caar p)
					 #:slot-source-card-idx (cdar p)
					 #:slot-dest (cdr p))))
				slots-with-idxs
				(slots-all (game-get self))))
  (proc (make <move-deal>))
  )
 
;; All possible valid moves at this node.
(define-method (moves-valid (self <game-state>) proc)
  (moves-all-it self (lambda (m) (if (valid? m) (proc m)))))

(define-method (execute (gs <game-state>) (move <move-cards>))
  (let* ((moving-cards (card-list-get move))
		 (newsrccards (list-tail (cards-get (slot-source-get move)) (length moving-cards)))
		 (newdstcards (append moving-cards (cards-get (slot-dest-get move)))))
;;;	(format #t "card move: ~a srccards: ~a newsrc: ~a newdst: ~a\n"
;;;			(inspect move) (map inspect moving-cards) (map inspect newsrccards) (map inspect newdstcards))
	;; Remove cards from source slot
	(set-cards! (id-get (slot-source-get move)) (map legacy-get newsrccards))
	;; Put cards in dest slot
	(set-cards! (id-get (slot-dest-get move)) (map legacy-get newdstcards))
	(unless (empty? (slot-source-get move))
	  (card-top-make-visible (slot-source-get move)))
	)
  )

(define-method (execute (gs <game-state>) (move <move-deal>))
  (meta-deal))

;;; NN for cards

(define (nn-encode-no-card)
  '(0 0 0))

(define-method (nn-encode (self <slot>))
  (append
   (list (length (cards-get self)))
   (cdr (if (empty? self) ; cdr = ignore 'visible' flag
			(nn-encode-no-card)
			(nn-encode (car (cards-get self))))))
  )

(define-method (nn-encode (self <card>))
  (if (visible? self)
	  (list (color-get self) (suit-get self) 1)
	  '(2 0 0))
  )

(define-method (nn-encode-length-move-cards)
  10
  )

(define-method (nn-encode (self <move-cards>))
  (assert (valid? self))
  (let ((result
		 (append (list (id-get (slot-source-get self)))
				 (list (id-get (slot-dest-get self)))
				 (list (slot-source-card-idx-get self))
				 (list (length (card-list-get self)))
				 (nn-encode (card-source-get self))
				 (nn-encode (slot-dest-get self))
				 )))
;;;	(format #t "Encoded movecards: ~a\n" result)
	(assert (= (length result) (nn-encode-length-move-cards)))
	result
	)
  )

(define-method (nn-encode (self <game-state>))
  (apply append (map nn-encode (slots-all (game-get self))))
  )

(define-method (mutation-factor (p0 <number>) (p1 <number>) (c <number>))
  (if (or (= p0 c) (= p1 c)) 0.0 1.0))

(define-method (mutation-factor (p0 <nn-link>) (p1 <nn-link>) (c <nn-link>))
  (mutation-factor (slot-ref p0 '-weight) (slot-ref p1 '-weight) (slot-ref c '-weight)))

(define-method (mutation-factor (p0 <nn-node>) (p1 <nn-node>) (c <nn-node>))
  0.0)

(define-method (mutation-factor (p0 <nn-node-consumer>) (p1 <nn-node-consumer>) (c <nn-node-consumer>))
  (if (null? (links-in-get p0)) 0.0
	  (/ (fold (lambda (ep0 ep1 ec acc) (+ acc (mutation-factor ep0 ep1 ec)))
			   0.0
			   (links-in-get p0)
			   (links-in-get p1)
			   (links-in-get c))
		 (length (links-in-get p0)))))

(define-method (mutation-factor (p0 <nn-layer>) (p1 <nn-layer>) (c <nn-layer>))
  (/ (fold (lambda (ep0 ep1 ec acc) (+ acc (mutation-factor ep0 ep1 ec)))
		   0.0
		   (nodes-get p0)
		   (nodes-get p1)
		   (nodes-get c))
	 (length (nodes-get p0))))

;;; Get a normalized value describing the degree to which network c differs from either of its parents p0 and p1.
(define-method (mutation-factor (p0 <nn-network>) (p1 <nn-network>) (c <nn-network>))
  (/ (fold (lambda (ep0 ep1 ec acc) (+ acc (mutation-factor ep0 ep1 ec)))
		   0.0
		   (layers-all-get p0)
		   (layers-all-get p1)
		   (layers-all-get c))
	 (length (layers-all-get p0))))

(define-method (nn-combine (n0 <number>) (n1 <number>))
  (if (<= (random 1.0) nn-mutation-chance) (random 1.0)
	  (if (<= (random 1.0) nn-combine-chance) n1 n0)))

(define-method (nn-combine (link0 <nn-link>) (link1 <nn-link>))
  (assert (equal? (class-of link0) (class-of link1)))
  (assert (equal? (slot-ref link0 '-node-source-id) (slot-ref link1 '-node-source-id)))
  (assert (equal? (slot-ref link0 '-node-dest-id) (slot-ref link1 '-node-dest-id)))
  (make (class-of link0)
	#:node-source-id (slot-ref link0 '-node-source-id)
	#:node-dest-id (slot-ref link0 '-node-dest-id)
	#:weight (nn-combine (slot-ref link0 '-weight) (slot-ref link1 '-weight)))
  )

(define-method (nn-combine (node0 <nn-node>) (node1 <nn-node>))
  (assert (equal? (id-get node0) (id-get node1)))
  (assert (equal? (class-of node0) (class-of node1)))
  (make (class-of node0)
	#:id (id-get node0))
  )

(define-method (nn-combine (node0 <nn-node-consumer>) (node1 <nn-node-consumer>))
  (let ((node (next-method)))
	(slot-set! node '-links-in (reverse (fold (lambda (a b r) (cons (nn-combine a b) r)) '() (links-in-get node0) (links-in-get node1))))
	node))

(define-method (nn-combine (layer0 <nn-layer>) (layer1 <nn-layer>))
  (assert (equal? (class-of layer0) (class-of layer1)))
  (assert (equal? (length (nodes-get layer0)) (length (nodes-get layer1))))
  (make (class-of layer0)
	#:nodes (reverse (fold (lambda (a b r) (cons (nn-combine a b) r)) '() (nodes-get layer0) (nodes-get layer1))))
  )

;;; The better network should be passed to network0
(define-method (nn-combine (network0 <nn-network>) (network1 <nn-network>))
  (assert (equal? (class-of network0) (class-of network1)))
  (let ((result (make (class-of network0)
	#:layer-input (nn-combine (layer-input-get network0) (layer-input-get network1))
	#:layers-hidden (reverse (fold (lambda (a b r) (cons (nn-combine a b) r)) '() (layers-hidden-get network0) (layers-hidden-get network1)))
	#:layer-output (nn-combine (layer-output-get network0) (layer-output-get network1))
	)))

	;; Give the network the last name of its best parent. If it was significantly mutated, give it a new last name.
	(if (= 0 (mutation-factor network0 network1 result))
		(name-last-set! result (name-last-get network0)))
	
	result
  ))

(define (nn-by-best networks)
  (sort networks (lambda (a b) (> (fitness-get a) (fitness-get b)))))

;;; networks should be ordered by fitness
(define (nn-select-parents networks probability ndesired)
  (let f ((out '()) (ln networks) (p probability))
	(cond ((= (length out) ndesired) (reverse out))
		  ((null? ln) (list-head networks ndesired))
		  ((<= (random 1.0) p) (f (cons (car ln) out) (cdr ln) (* p p)))
		  (else (f (cons (car ln) out) (cdr ln) p)))))

(define (nn-calc-generation-stats evaluated-networks stats)
  (let ((best (nn-by-best evaluated-networks)))
	(format #t "INSPECTION OF BEST:\n~a\n\n~a\n\n" (inspect (car best)) (inspect (cadr best)))
	(format #t "EVOLVE POPULATION:\n Final 25 fitnesses: ~a\n"
			(list-head (map (lambda (nn) (cons (name-get nn) (fitness-get nn))) best) (min (length best) 25))
			)
	(exact->inexact (/ (apply + (map fitness-get best)) (length best)))
	)
  )

(define-method (nn-evolve-population evaluated-networks)
  (let ((best (nn-by-best evaluated-networks)))
	(assert (>= (length best) 2))
	(map (lambda (n) (apply nn-combine (nn-select-parents best 0.95 2))) evaluated-networks)))


;;; Klondike
(define-class <nn-network-klondike> (<nn-network>)
  )

(define-method (fitness-eval (self <nn-network-klondike>) (game <game>) (steps-elapsed <integer>))
  (apply + (- nn-max-moves steps-elapsed)
		 (map (lambda (slot) (* 5 (length (cards-get slot)))) (foundation-get game)))
  )

;;; Returns (outcome new-move-cache)
(define-method (success-probability! (network <nn-network-klondike>) (move <move>) (game <game>) move-cache)
  ;; If the network ping-pongs a lot, then the cache can speed up the inevitable
  ;;(format #t "~a\n" move-cache)
  ;(let ((cached (assoc move move-cache)))
	;(if cached
		;(begin
		  ;(display "CACHE HIT\n")
		  ;(list (cdar move-cache) move-cache)
		  ;)
		;(begin
		  (nn-network-set! network move game)
		  (let ((outcome (value-get (car (nodes-get (layer-output-get network))) network)))
			(cons outcome (cons (cons move outcome) move-cache))
			)
		  ;))
	;)
  )

(define-class <nn-layer-input-klondike> (<nn-layer>)
  )

;;; Note: searching for nodes rather than holding references simplifies reconnecting networks after mutation.
(define-method (nodes-gamestate-get (self <nn-layer-input-klondike>))
  (filter (lambda (n) (string-contains (id-get n) "-gamestate-")) (nodes-get self)))

(define-method (nodes-movecards-get (self <nn-layer-input-klondike>))
  (filter (lambda (n) (string-contains (id-get n) "-movecards-")) (nodes-get self)))

(define-method (node-deal-get (self <nn-layer-input-klondike>))
  (find (lambda (n) (equal? (id-get n) "input-deal")) (nodes-get self)))

;;; One group per slot with length, suit shown, value shown.
;;; One group of 11 move-cards test nodes.
;;; One deal node.
(define-method (nn-make-gamestate-nodes (game <game>))
  (append-map
   (lambda (slot)
	 (map
	  (lambda (i) (make <nn-node-input-real> #:id (format #f "input-gamestate-~a-~a" (id-get slot) i)))
	  (iota 3))
	 )
   (sort (slots-all game) (lambda (slot0 slot1) (>= (id-get slot0) (id-get slot1)))))
  )

(define (nn-make-movecards-nodes)
  (map (lambda (i) (make <nn-node-input-real> #:id (format #f "input-movecards-~a" i)))
	   (iota (nn-encode-length-move-cards))
	   )
  )

(define-method (nn-make-input-layer (game <game>))
  (make <nn-layer-input-klondike>
	#:nodes (append (nn-make-gamestate-nodes game)
					(nn-make-movecards-nodes)
					(list (make <nn-node-input-real> #:id "input-deal"))
					)
	)
  )

(define-method (nn-network-klondike (game <game>))
  (let ((input-layer (nn-make-input-layer game)))
	(nn-network-fully-connected
	 <nn-network-klondike>
	 input-layer
	 1
	 (floor (/ (1+ (length (nodes-get input-layer))) 2)) ; +1 output node
	 (list (make <nn-node-output-real> #:id "output" #:links-in '())))
	)
  )

(define-method (nn-network-set! (network <nn-network-klondike>) (move <move>) (game <game>))
  (clear! (layer-input-get network))
  ;; Set gamestate nodes
  (for-each value-set!
			(nodes-gamestate-get (layer-input-get network))
			(append-map nn-encode (sort (slots-all game) (lambda (slot0 slot1) (>= (id-get slot0) (id-get slot1)))))
			)
  )

(define-method (nn-network-set! (network <nn-network-klondike>) (move <move-cards>) (game <game>))
  (next-method)
  ;; Set card move
  (for-each value-set!
			(nodes-movecards-get (layer-input-get network))
			(nn-encode move)
			)
;;;	(format #t "Setting move ~a to node ~a\n" (inspect move) (inspect target-node network))
  )

(define-method (nn-network-set! (network <nn-network-klondike>) (move <move-deal>) (game <game>))
  (clear! (layer-input-get network))
  (value-set! (node-deal-get (layer-input-get network)) 1.0)
  )

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)

  (make-standard-deck)
  (shuffle-deck)
  
  (add-normal-slot DECK 'stock)

  (if deal-three
      (add-partially-extended-slot '() right 3 'waste)
      (add-normal-slot '() 'waste))

  (add-blank-slot)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-carriage-return-slot)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)

  (deal-tableau tableau)
  
  (map flip-top-card tableau)

  (give-status-message)

  (list 7 3.1)
)

(define (deal-tableau tableau)
  (if (not (null? tableau))
      (begin
        (deal-cards stock tableau)
        (deal-tableau (cdr tableau)))))

(define (give-status-message)
  (set-statusbar-message (string-append (get-stock-no-string)
					"   "
					(get-redeals-string))))

(define (get-redeals-string)
  (if (< max-redeal 0) ""
      (string-append (_"Redeals left:") " "
		     (number->string (- max-redeal FLIP-COUNTER)))))

(define (get-stock-no-string)
  (string-append (_"Stock left:") " " 
		 (number->string (length (get-cards 0)))))

(define (is-tableau-build? card-list)
  (and (is-visible? (car card-list))
       (or (null? (cdr card-list))
           (and (not (color-eq? (car card-list) (cadr card-list)))
                (= (get-value (cadr card-list))
                   (+ 1 (get-value (car card-list))))
                (is-tableau-build? (cdr card-list))))))

(define (button-pressed slot-id card-list)
  (and (or (> slot-id 1)
	   (and (= slot-id 1)
		(= (length card-list) 1)))
       (is-tableau-build? card-list)))

(define (complete-transaction start-slot card-list end-slot)
  (move-n-cards! start-slot end-slot card-list)
  (if (member start-slot foundation)
      (add-to-score! -1))
  (if (member end-slot foundation)
      (add-to-score! 1))
  (if (and (not (empty-slot? start-slot)) 
	   (member start-slot tableau))
      (make-visible-top-card start-slot))
  #t)

(define (button-released start-slot card-list end-slot)
  (if (droppable? start-slot card-list end-slot)
      (complete-transaction start-slot card-list end-slot) 
  #f))

(define (droppable? start-slot card-list end-slot)
  (and (not (= start-slot end-slot))
       (or (and (member end-slot tableau)
		(if (empty-slot? end-slot)
		    (or (not kings-only)
		        (= king (get-value (car (reverse card-list)))))
		    (and (not (eq? (is-red? (get-top-card end-slot))
				   (is-red? (car (reverse card-list)))))
			 (= (get-value (get-top-card end-slot))
			    (+ (get-value (car (reverse card-list))) 1)))))
	   (and (member end-slot foundation)
		(= 1 (length card-list))
		(if (empty-slot? end-slot)
		    (= ace (get-value (car card-list)))
		    (and (= (get-suit (get-top-card end-slot))
			    (get-suit (car card-list)))
			 (= (get-value (get-top-card end-slot)) 
			    (- (get-value (car card-list)) 1))))))))

(define (dealable?)
  (flippable? stock waste max-redeal))

(define (do-deal-next-cards)
  (flip-stock stock waste max-redeal (if deal-three 3 1)))

(define (button-clicked start-slot)
  (and (= start-slot stock)
       (flip-stock stock waste max-redeal 
                               (if deal-three 3 1))))

(define (button-double-clicked start-slot)
  (or (and (member start-slot foundation)
	   (autoplay-foundations))
      (and (member start-slot (cons waste tableau))
	   (not (empty-slot? start-slot))
	   (let* ((target-card
		   (cond ((= (get-value(get-top-card start-slot)) ace) '())
			 (#t (add-to-value (get-top-card start-slot) -1))))
		  (end-slot (search-foundation target-card foundation)))
	     (and end-slot
		  (complete-transaction start-slot 
					(list (remove-card start-slot)) 
					end-slot))))))

(define (search-foundation card foundations)
  (or-map (lambda (slot) (if (equal? card (get-top-card slot))
			     slot
			     #f)) foundations))

(define (autoplay-foundations)
  (define (autoplay-foundations-tail)
    (if (or-map button-double-clicked (cons waste tableau))
        (delayed-call autoplay-foundations-tail)
        #t))
  (if (or-map button-double-clicked (cons waste tableau))
      (autoplay-foundations-tail)
      #f))

; Global variables used in searching (keeping it simple):

(define build '())
(define card #f)
(define color 0)
(define suit 0)
(define value 0)
(define slot-id1 0)

(define (match? slot-id2)
  (and (not (empty-slot? slot-id2))
       (= suit (get-suit (get-top-card slot-id2)))
       (= value (get-value (get-top-card slot-id2)))
       (hint-move slot-id2 1 slot-id1)))

(define (ploppable? slot-id)
  (and (not (empty-slot? slot-id))
       (set! card (get-top-card slot-id))
       (set! suit (get-suit card))
       (set! value (+ (get-value card) 1))
       (set! slot-id1 slot-id)
       (or-map match? (cons waste tableau))))

(define (is-ace? slot-id)
  (and (not (empty-slot? slot-id))
       (= ace (get-value (get-top-card slot-id)))
       (hint-move slot-id 1 (find-empty-slot foundation))))

(define (shiftable? slot-id2)
  (and (not (= slot-id2 slot-id1))
       (if (empty-slot? slot-id2)
	   (and (= value king)
		(hint-move slot-id1 (length build) slot-id2))
	   (and (= (get-value (get-top-card slot-id2)) (+ 1 value))
		(not (= (get-color (get-top-card slot-id2)) color))
		(hint-move slot-id1 (length build) slot-id2)))))

(define (get-top-build card-list acc)
  (if (or (null? card-list)
          (not (is-visible? (car card-list))))
      acc
      (if (or (null? acc)
              (and (not (color-eq? (car card-list) (car acc)))
                   (= (get-value (car card-list))
                      (+ 1 (get-value (car acc))))))
          (get-top-build (cdr card-list) (cons (car card-list) acc))
          acc)))

(define (shiftable-iter slot-id)
  (and (not (empty-slot? slot-id))
       (begin
         (set! build (get-top-build (get-cards slot-id) '()))
	 (set! card (car build))
	 (set! color (get-color card))	
	 (set! value (get-value card))
	 (set! slot-id1 slot-id)
	 (and (not (and (= value king)
			(equal? card (car (reverse (get-cards slot-id))))))
	      (or-map shiftable? tableau)))))

(define (addable? slot-id)
  (if (empty-slot? slot-id)
      (and (= (get-value card) king)
	   (hint-move waste 1 slot-id))
      (and (= (get-value (get-top-card slot-id)) (+ 1 (get-value card)))
	   (not (= (get-color (get-top-card slot-id)) (get-color card)))
	   (hint-move waste 1 slot-id))))

(define (any-slot-nonempty? slots)
  (if (eq? slots '())
      #f
      (or (not (empty-slot? (car slots)))
          (any-slot-nonempty? (cdr slots)))))

(define (autoplay)
  (display "Autoplay begins\n")
  (set! *random-state* (random-state-from-platform))
  (let* ((game (make <game>
				 #:tableau (map (lambda (id) (make <slot> #:id id)) tableau)
				 #:foundation (map (lambda (id) (make <slot> #:id id)) foundation)
				 #:waste (make <slot> #:id waste)
				 ))
		 (networks (map (lambda (n)
						  (format #t "Initialize network ~a\n" n)
						  (randomize! (nn-network-klondike game))) (iota nn-population-size)))
		 )
	(delayed-call (lambda () (evaluate game '() networks 0 0 '() '()))))
  )

(define (get-hint)
  (or (or-map is-ace? (cons waste tableau))
      (or-map shiftable-iter tableau)
      (and (not (empty-slot? waste))
	   (set! card (get-top-card waste))
	   (or-map addable? tableau))
      (or-map ploppable? foundation)
      (and (not kings-only)
           (any-slot-empty? tableau)
           (any-slot-nonempty? (cons waste tableau))
           (list 0 (_"Consider moving something into an empty slot")))
      (and (or (and (or (= max-redeal -1)
			(< FLIP-COUNTER max-redeal))
		    (not (empty-slot? waste)))
	       (not (empty-slot? stock))) 
	   (hint-click stock (_"Deal a new card from the deck")))
; FIXME: need to give proper hints for this case too ...
      (and (not (and-map empty-slot? foundation))
           (list 0 (_"Try moving cards down from the foundation")))
      (list 0 (_"No hint available right now"))))

(define (game-won)
  (and (= 13 (length (get-cards 2)))
       (= 13 (length (get-cards 3)))
       (= 13 (length (get-cards 4)))
       (= 13 (length (get-cards 5)))))

; The hints still miss some useful reversible moves:
;
; 1) unplopping cards to assist in shifting groups,
; 2) unplopping cards to assist in plopping cards in other suits, 
; 3) shifting groups to assist in plopping & unplopping cards.
;
; so we must NOT report game-over when they run out.

(define (game-over)
  (give-status-message)
  (not (game-won)))

(define (get-options)
  (list 'begin-exclusive 
	(list (_"Three card deals") deal-three)
	(list (_"Single card deals") deal-one)
	(list (_"No redeals") no-redeal)
	(list (_"Unlimited redeals") unl-redeal)
	'end-exclusive))

(define (apply-options options)
  (set! deal-three (cadr (list-ref options 1)))
  (set! deal-one (cadr (list-ref options 2)))
  (set! no-redeal (cadr (list-ref options 3)))
  (set! unl-redeal (cadr (list-ref options 4)))
  (set! max-redeal (cond (no-redeal 0)
			 (deal-one 2)
			 (#t -1))))

(define (timeout) #f)

(set-features droppable-feature dealable-feature autoplay-feature)

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint get-options apply-options timeout droppable? dealable? autoplay)
