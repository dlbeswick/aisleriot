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

(define (report)
  (let* ((game (make <game> #:tableau tableau #:foundation foundation #:waste waste))
		 (gs (make <game-state> #:game game))
		 (network (randomize! (nn-network-klondike game))))

	(format #t "\nNETWORK:\n~a\n" (inspect network))
	
	(display "\nALL CARDS\n")

	(let f ((slots (slots-all game)))
	  (unless (null? slots)
		(format #t "Slot ~a: ~a\n" (car slots) (get-slot (car slots)))
		(f (cdr slots))))
   
	(format #t "\nALL VALID MOVES\n")
	(let ((moves '()))
	  (moves-valid
	   gs
	   (lambda (m)
		 (format #t "~a\n" (inspect m))
		 (set! moves (cons m moves))))

	  (for-each (lambda (m)
				  (format #t "Try move on network: ~a\n" (inspect m))
				  (nn-network-set! network m game)
				  (format #t "\nNETWORK:\n~a\n" (inspect network))
				  (format #t " Success chance: ~a\n" (success-probability network))
				  )
				(reverse moves))
	  
	  (if (null? moves) '()
	'());	  (begin (execute gs (last moves)) (delayed-call report))))
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

;;; Connection between neural network nodes.
(define-class <nn-link> (<object>)
  (-node-source #:init-keyword #:node-source)
  (-node-dest #:init-keyword #:node-dest)
  (-weight #:init-value 1.0) ; <real>
  )

(define-method (inspect (self <nn-link>))
  (format #f
		  "src: ~a dst: ~a weight: ~a"
		  (slot-ref self '-node-source)
		  (slot-ref self '-node-dest)
		  (slot-ref self '-weight)))

(define-method (contribution (self <nn-link>))
  (* (value-get (slot-ref self '-node-source)) (slot-ref '-weight)))

(define-method (randomize! (self <nn-link>))
  (slot-set! self '-weight (random 1.0))
  self)

;;; Node in a neural network.
(define-class <nn-node> (<object>)
  (-id #:init-keyword #:id)
  )

(define-method (inspect (self <nn-node>))
  (format #f
		  "<~a ~a>"
		  (class-name (class-of self))
		  (-display self)))

(define-method (-display (self <nn-node>))
  (format #f "~a" (slot-ref self '-id)))


;;; Node that accepts links.
(define-class <nn-node-consumer> (<nn-node>)
  (-links-in #:init-keyword #:links-in #:getter links-in-get #:setter links-in-set!) ; <list <nn-link>>
  )

(define-method (-display (self <nn-node-consumer>))
  (format #f
		  "~a links-in: ~a"
		  (next-method)
		  (map inspect (links-in-get self))))

(define-method (randomize! (self <nn-node-consumer>))
  (let each-link ((ilinks (links-in-get self)))
	(cond ((null? ilinks) '())
		  (else (randomize! (car ilinks))
				(each-link (cdr ilinks)))))
  self)

(define-method (value-get (self <nn-node-consumer>))
  (fold (lambda (link acc) (+ (contribution link) acc) 0.0 (links-in-get self))))


;;; Input node.
(define-class <nn-node-input> (<nn-node>)
  )

(define-method (value-get (self <nn-node-input>))
  (error "Implementation required"))

(define-method (clear! (self <nn-node-input>))
  (error "Implementation required"))

;;; Output node.
(define-class <nn-node-output> (<nn-node-consumer>)
  )

;;; Terminating node with real value output.
(define-class <nn-node-output-real> (<nn-node-output>)
  (-value #:init-value 0.0 #:getter value-get #:setter value-set!)
  )

;;; Hidden layer node.
(define-class <nn-node-hidden> (<nn-node-consumer>)
  )

;;; Network layer.
(define-class <nn-layer> (<object>)
  (-nodes #:init-keyword #:nodes #:getter nodes-get)
  )

(define-method (inspect (self <nn-layer>))
  (format #f
		  "<nn-layer nodes: ~a>"
		  (map inspect (nodes-get self))))

;; Ideally shouldn't need this method. Use overrides of the network class and layer classes instead.
;; Refer to data using fields, not strings.
;(define-method (node-by-id id)
;  (find (lambda (n) (equal? (get-id n))) (nodes-get self)))

;; layer1 should be a layer full of node-consumers
(define-method (fully-connect! (layer0 <nn-layer>) (layer1 <nn-layer>))
  (permute-it (lambda (anodes)
				(links-in-set! (cdr anodes) (cons (make <nn-link> #:node-source (car anodes) #:node-dest (cdr anodes))
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
(define-class <nn-network> (<object>)
  (-layer-input #:init-keyword #:layer-input #:getter layer-input-get)
  (-layers-hidden #:init-keyword #:layers-hidden #:getter layers-hidden-get)
  (-layer-output #:init-keyword #:layer-output  #:getter layer-output-get)
  )

(define-method (inspect (self <nn-network>))
  (format #f
		  "<nn-network\n input: ~a\n hidden: ~a\n output: ~a>"
		  (inspect (slot-ref self '-layer-input))
		  (map inspect (layers-hidden-get self))
		  (inspect (layer-output-get self))))

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
		 (layers-all (append (list layer-input) layers-hidden (list layer-output)))
		 )
	(let connect ((li layers-all))
	  (cond ((null? (cdr li)) #nil)
			(else (fully-connect! (car li) (cadr li))
				  (connect (cdr li)))))
	(make class
	  #:layer-input layer-input
	  #:layers-hidden layers-hidden
	  #:layer-output layer-output)
	)
  )

(define-method (randomize! (self <nn-network>))
  (let each-layer ((ilayers (append (layers-hidden-get self) (list (slot-ref self '-layer-output)))))
	(cond ((null? ilayers) '())
		  (else (randomize! (car ilayers))
				(each-layer (cdr ilayers)))))
  self)

;;; A potential game move. It need not be valid.
(define-class <move> (<object>)
  )

(define-method (inspect (self <move>))
  (format #f "#<~a ~a>" (class-name (class-of self)) (-inspect self)))

(define-method (-inspect (self <move>))
  (format #f "id: ~a" (id-get self)))

(define-method (id-get (self <move>))
  (assert #f))

(define-method (valid? (self <move>))
  (assert #f))

;;; Move by dealing a card from the deck.
(define-class <move-deal> (<move>)
  )

(define-method (id-get (self <move-deal>))
  999)

(define-method (valid? (self <move-deal>))
  (dealable?))

;;; Move by shifting a stack of cards of size 1 or greater.
(define-class <move-cards> (<move>)
  (-slot-source #:init-keyword #:slot-source #:getter slot-source-get)
  (-slot-dest #:init-keyword #:slot-dest #:getter slot-dest-get)
  (-slot-source-card-idx #:init-keyword #:slot-source-card-idx #:getter slot-source-card-idx-get)
  )

(define-method (-inspect (self <move-cards>))
  (format #f
		  "~a src: ~a src-card: ~a dst: ~a"
		  (next-method)
		  (slot-source-get self)
		  (slot-source-card-idx-get self)
		  (slot-dest-get self)))

(define-method (id-get (self <move-cards>))
  (format #f "~a-~a-~a" (slot-source-get self) (slot-source-card-idx-get self) (slot-dest-get self)))

(define (-nn-card-encode card)
  (list (car card)     ; rank
		(caar card)    ; suit
		(if (caaar card) 1 0))) ; flipped

(define-method (nn-encode (self <move-cards>))
  (append (list (slot-source-get move)
				(slot-dest-get move)
				(slot-source-card-idx move))
		  (map -nn-card-encode (card-list-get move))))

;;; Get the list of cards that would be picked up by the player for this move.
;;; '() if no cards could be picked up.
;;; Order is lowest to highest.
(define-method (card-list-get (self <move-cards>))
  (let ((top-build (get-top-build (get-cards (slot-source-get self)) '())))
	(if (>= (slot-source-card-idx-get self) (length top-build))
		'()
		(take (reverse top-build) (+ 1 (slot-source-card-idx-get self)))))
  )

;;; Card at source position and slot
(define-method (card-source-get (self <move-cards>))
  (list-ref (get-cards (slot-source-get self)) (slot-source-card-idx-get self)))

;;; True if move is valid
(define-method (valid? (self <move-cards>))
  (and (is-visible? (card-source-get self))
	   ;; Only the top cards of non-expanded slots (i.e. foundation) can be played.
	   (or (is-slot-expanded (slot-source-get self)) (= (slot-source-card-idx-get self) 0))
	   (not (empty-slot? (slot-source-get self)))
	   (droppable? (slot-source-get self) (card-list-get self) (slot-dest-get self))
	   )
  )

;;; Information relating to game rules
(define-class <game> (<object>)
  (-tableau #:init-keyword #:tableau)
  (-foundation #:init-keyword #:foundation)
  (-waste #:init-keyword #:waste))

(define-method (slots-all (self <game>))
  (append (slot-ref self '-tableau)
		  (slot-ref self '-foundation)
		  (list (slot-ref self '-waste))))

;;; A node in the current game state tree
(define-class <game-state> (<object>)
  (-game #:init-keyword #:game #:getter game-get)
  (-move-id #:init-value 0))

(define-method (move-id-create (self <game-state>))
  (let ((move-id (slot-ref self '-move-id)))
	(slot-set! self '-move-id (+ move-id 1))
	move-id))

;; All potential moves at this node.
(define-method (moves-all-it (self <game-state>) proc)
  ;; Get alist of slots and indicies with cards ((slot . card-idx) (6 . 1) ... )
  ;; Used to make a move from every card in the stack that could be played from a slot.
  (let ((slots-with-idxs
		 (append-map (lambda (slot)
					   (map (lambda (idx) (cons slot idx)) (iota (length (get-cards slot)))))
					 (slots-all (game-get self)))))
	;; Get all combinations of moves among the slots and each card in the slots
	;; (((slot-src . card-idx) . slot-dst) ((6 . 0) . 8) ... )
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
  (let* ((srccards (card-list-get move))
		 (newsrccards (list-tail (get-cards (slot-source-get move)) (length srccards)))
		 (newdstcards (append srccards (get-cards (slot-dest-get move)))))
	(format #t "card move: ~a srccards: ~a newsrc: ~a newdst: ~a\n" move srccards newsrccards newdstcards)
	;; Remove cards from source slot
	(set-cards-c! (slot-source-get move) newsrccards)
	;; Put cards in dest slot
	(set-cards-c! (slot-dest-get move) newdstcards)
	(unless (empty-slot? (slot-source-get move))
	  (make-visible-top-card (slot-source-get move)))))

(define-method (execute (gs <game-state>) (move <move-deal>))
  (deal))

;;; NN for cards
;;; Input node corresponding to a move from a source slot that can have a single card.
;;;
;;; Encodes data from the move as a numeric value.
(define-class <nn-node-input-move> (<nn-node-input>)
  (-move #:setter move-set! #:init-value #nil)  ; <move>
  (-value #:init-value #nil)  ; <move>
  )

(define-method (move-set! (self <nn-node-input-move>) (move <move>))
  (slot-set! self '-move move)
  (slot-set! self '-value #nil))

(define-method (clear! (self <nn-node-input-move>))
  (slot-set! self '-move #nil)
  (slot-set! self '-value #nil))

(define-method (-display (self <nn-node-input-move>))
  (format #f
		  "~a move: ~a value: ~a"
		  (next-method)
		  (slot-ref self '-move)
		  (value-get self)))

(define-method (value-get (self <nn-node-input-move>))
  (cond ((slot-ref self '-value) (slot-ref self '-value))
		(else (let ((result
					 (if (slot-ref self '-move)
						 (hash-djb (uint-list->bytevector (nn-encode (slot-ref self '-move))))
						 0)))
				(slot-set! self '-value result)
				result))))

;;; Klondike
(define-class <nn-network-klondike> (<nn-network>)
  )

(define-method (success-probability (self <nn-network-klondike>))
  (value-get (car (nodes-get (layer-output-get self)))))

(define-class <nn-layer-input-klondike> (<nn-layer>)
  (-nodes-slots #:init-keyword #:nodes-slots #:getter nodes-slots-get)
  (-node-deal #:init-keyword #:node-deal #:getter node-deal-get)
  )

(define-method (nn-make-input-layer (game <game>))
  (let ((node-per-slot
		 (let f ((out '()) (lislot (slots-all game)) (i 0))
		   (cond ((null? lislot) (reverse out))
				 (else (f (cons (make <nn-node-input-move>
								  #:id (format #f "i~a" i))
								out)
						  (cdr lislot)
						  (1+ i))))))
		)
	(cons (make <nn-node-input-move> #:id "i-deal") node-per-slot)
	(make <nn-layer-input-klondike>
	  #:nodes node-per-slot
	  #:node-deal (car node-per-slot)
	  #:nodes-slots (cdr node-per-slot))
	)
  )

(define-method (nn-network-klondike (game <game>))
  (nn-network-fully-connected
   <nn-network-klondike>
   (nn-make-input-layer game)
   1
   6
   (list (make <nn-node-output-real> #:id "output" #:links-in '())))
  )

(define-method (nn-network-set! (network <nn-network-klondike>) (move <move-cards>) (game <game>))
  (clear! (layer-input-get network))
  (move-set! (list-ref (nodes-slots-get (layer-input-get network))
					   (1- (slot-source-get move)))
			 move)
  )

(define-method (nn-network-set! (network <nn-network-klondike>) (move <move-deal>) (game <game>))
  (clear! (layer-input-get network))
  (move-set! (node-deal-get (layer-input-get network)) move)
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

(define (get-hint)
  (delayed-call report)
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

(set-features droppable-feature dealable-feature)

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint get-options apply-options timeout droppable? dealable?)
