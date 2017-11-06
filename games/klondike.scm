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

(define-module (aisleriot klondike)
  #:use-module (ice-9 format)
  #:use-module (ice-9 local-eval)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1)
  #:use-module (rnrs base)
  #:use-module (rnrs bytevectors)
  #:use-module (oop goops)
  #:use-module (aisleriot interface)
  #:use-module (aisleriot api)
  #:use-module (aisleriot serialize)
  #:use-module (aisleriot ga)
  #:use-module (aisleriot nn)
  #:use-module (aisleriot permute)
  
  #:duplicates (merge-generics replace warn-override-core warn last)
  )

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


;;; A potential game move. It need not be valid.
(define-class <move> (<object>)
  (-game #:init-keyword #:game)
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

;;; A playing card
(define-class <card> (<object>)
  (-card #:init-keyword #:card #:getter legacy-get)
  )

(define-method (serialize (self <card>))
  (list (next-method) (serialize-slots self '-card)))

(define-method (visible? (self <card>))
  (is-visible? (slot-ref self '-card)))

(define-method (suit-get (self <card>))
  (get-suit (slot-ref self '-card)))

(define-method (color-get (self <card>))
  (get-color (slot-ref self '-card)))

(define-method (inspect (self <card>))
  (format #f "#<card ~a>" (legacy-get self)))

;; A slot in which cards can be placed
(define-class <slot-card> (<object>)
  (-id #:init-keyword #:id #:getter id-get)
  )

(define-method (serialize (self <slot-card>))
  (list (next-method) (serialize-slots self '-id)))

(define-method (equal? (a <slot-card>) (b <slot-card>))
  (equal? (id-get a) (id-get b)))

(define-method (cards-get (self <slot-card>))
  (map (lambda (c) (make <card> #:card c)) (get-cards (id-get self))))

(define-method (empty? (self <slot-card>))
  (empty-slot? (id-get self)))

(define-method (expanded? (self <slot-card>))
  (is-slot-expanded? (id-get self)))

(define-method (card-top-make-visible (self <slot-card>))
  (make-visible-top-card (id-get self)))

(define-method (inspect (slot <slot-card>))
  (format #f "#<slot id: ~a>" (id-get slot)))

(define-class <game> (<object>)
  (-tableau #:init-keyword #:tableau #:getter tableau-get)
  (-foundation #:init-keyword #:foundation #:getter foundation-get)
  (-waste #:init-keyword #:waste #:getter waste-get)
  )

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

(define-method (nn-encode (self <slot-card>))
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

;;; Klondike
(define-class <nn-network-klondike> (<nn-network>)
  )

(define-method (fitness-eval (self <nn-network-klondike>) (game <game>) (steps-elapsed <integer>) (steps-max <integer>))
  (apply + (- steps-max steps-elapsed)
		 (map (lambda (slot) (* 5 (length (cards-get slot)))) (foundation-get game)))
  )

(define-method (success-probability (network <nn-network-klondike>) (move <move>) (game <game>))
  (let ((nn-new (nn-network-for network move game)))
  ;;(format #t "Try move on network: ~a\n" (inspect move))
  ;;(format #t "\nNETWORK:\n~a\n\n" (inspect nn-new))
  ;; If the network ping-pongs a lot, then the cache can speed up the inevitable
	(let* ((cache-key (cache-key nn-new))
		   (cached (cached-output network cache-key)))
	  (if cached
		  (begin
;;;			(format #t "CACHE HIT ~a ~a\n" cached (inspect move))
			(cdr cached)
			)
		  (begin
			(let ((outcome (value-get (car (nodes-get (layer-output-get nn-new))) nn-new)))
			  (cache-output-value! network cache-key outcome)
			  outcome
			  )
			))
	  )
	)
  )

(define-class <nn-layer-input-klondike> (<nn-layer-input>)
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

(define-method (nn-network-for (network <nn-network-klondike>) (move <move>) (game <game>))
  (let ((network-new-input (shallow-clone network))
		(nodes-new (map (lambda (n) (make <nn-node-input-real> #:id (id-get n)))
						(nodes-get (layer-input-get network)))))
	(layer-input-set! network-new-input (make <nn-layer-input-klondike> #:nodes nodes-new))
	;; Set gamestate nodes
	(for-each value-set!
			  (nodes-gamestate-get (layer-input-get network-new-input))
			  (append-map nn-encode (sort (slots-all game) (lambda (slot0 slot1) (>= (id-get slot0) (id-get slot1)))))
			  )
;;;	(format #t "~a\n" (map value-get (nodes-get (layer-input-get network-new-input))))
	network-new-input
	)
  )

(define-method (nn-network-for (network <nn-network-klondike>) (move <move-cards>) (game <game>))
  (let ((result (next-method)))
	;; Set card move
	(for-each value-set!
			  (nodes-movecards-get (layer-input-get result))
			  (nn-encode move)
			  )
;;;	(format #t "Setting move ~a to node ~a\n" (inspect move) (inspect target-node network))
;;;  (format #t "M: ~a\n" (map value-get (nodes-get (layer-input-get result))))
	result
	)
  )

(define-method (nn-network-for (network <nn-network-klondike>) (move <move-deal>) (game <game>))
  (let ((result (next-method)))
	(value-set! (node-deal-get (layer-input-get result)) (nn-encode move))
;;;  (format #t "D: ~a\n" (map value-get (nodes-get (layer-input-get result))))
	result
	)
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
        (idle-call autoplay-foundations-tail)
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

(define (game-step genome game)
  (let* ((gs (make <game-state> #:game game))
		 (network (subject-get genome)))
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
					 (map (lambda (m) (cons m (success-probability network m game)))
						  (reverse moves))))
				(let ((by-score (sort evals (lambda (ala alb) (> (cdr ala) (cdr alb))))))
;;;				(for-each (lambda (al) (format #t "Success chance for move ~a: ~a\n" (inspect (car al)) (cdr al))) by-score)
				  (execute gs (caar by-score))
				  )
				)
			  )
	  )
	)
  )
			
(define (automate game automation)
  (let ((genome (genome-get automation))
		 (seed (seed-get automation)))
	(idle-call
	 (lambda () (ga-evolve game
						   '()
						   (list genome)
						   '()
						   (list seed)
						   0
						   0
						   '()
						   meta-new-game-with-seed
						   game-won
						   game-step)))
	)
  )

(define (autoplay)
  (display "Autoplay begins\n")
  (set! *random-state* (random-state-from-platform))
  (let ((game (make <game>
				#:tableau (map (lambda (id) (make <slot-card> #:id id)) tableau)
				#:foundation (map (lambda (id) (make <slot-card> #:id id)) foundation)
				#:waste (make <slot-card> #:id waste)
				)))
	(if (ga-is-automation)
		(ga-automation-run (the-environment) (lambda (auto) (automate game auto)))
		(idle-call
		 (lambda ()
		  (let* ((genomes
				  (par-map
				   (lambda (n)
					 (make <ga-genome> #:subject (randomize! (nn-network-klondike game))))
				   (iota ga-population-size))))
			(ga-evolve game
					   '()
					   genomes
					   '()
					   (iota ga-training-set-size)
					   0
					   0
					   '()
					   meta-new-game-with-seed
					   game-won
					   game-step))
		  ))
		)
	)
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
