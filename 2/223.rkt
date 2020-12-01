;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |223|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; Constants

(define WIDTH 10)   ; # of blocks horizontally
(define HEIGHT 35) ; # of blocks vertically 
(define SIZE 10)
(define HALF-BLOCK (/ SIZE 2))
(define MTSCENE (empty-scene (* SIZE WIDTH) (* SIZE HEIGHT)))
(define LAST-BLOCK-RIGHT (- WIDTH 1))
(define LAST-BLOCK-BOTTOM (- HEIGHT 1))
(define RATE 0.2)

(define BLOCK
  (overlay (square (- SIZE 1) "solid" "red")
           (square SIZE "outline" "black")))

; Data definitions

(define-struct block [x y])
; A Block is a structure:
; (make-block N N)
; interp. (make-block x y) depicts a block whose left corner is (* x SIZE) pixels from the left and
; (* y SIZE) pixels from the top

(define BLOCK-TOP-LEFT (make-block 0 0))
(define BLOCK-TOP-RIGHT (make-block LAST-BLOCK-RIGHT 0))
(define BLOCK-BOTTOM-LEFT (make-block 0 (- HEIGHT 1)))
(define BLOCK-BOTTOM-RIGHT (make-block LAST-BLOCK-RIGHT (- HEIGHT 1)))
(define BLOCK-DROPPING1 (make-block 1 1))
(define BLOCK-DROPPING2 (make-block 8 5))
(define BLOCK-LANDED (make-block 8 (- HEIGHT 1)))
(define BLOCK-ON-BLOCK (make-block 0 (- HEIGHT 2)))
(define INIT-BLOCK-X (/ WIDTH 2))
(define INIT-BLOCK-Y 0)
(define LAST-BLOCK-Y (- HEIGHT 1))
(define FIRST-BLOCK-X 0)
(define FIRST-BLOCK-Y 0)
(define LAST-BLOCK-X LAST-BLOCK-RIGHT)
(define INIT-BLOCK (make-block INIT-BLOCK-X INIT-BLOCK-Y))

(define (fn-for-block b)
  (... (block-x b)
       (block-y b)))

; Template rules used:
; - compound: 2 fields
; - atomic non-distinct: x is Number
; - atomic non-distinct: y is Number

; A Landscape is one of:
; - '()
; - (cons Block Landscape)

(define LANDSCAPE0 (cons (make-block 5 (- HEIGHT 1)) (cons (make-block 8 (- HEIGHT 1)) empty)))
(define LANDSCAPE1 (cons (make-block 5 LAST-BLOCK-BOTTOM) (cons (make-block 8 (- HEIGHT 2))  (cons (make-block 8 LAST-BLOCK-BOTTOM) empty))))

(define (fn-for-landscape l)
  (cond [(empty? l) (...)]
        [else
         (... (fn-for-block (first l))         ;Block
              (fn-for-landscape (rest l)))]))  ;Landscape

; Template rules used:
; - one of: 2 cases
; - atomic distinct: empty
; - compound: (cons Block Landscape)
; - reference: (first l) is Block
; - self-reference: (rest l) is Landscape

(define-struct tetris [block landscape])
; A Tetris is a structure:
; (make-tetris Block Landscape)
; interp. (make-tetris b0 (list b1 b2 ...)) means b0 is the
; dropping block while b1, b2 and ... are the resting

(define TETRIS0-DROP (make-tetris BLOCK-DROPPING1 LANDSCAPE0))
(define TETRIS-INIT (make-tetris INIT-BLOCK empty))

(define (fn-for-tetris t)
  (... (fn-for-block (tetris-block t))          ;Block
       (fn-for-lanscape (tetris-landscape t)))) ;Landscape

; Template rules used:
; - compound: 2 fields
; - reference: (tetris-block t) is Block
; - reference: (tetris-landscape t) is Landscape

; Functions

; Number -> Tetris
; runs tetris game
; start with (tetris-main RATE)
(define (tetris-main rate)
  (big-bang TETRIS-INIT
    [on-tick tetris-next rate]
    [to-draw tetris-render]
    [on-key  control-block]
    [stop-when resting-touches-top?]))   

; Tetris -> Tetris
; generates the next tetris state.
; either continues moving the block down or generates a new one if landed
(check-expect (tetris-next TETRIS-INIT) ; will not land
              (make-tetris (make-block INIT-BLOCK-X (+ INIT-BLOCK-Y 1)) empty))   
(check-expect (tetris-next (make-tetris (make-block INIT-BLOCK-X (- LAST-BLOCK-Y 1)) empty))  ; will land on floor. Not right-most column 
              (make-tetris (make-block (+ INIT-BLOCK-X 1) INIT-BLOCK-Y) (cons (make-block INIT-BLOCK-X LAST-BLOCK-Y) empty)))
(check-expect (tetris-next (make-tetris (make-block LAST-BLOCK-X (- LAST-BLOCK-Y 1)) empty))  ; will land on floor. right-most column 
              (make-tetris (make-block FIRST-BLOCK-X FIRST-BLOCK-Y) (cons BLOCK-BOTTOM-RIGHT empty)))
(check-expect (tetris-next (make-tetris (make-block 6 (- LAST-BLOCK-Y 2)) (cons (make-block 6 (- LAST-BLOCK-Y 1)) (cons (make-block 6 LAST-BLOCK-Y) empty)))) ; will land on block. Not right-most column
              (make-tetris (make-block (+ 6 1) INIT-BLOCK-Y) (cons (make-block 6 (- LAST-BLOCK-Y 2)) (cons (make-block 6 (- LAST-BLOCK-Y 1)) (cons (make-block 6 LAST-BLOCK-Y) empty))))) 
(check-expect (tetris-next (make-tetris (make-block 6 (- LAST-BLOCK-Y 1)) (cons (make-block 6 LAST-BLOCK-Y) empty))) ; will land on block and next is floor. Not right-most column
              (make-tetris (make-block (+ 6 1) INIT-BLOCK-Y) (cons (make-block 6 (- LAST-BLOCK-Y 1)) (cons (make-block 6 LAST-BLOCK-Y) empty)))) 
(check-expect (tetris-next (make-tetris (make-block LAST-BLOCK-X (- LAST-BLOCK-Y 1)) (cons (make-block LAST-BLOCK-X LAST-BLOCK-Y) empty))) ; will land on block and next is floor. Right-most column
              (make-tetris (make-block FIRST-BLOCK-X FIRST-BLOCK-Y) (cons (make-block LAST-BLOCK-X (- LAST-BLOCK-Y 1)) (cons (make-block LAST-BLOCK-X LAST-BLOCK-Y) empty)))) 

;(define (tetris-next t) t) ;stub

(define (tetris-next t)
  (if (will-land? t)
      (tetris-next-will-land t)
      (make-tetris (drop-block (tetris-block t)) (tetris-landscape t))))

; Tetris KeyEvent -> Tetris
; allows user to control the horizontal movement of the dropping block.
; When "left" is pressed, the block shifts one column to the left unless already at left most column or resting blocks to its left
; Same for right
(check-expect (control-block (make-tetris (make-block 1 0) empty) "left") ;left
              (make-tetris (make-block 0 0) empty))
(check-expect (control-block (make-tetris (make-block 0 0) empty) "left") ;left, already left-most
              (make-tetris (make-block 0 0) empty))
(check-expect (control-block (make-tetris (make-block 6 4) (cons (make-block 5 4)(cons (make-block 5 5) empty))) "left") ;left, stack to left
              (make-tetris (make-block 6 4) (cons (make-block 5 4)(cons (make-block 5 5) empty))))
(check-expect (control-block (make-tetris (make-block 1 0) empty) "right") ;right
              (make-tetris (make-block 2 0) empty))
(check-expect (control-block (make-tetris (make-block LAST-BLOCK-X 0) empty) "right") ;right, already right-most
              (make-tetris (make-block LAST-BLOCK-X 0) empty))
(check-expect (control-block (make-tetris (make-block 4 4) (cons (make-block 5 4)(cons (make-block 5 5) empty))) "right") ;righ, stack to right
              (make-tetris (make-block 4 4) (cons (make-block 5 4)(cons (make-block 5 5) empty))))

;(define (control-block t ke) t) ;stub

(define (control-block t ke)
  (cond [(key=? ke "left") (check-and-move-block-left t)]
        [(key=? ke "right") (check-and-move-block-right t)]
        [else t]))

; Tetris -> Tetris
; moves block one column to the left unless already at left most column or resting blocks to its left
(check-expect (check-and-move-block-left (make-tetris (make-block 5 5) empty)) ;move left
              (make-tetris (make-block (- 5 1) 5) empty))
(check-expect (check-and-move-block-left (make-tetris (make-block 0 5) empty)) ;don't move left. already left most
              (make-tetris (make-block 0 5) empty))
(check-expect (check-and-move-block-left (make-tetris (make-block 6 4) (list (make-block 5 4) (make-block 5 5)))) ;don't move left. resting blocks to left
              (make-tetris (make-block 6 4) (list (make-block 5 4) (make-block 5 5)))) 

;(define (check-and-move-block-left t) t) ;stub

(define (check-and-move-block-left t)
  (if (or (block-already-left-most? (tetris-block t))         
          (resting-block-to-the-left? (tetris-block t) (tetris-landscape t)))
      t
      (make-tetris (move-block-left (tetris-block t)) (tetris-landscape t))))

; Block -> Boolean
; produces true if block is at left most column
(check-expect (block-already-left-most? (make-block 0 5)) true)  ;already left most
(check-expect (block-already-left-most? (make-block 1 5)) false)  ;not left most            

;(define (block-already-left-most? b) false) ;stub

(define (block-already-left-most? b)
  (= (block-x b)
     0))

; Block Landscape -> Boolean
; produces true if there is a resting block to the left of b
(check-expect (resting-block-to-the-left? (make-block 6 4) (list (make-block 5 4) (make-block 5 5))) true)
(check-expect (resting-block-to-the-left? (make-block 7 4) (list (make-block 5 4) (make-block 5 5))) false)

;(define (resting-block-to-the-left? b l) false) ;stub

(define (resting-block-to-the-left? b l)
  (member? (move-block-left b) l))

; Block -> Block
; moves b one column to the left
(check-expect (move-block-left (make-block 6 4)) (make-block 5 4))

;(define (move-block-left b) b) ;stub

(define (move-block-left b)
  (make-block (- (block-x b) 1)
              (block-y b)))

; Tetris -> Tetris
; moves block one column to the right unless already at right most column or resting blocks to its right
(check-expect (check-and-move-block-right (make-tetris (make-block 5 5) empty)) ;move right
              (make-tetris (make-block (+ 5 1) 5) empty))
(check-expect (check-and-move-block-right (make-tetris (make-block LAST-BLOCK-X 5) empty)) ;don't move right. already right most
              (make-tetris (make-block LAST-BLOCK-X 5) empty))
(check-expect (check-and-move-block-right (make-tetris (make-block 4 4) (list (make-block 5 4) (make-block 5 5)))) ;don't move right. resting blocks to right
              (make-tetris (make-block 4 4) (list (make-block 5 4) (make-block 5 5))))

;(define (check-and-move-block-right t) t) ;stub

(define (check-and-move-block-right t)
  (if (or (block-already-right-most? (tetris-block t))         
          (resting-block-to-the-right? (tetris-block t) (tetris-landscape t)))
      t
      (make-tetris (move-block-right (tetris-block t)) (tetris-landscape t))))

; Block -> Boolean
; produces true if block is at right most column
(check-expect (block-already-right-most? (make-block LAST-BLOCK-X 5)) true)  ;already right most
(check-expect (block-already-right-most? (make-block 1 5)) false)  ;not right most            

;(define (block-already-right-most? b) false) ;stub

(define (block-already-right-most? b)
  (= (block-x b)
     LAST-BLOCK-X))

; Block Landscape -> Boolean
; produces true if there is a resting block to the right of b
(check-expect (resting-block-to-the-right? (make-block 4 4) (list (make-block 5 4) (make-block 5 5))) true)
(check-expect (resting-block-to-the-right? (make-block 3 4) (list (make-block 5 4) (make-block 5 5))) false)

;(define (resting-block-to-the-right? b l) false) ;stub

(define (resting-block-to-the-right? b l)
  (member? (move-block-right b) l))

; Block -> Block
; moves b one column to the right
(check-expect (move-block-right (make-block 6 4)) (make-block 7 4))

;(define (move-block-right b) b) ;stub

(define (move-block-right b)
  (make-block (+ (block-x b) 1)
              (block-y b)))

; Tetris -> Boolean
; produces true if block will land after dro,mnnpping. Either by hitting the floor or overlapping with resting block
(check-expect (will-land? TETRIS-INIT) false) ; will not land. No resting blocks
(check-expect (will-land? (make-tetris (make-block INIT-BLOCK-X (- LAST-BLOCK-Y 1)) empty)) true) ; will land on floor
(check-expect (will-land? (make-tetris (make-block 6 (- LAST-BLOCK-Y 1))
                                       (cons (make-block 3 LAST-BLOCK-Y) (cons (make-block 6 LAST-BLOCK-Y) empty)))) true) ; will land on block
(check-expect (will-land? (make-tetris (make-block 5 1)
                                       (cons (make-block 3 LAST-BLOCK-Y) (cons (make-block 6 LAST-BLOCK-Y) empty)))) false) ; will not land. Resting blocks
              
;(define (will-land? t) false) ;stub

(define (will-land? t)
  (or (on-floor? (drop-block (tetris-block t)))          
      (member (drop-block (tetris-block t)) (tetris-landscape t))))

; Block -> Boolean
; produces true if block is on floor
(check-expect (on-floor? (make-block 1 (- LAST-BLOCK-Y 1))) false)
(check-expect (on-floor? (make-block 1 LAST-BLOCK-Y)) true)

;(define (on-floor? b) false) ;stub

(define (on-floor? b)
  (= (block-y b) LAST-BLOCK-Y))

; Tetris -> Tetris
; Produces the next tetris state following a block that will land after dropping. Either will land on floor or land on block
(check-expect (tetris-next-will-land (make-tetris (make-block INIT-BLOCK-X (- LAST-BLOCK-Y 1)) empty))  ; will land on floor. Not right-most column 
              (make-tetris (make-block (+ INIT-BLOCK-X 1) INIT-BLOCK-Y) (cons (make-block INIT-BLOCK-X LAST-BLOCK-Y) empty)))
(check-expect (tetris-next-will-land (make-tetris (make-block LAST-BLOCK-X (- LAST-BLOCK-Y 1)) empty))  ; will land on floor. right-most column 
              (make-tetris (make-block FIRST-BLOCK-X FIRST-BLOCK-Y) (cons BLOCK-BOTTOM-RIGHT empty)))
(check-expect (tetris-next-will-land (make-tetris (make-block 6 (- LAST-BLOCK-Y 1)) (cons (make-block 6 LAST-BLOCK-Y) empty))) ; will land on block. Not right-most column
              (make-tetris (make-block (+ 6 1) INIT-BLOCK-Y) (cons (make-block 6 (- LAST-BLOCK-Y 1)) (cons (make-block 6 LAST-BLOCK-Y) empty))))
(check-expect (tetris-next-will-land (make-tetris (make-block LAST-BLOCK-X (- LAST-BLOCK-Y 1)) (cons (make-block LAST-BLOCK-X LAST-BLOCK-Y) empty))) ; will land on block. Right-most column
              (make-tetris (make-block FIRST-BLOCK-X FIRST-BLOCK-Y) (cons (make-block LAST-BLOCK-X (- LAST-BLOCK-Y 1)) (cons (make-block LAST-BLOCK-X LAST-BLOCK-Y) empty))))

;(define (tetris-next-will-land t) t) ;stub

(define (tetris-next-will-land t)
  (make-tetris (create-new-block (tetris-block t)) 
               (cons (if (member (drop-block (tetris-block t)) (tetris-landscape t))
                         (tetris-block t)
                         (drop-block (tetris-block t)))
                     (tetris-landscape t))))

; Block -> Block
; creates the new block at INIT-BLOCK-X on the column to the right of the current one.
; If the current block is already in the right-most column use the left-most one
(check-expect (create-new-block (make-block 0 LAST-BLOCK-Y)) (make-block (+ 0 1) INIT-BLOCK-Y)) ; not right-most
(check-expect (create-new-block (make-block (- LAST-BLOCK-X 1) LAST-BLOCK-Y)) (make-block LAST-BLOCK-X INIT-BLOCK-Y)) ; almost right-most
(check-expect (create-new-block (make-block LAST-BLOCK-X LAST-BLOCK-Y)) (make-block FIRST-BLOCK-X INIT-BLOCK-Y)) ; right-most

;(define (create-new-block b) b) ;stub

(define (create-new-block b)
  (make-block
   (if (= (block-x b) LAST-BLOCK-X)
       FIRST-BLOCK-X
       (+ (block-x b) 1))
   INIT-BLOCK-Y))

; Block -> Block
; drops a block by one position
(check-expect (drop-block (make-block 1 1)) (make-block 1 2))

;(define (drop-block b) b) ;stub

(define (drop-block b)
  (make-block (block-x b)
              (+ (block-y b) 1)))

; Tetris -> Image
; Renders the tetris instance
(check-expect (tetris-render (make-tetris BLOCK-TOP-LEFT empty))               ; top left
              (place-image BLOCK (+ (* 0 SIZE) HALF-BLOCK) (+ (* 0 SIZE) HALF-BLOCK) MTSCENE))
(check-expect (tetris-render (make-tetris BLOCK-TOP-RIGHT empty))              ; top right
              (place-image BLOCK (+ (* LAST-BLOCK-RIGHT SIZE) HALF-BLOCK) (+ (* 0 SIZE) HALF-BLOCK)  MTSCENE))
(check-expect (tetris-render (make-tetris BLOCK-BOTTOM-LEFT empty))            ; bottom left
              (place-image BLOCK (+ (* 0 SIZE) HALF-BLOCK) (+ (* LAST-BLOCK-BOTTOM SIZE) HALF-BLOCK) MTSCENE))
(check-expect (tetris-render (make-tetris BLOCK-BOTTOM-RIGHT empty))              ; bottom right
              (place-image BLOCK (+ (* LAST-BLOCK-RIGHT SIZE) HALF-BLOCK) (+ (* LAST-BLOCK-BOTTOM SIZE) HALF-BLOCK)  MTSCENE))
(check-expect (tetris-render (make-tetris BLOCK-DROPPING2 empty))              ; somewhere in the scene, not edges
              (place-image BLOCK (+ (* 8 SIZE) HALF-BLOCK) (+ (* 5 SIZE) HALF-BLOCK)  MTSCENE))
(check-expect (tetris-render (make-tetris BLOCK-DROPPING2 LANDSCAPE0))              ; A couple of resting blocks 
              (place-image BLOCK (+ (* 8 SIZE) HALF-BLOCK) (+ (* 5 SIZE) HALF-BLOCK)
                           (place-image BLOCK (+ (* 5 SIZE) HALF-BLOCK) (+ (* LAST-BLOCK-BOTTOM SIZE) HALF-BLOCK) 
                                        (place-image BLOCK (+ (* 8 SIZE) HALF-BLOCK) (+ (* LAST-BLOCK-BOTTOM SIZE) HALF-BLOCK) MTSCENE))))
(check-expect (tetris-render (make-tetris BLOCK-DROPPING2 LANDSCAPE1))              ; A couple of resting blocks with a stack
              (place-image BLOCK (+ (* 8 SIZE) HALF-BLOCK) (+ (* 5 SIZE) HALF-BLOCK)
                           (place-image BLOCK (+ (* 5 SIZE) HALF-BLOCK) (+ (* LAST-BLOCK-BOTTOM SIZE) HALF-BLOCK) 
                                        (place-image BLOCK (+ (* 8 SIZE) HALF-BLOCK) (+ (* (- HEIGHT 2) SIZE) HALF-BLOCK) 
                                                     (place-image BLOCK (+ (* 8 SIZE) HALF-BLOCK) (+ (* LAST-BLOCK-BOTTOM SIZE) HALF-BLOCK) MTSCENE)))))

;(define (tetris-render t) MTSCENE) ;stub

(define (tetris-render t)
  (render-block (tetris-block t)          
                (render-landscape (tetris-landscape t)))) 

; Block -> Image
; Renders a block on img
(check-expect (render-block BLOCK-TOP-LEFT MTSCENE)              ; top left
              (place-image BLOCK (+ (* 0 SIZE) HALF-BLOCK) (+ (* 0 SIZE) HALF-BLOCK) MTSCENE))
(check-expect (render-block BLOCK-TOP-RIGHT MTSCENE)             ; top right
              (place-image BLOCK (+ (* LAST-BLOCK-RIGHT SIZE) HALF-BLOCK) (+ (* 0 SIZE) HALF-BLOCK)  MTSCENE))
(check-expect (render-block BLOCK-BOTTOM-LEFT MTSCENE)          ; bottom left
              (place-image BLOCK (+ (* 0 SIZE) HALF-BLOCK) (+ (* LAST-BLOCK-BOTTOM SIZE) HALF-BLOCK) MTSCENE))
(check-expect (render-block BLOCK-BOTTOM-RIGHT MTSCENE)          ; bottom right
              (place-image BLOCK (+ (* LAST-BLOCK-RIGHT SIZE) HALF-BLOCK) (+ (* LAST-BLOCK-BOTTOM SIZE) HALF-BLOCK)  MTSCENE))
(check-expect (render-block BLOCK-DROPPING2 MTSCENE)              ; somewhere in the scene, not edges
              (place-image BLOCK (+ (* 8 SIZE) HALF-BLOCK) (+ (* 5 SIZE) HALF-BLOCK)  MTSCENE))
              
;(define (render-block b img) img) ;stub

(define (render-block b img)
  (place-image BLOCK
               (block-physical-x b)
               (block-physical-y b)
               img))

; Block -> Number
; produces physical x position of b
(check-expect (block-physical-x (make-block 0 7)) (+ (* 0 SIZE) HALF-BLOCK))
(check-expect (block-physical-x (make-block 8 5)) (+ (* 8 SIZE) HALF-BLOCK))

;(define (block-physical-x b) 0) ;stub

(define (block-physical-x b)
  (block-logical-to-physical-pos (block-x b)))

; Number -> Number
; translates block's logical to physical position
(check-expect (block-logical-to-physical-pos 0) (+ (* 0 SIZE) HALF-BLOCK))
(check-expect (block-logical-to-physical-pos 8) (+ (* 8 SIZE) HALF-BLOCK))

;(define (block-logical-to-physical-pos n) n) ;stub

(define (block-logical-to-physical-pos n)
  (+ (* n SIZE) HALF-BLOCK)) 

; Block -> Number
; produces physical y position of b
(check-expect (block-physical-y (make-block 6 0)) (+ (* 0 SIZE) HALF-BLOCK))
(check-expect (block-physical-y (make-block 8 5)) (+ (* 5 SIZE) HALF-BLOCK))

;(define (block-physical-y b) 0) ;stub

(define (block-physical-y b)
  (block-logical-to-physical-pos (block-y b)))

; Landscape -> Image
; Renders landscape on MTSCENE
(check-expect (render-landscape empty) MTSCENE)
(check-expect (render-landscape LANDSCAPE0)
              (place-image BLOCK (+ (* 5 SIZE) HALF-BLOCK) (+ (* LAST-BLOCK-BOTTOM SIZE) HALF-BLOCK) 
                           (place-image BLOCK (+ (* 8 SIZE) HALF-BLOCK) (+ (* LAST-BLOCK-BOTTOM SIZE) HALF-BLOCK) MTSCENE)))

;(define (render-landscape l) MTSCENE) ;stub

(define (render-landscape l)
  (cond [(empty? l) MTSCENE]
        [else
         (render-block (first l)         
                       (render-landscape (rest l)))]))

; Tetris -> Boolean
; produces true if one of the resting columns contains enough blocks to “touch” the top of the canvas
(check-expect (resting-touches-top? (make-tetris (make-block 1 1) empty)) false)
(check-expect (resting-touches-top? (make-tetris (make-block 1 1) (list (make-block (- LAST-BLOCK-Y 1) 5) (make-block LAST-BLOCK-Y 5)))) false)
(check-expect (resting-touches-top? (make-tetris (make-block 1 1) (make-column 5 HEIGHT))) true)

;(define (resting-touches-top? t) false) ;stub

(define (resting-touches-top? t)
  (has-block-at-top? (tetris-landscape t)))

; Landscape -> Boolean
; produces true if one of the blocks is at the top of the canvas
(check-expect (has-block-at-top? empty) false)
(check-expect (has-block-at-top? (make-column 5 HEIGHT)) true)

;(define (has-block-at-top? l) false) ;stub

(define (has-block-at-top? l)
  (cond [(empty? l) false]
        [else
         (if (block-at-top? (first l))
             true
             (has-block-at-top? (rest l)))]))

; Block -> Boolean
; produces true if block is at top row of canvas
(check-expect (block-at-top? (make-block 5 5)) false)
(check-expect (block-at-top? (make-block 5 0)) true)

;(define (block-at-top? b) false) ;stub

(define (block-at-top? b)
  (= 0
     (block-y b)))

; Natural Natural -> Landscape
; creates a column of n blocks at column c
(check-expect (make-column 0 0) empty)
(check-expect (make-column 0 3) (list (make-block 0 2)
                                      (make-block 0 1)
                                      (make-block 0 0)))

;(define (make-column n h) empty) ;stub

; template from ??? (natural?)

(define (make-column n h)
  (cond [(zero? h) empty]
        [else
         (cons (make-block n (- h 1))
               (make-column n (- h 1)))]))