;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |220|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define WIDTH 10)   ; # of blocks horizontally
(define HEIGHT 35) ; # of blocks vertically 
(define SIZE 10)
(define HALF-BLOCK (/ SIZE 2))
(define MTSCENE (empty-scene (* SIZE WIDTH) (* SIZE HEIGHT)))
(define LAST-BLOCK-RIGHT (- WIDTH 1))
(define LAST-BLOCK-BOTTOM (- HEIGHT 1))

(define BLOCK
  (overlay (square (- SIZE 1) "solid" "red")
           (square SIZE "outline" "black")))


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

(define (fn-for-tetris t)
  (... (fn-for-block (tetris-block t))          ;Block
       (fn-for-lanscape (tetris-landscape t)))) ;Landscape

; Template rules used:
; - compound: 2 fields
; - reference: (tetris-block t) is Block
; - reference: (tetris-landscape t) is Landscape

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





