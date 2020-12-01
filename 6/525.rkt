;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |525|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define DEFAULT-THRESHOLD 10)
(define SPARSER-THRESHOLD 20)
(define DENSER-THRESHOLD 5)
(define THRESHOLD DEFAULT-THRESHOLD)

(define MT (empty-scene 400 400))
(define A (make-posn 200  50))
(define B (make-posn  27 350))
(define C (make-posn 373 350))

; Image Posn Posn Posn -> Image 
; generative adds the triangle (a, b, c) to s, 
; subdivides it into three triangles by taking the 
; midpoints of its sides; stop if (a, b, c) is too small
; accumulator the function accumulates the triangles scene0

(define (add-sierpinski scene0 a b c)
  (cond
    [(too-small? a b c) scene0]
    [else
     (local
       ((define scene1 (add-triangle scene0 a b c))
        (define mid-a-b (mid-point a b))
        (define mid-b-c (mid-point b c))
        (define mid-c-a (mid-point c a))
        (define scene2
          (add-sierpinski scene1 a mid-a-b mid-c-a))
        (define scene3
          (add-sierpinski scene2 b mid-b-c mid-a-b)))
       ; —IN—
       (add-sierpinski scene3 c mid-c-a mid-b-c))]))

; Image Posn Posn Posn -> Image 
; adds the black triangle a, b, c to scene
; point (0, 0) is top left of scene and y grows down
(check-expect (add-triangle (rectangle 40 40 "solid" "gray")
                            (make-posn 0 0)
                            (make-posn 11 0)
                            (make-posn 5.5 (* 5.5 (sqrt 3))))
              (scene+line
               (scene+line
                (scene+line (rectangle 40 40 "solid" "gray") 0 0 11 0 "black")
                11 0 5.5 (* 5.5 (sqrt 3)) "black")
               5.5 (* 5.5 (sqrt 3)) 0 0 "black"))
(check-expect (add-triangle (rectangle 40 40 "solid" "gray")
                            (make-posn 5 10)
                            (make-posn 16 10)
                            (make-posn 10.5 (+ 10 (* 5.5 (sqrt 3)))))
              (scene+line
               (scene+line
                (scene+line (rectangle 40 40 "solid" "gray") 5 10 16 10 "black")
                16 10 10.5 (+ 10 (* 5.5 (sqrt 3))) "black")
               10.5 (+ 10 (* 5.5 (sqrt 3))) 5 10 "black"))

;(define (add-triangle scene a b c) scene) ;stub

(define (add-triangle scene a b c)
  (scene+line
   (scene+line
    (scene+line scene (posn-x a) (posn-y a) (posn-x b)  (posn-y b) "black")
    (posn-x b)  (posn-y b) (posn-x c) (posn-y c) "black")
   (posn-x c)  (posn-y c) (posn-x a) (posn-y a) "black"))

 
; Posn Posn Posn -> Boolean 
; is the triangle a, b, c too small to be divided.
; is the distance between two points below THRESHOLD
(check-expect (too-small? (make-posn 1 0) (make-posn 3 0) (make-posn 2 (* 1 (sqrt 3)))) #true)         ;distance < THRESHOLD
(check-expect (too-small? (make-posn 0 0) (make-posn 9 0) (make-posn 4.5 (* 4.5 (sqrt 3)))) #true)     ;distance < THRESHOLD
(check-expect (too-small? (make-posn 0 0) (make-posn 10 0) (make-posn 5 (* 5 (sqrt 3)))) #false)       ;distance = THRESHOLD
(check-expect (too-small? (make-posn 0 0) (make-posn 11 0) (make-posn 5.5 (* 5.5 (sqrt 3)))) #false)   ;distance > THRESHOLD


;(define (too-small? a b c) #false) ;stub

(define (too-small? a b c)
  (< (distance a b)
     THRESHOLD))

; Posn Posn -> Number
; produces distance between p1 and p2
(check-within (distance (make-posn 1 0) (make-posn 3 0))
              (sqrt (+ (expt (- 1 3) 2) (expt (- 0 0) 2)))
              0.1)
(check-within (distance (make-posn 10 0) (make-posn 5 (* 5 (sqrt 3))))
              (sqrt (+ (expt (- 10 5) 2) (expt (- 0 (* 5 (sqrt 3))) 2)))
              0.1)

; (define (distance p1 p2) 0) ;stub

(define (distance p1 p2)
  (sqrt (+ (expt (- (posn-x p1) (posn-x p2)) 2) (expt (- (posn-y p1) (posn-y p2)) 2))))
 
; Posn Posn -> Posn 
; determines the midpoint between a and b
(check-expect (mid-point (make-posn 0 0) (make-posn 11 0))
              (make-posn (* (+ 0 11) 1/2) (* (+ 0 0) 1/2)))
(check-expect (mid-point (make-posn 5 3)  (make-posn 10 7))
              (make-posn (* (+ 5 10) 1/2) (* (+ 3 7) 1/2)))

;(define (mid-point a b) a) ;stub

(define (mid-point a b)
  (make-posn (* (+ (posn-x a) (posn-x b)) 1/2) (* (+ (posn-y a) (posn-y b)) 1/2)))


; set THRESHOLD to either
; - DEFAULT-THRESHOLD 
; - SPARSER-THRESHOLD
; - DENSER-THRESHOLD
(add-sierpinski MT A B C)