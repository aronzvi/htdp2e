;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |44|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH-OF-WORLD 600)
(define HEIGHT-OF-WORLD (/ WIDTH-OF-WORLD 4))
(define WHEEL-RADIUS 5)
(define WHEEL-DISTANCE (* WHEEL-RADIUS 8))
(define WHEEL
  (circle WHEEL-RADIUS "solid" "black"))
(define CHASIS
  (rectangle WHEEL-DISTANCE ( * WHEEL-RADIUS 1.5) "solid" "red"))
(define TOP (rectangle (/ WHEEL-DISTANCE 2) WHEEL-RADIUS "solid" "red"))
(define CHASIS-WITH-TOP
  (above TOP CHASIS))
(define SPACE (rectangle ( - WHEEL-DISTANCE (* WHEEL-RADIUS 5)) (/ WHEEL-RADIUS 3) "solid" "white"))
(define WHEELS-WITH-SPACE (beside WHEEL SPACE WHEEL))
(define CAR (overlay/align/offset "middle" "bottom"  WHEELS-WITH-SPACE 0 (- WHEEL-RADIUS) CHASIS-WITH-TOP))
(define TREE
  (underlay/xy (circle 10 "solid" "green")
               9 15
               (rectangle 2 20 "solid" "brown")))
(define BACKGROUND (underlay/align "middle" "bottom" (empty-scene WIDTH-OF-WORLD HEIGHT-OF-WORLD) TREE))
(define GROUND-FOR-CAR (- HEIGHT-OF-WORLD
                 (/ (image-height CAR) 2)))
(define CAR-OFF-RIGHT (+ WIDTH-OF-WORLD
                         (image-width CAR)))
(define Y-CAR GROUND-FOR-CAR)



; A WorldState is a Number.
; interpretation the number of pixels between
; the left border of the scene and the x-coordinate of the right-most edge of the car

; WorldState -> Image
; places the car into the BACKGROUND scene,
; according to the given world state
(check-expect (render 100) (place-image CAR 100 Y-CAR BACKGROUND))
(check-expect (render 200) (place-image CAR 200 Y-CAR BACKGROUND))
(define (render ws)
   (place-image CAR ws Y-CAR BACKGROUND))
 
; WorldState -> WorldState 
; moves the car by 3 pixels for every clock tick
(check-expect (tock 20) 23)
(check-expect (tock 78) 81)
(define (tock ws)
  (+ ws 3))
 
 
; WorldState -> Boolean
; stop when the car has disappeared on the right side
(check-expect (end? (-  WIDTH-OF-WORLD 3)) false)
(check-expect (end? WIDTH-OF-WORLD) false)
(check-expect (end? (- CAR-OFF-RIGHT 1)) false)
(check-expect (end? CAR-OFF-RIGHT) true)
(check-expect (end? (+ CAR-OFF-RIGHT 1)) true)
(define (end? ws)
  (>= ws CAR-OFF-RIGHT))

; WorldState Number Number String -> WorldState
; places the car at x-mouse
; if the given me is "button-down"
(check-expect (hyper 21 10 20 "enter") 21)
(check-expect (hyper 42 10 20 "button-down") 10)
(check-expect (hyper 42 10 20 "move") 42)
(define (hyper x-position-of-car x-mouse y-mouse me)
  (cond [(string=? "button-down" me)  x-mouse]
        [else x-position-of-car]))

; WorldState -> WorldState
; launches the program from some initial state 
(define (main ws)
   (big-bang ws
     [on-tick tock]
     [to-draw render]
     [on-mouse hyper]
     [stop-when end?]))