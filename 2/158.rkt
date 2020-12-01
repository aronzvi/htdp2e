;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |158|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Constants:

(define HEIGHT 220) ; distances in terms of pixels 
(define WIDTH 30)
(define XSHOTS (- (/ WIDTH 2) 10))
 
; graphical constants 
(define BACKGROUND (rectangle WIDTH HEIGHT "solid" "green"))
(define SHOT-COLOR "black")
(define SHOT (rectangle 2 20 "solid" SHOT-COLOR))

;; Data defenitions:

; A List-of-numbers is one of: 
; – '()
; – (cons Number List-of-numbers)
; interpretation. a list of numbers

(define LON1 empty)
(define LON2 (cons 45 LON1))
(define LON3 (cons 78 LON2))

(define (fn-for-lon lon)
  (cond
    [(empty? lon) (...)]
    [else
     (... (first lon)                  ;Number
          (fn-for-lon (rest lon)))]))  ;List-of-numbers

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: '()
;; - compound: (cons Number List-of-numbers)
;; - self-reference: (rest lon) is List-of-numbers


; A ShotWorld is List-of-numbers. 
; interpretation each number on such a list
;   represents the y-coordinate of a shot

;; Functions:

; ShotWorld -> ShotWorld
;; start the shots world with (main empty)
(define (main w)
  (big-bang w
    [on-tick tock]
    [on-key keyh]
    [to-draw to-image]))

; ShotWorld -> Image
; adds the image of a shot for each  y on w 
; at (MID,y} to the background image
(check-expect (to-image empty) BACKGROUND)
(check-expect (to-image (cons 9 '()))
              (place-image SHOT XSHOTS 9 BACKGROUND))
(check-expect (to-image (cons 40 (cons 9 '())))
              (place-image SHOT XSHOTS 40 (place-image SHOT XSHOTS 9 BACKGROUND)))

;(define (to-image w) BACKGROUND) ;stub

(define (to-image lon)
  (cond
    [(empty? lon) BACKGROUND]
    [else
     (place-image SHOT XSHOTS (first lon)                  
                  (to-image (rest lon)))]))  

; ShotWorld -> ShotWorld
; moves each shot on w up by one pixel
(check-expect (tock empty) empty)
(check-expect (tock (cons 9 empty)) (cons (- 9 1) empty))
(check-expect (tock (cons 0 empty)) empty)
(check-expect (tock (cons 55 (cons 0 (cons 9 empty)))) (cons (- 55 1) (cons (- 9 1) empty)))

;(define (tock w) w) ;stub

(define (tock lon)
  (cond
    [(empty? lon) empty]
    [else
     (remove-off-canvas
      (cons (- (first lon) 1)                 
            (tock (rest lon))))]))

;; ShotWorld -> ShotWorld
;; eliminates shots in the list above the canvas
(check-expect (remove-off-canvas empty) empty)
(check-expect (remove-off-canvas (cons -1 empty)) empty)
(check-expect (remove-off-canvas (cons 0 empty)) (cons 0 empty))
(check-expect (remove-off-canvas (cons -1 (cons 55 (cons -1 empty)))) (cons 55 empty))

;(define (remove-off-canvas w) w) ;stub

(define (remove-off-canvas lon)
  (cond
    [(empty? lon) empty]
    [else
     (if (>= (first lon) 0)
         (cons (first lon) (remove-off-canvas (rest lon)))
         (remove-off-canvas (rest lon)))])) 

; ShotWorld KeyEvent -> ShotWorld 
; adds a shot to the world 
; if the player presses the space bar
(check-expect (keyh empty " ") (cons HEIGHT empty))
(check-expect (keyh (cons 55 (cons 9 empty)) " ") (cons HEIGHT (cons 55 (cons 9 empty))))
(check-expect (keyh (cons 55 (cons 9 empty)) "up") (cons 55 (cons 9 empty)))

;(define (keyh w ke) w) ;stub

#;
(define (keyh w ke)
  (cond
    [(key=? ke " ") (... w)]
    [else
     (... w)]))
;; Template formed using the large enumeration special case

(define (keyh w ke)
  (cond
    [(key=? ke " ") (cons HEIGHT w)]
    [else w]))