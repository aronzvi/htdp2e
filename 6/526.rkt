;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |526|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define CENTER (make-posn 200 200))
(define RADIUS 200) ; the radius in pixels 
 
; Number -> Posn
; determines the point on the circle with CENTER. The point is the on-screen position which will be displayed properly and not the coordinate
; and RADIUS at angle.

; Parametric Equation of a Circle:
;  x = a + r * cos(t) y = b + r * sin(t)
;  t = the angle in radians

(check-within (circle-pt 360)
              (make-posn (+ (posn-x CENTER) (* RADIUS (cos (degrees->radians 360))))
                         (- (posn-y CENTER) (* RADIUS (sin (degrees->radians 360)))))
              0.1)
(check-within (circle-pt 240)
              (make-posn (+ (posn-x CENTER) (* RADIUS (cos (degrees->radians 240))))
                         (- (posn-y CENTER) (* RADIUS (sin (degrees->radians 240)))))
              0.1)
(check-within (circle-pt 120)
              (make-posn (+ (posn-x CENTER) (* RADIUS (cos (degrees->radians 120))))
                         (- (posn-y CENTER) (* RADIUS (sin (degrees->radians 120)))))
              0.1)

;(define (circle-pt angle) (make-posn 0 0)) ;stub

(define (circle-pt angle)
  (make-posn (+ (posn-x CENTER) (* RADIUS (cos (degrees->radians angle))))
             (- (posn-y CENTER) (* RADIUS (sin (degrees->radians angle))))))

; Number -> Number
; converts degrees to radians
(check-within (degrees->radians 360) (* 2 pi) 0.1)
(check-within (degrees->radians 120) (* 120 (/ pi 180)) 0.1)
(check-within (degrees->radians 240) (* 240 (/ pi 180)) 0.1)

;(define (degrees->radians d) 0) ;stub

(define (degrees->radians d)
  (* d (/ pi 180)))