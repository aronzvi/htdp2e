;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |98|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; ======================
;; Constants:

(define SCENE-WIDTH 400)
(define SCENE-HEIGHT 200)
(define SCENE (empty-scene SCENE-WIDTH SCENE-HEIGHT))
(define SCENE-MID-X (/ SCENE-WIDTH 2))

(define TANK-COLOR "black")
(define TANK-WIDTH 25)
(define TANK-HEIGHT (/ TANK-WIDTH 2))
(define TANK (rectangle TANK-WIDTH TANK-HEIGHT "solid" TANK-COLOR))
(define TANK-Y (- SCENE-HEIGHT (/ (image-height TANK) 2)))
(define TANK-SPEED 1)

(define UFO-COLOR "green")
(define UFO-WIDTH 40)
(define UFO-HEIGHT (/ UFO-WIDTH 2))
(define UFO-MID-HEIGHT (/ UFO-HEIGHT 2))
(define UFO-MID-WIDTH (/ UFO-WIDTH 2))
(define UFO-R UFO-MID-HEIGHT)
(define UFO (overlay (circle UFO-R "solid" UFO-COLOR) (rectangle UFO-WIDTH UFO-R "solid" UFO-COLOR)))
(define UFO-SPEED 1)
(define UFO-JUMP-LEN 1)
(define UFO-LANDED (- SCENE-HEIGHT UFO-MID-HEIGHT))

(define MISSILE-COLOR "red")
(define MISSILE-HEIGHT (/ TANK-HEIGHT 1.5))
(define MISSILE-MID-HEIGHT (/ MISSILE-HEIGHT 2))
(define MISSILE (triangle MISSILE-HEIGHT "solid" MISSILE-COLOR))
(define MISSILE-SPEED (* 2 UFO-SPEED))

(define GAME-OVER-X SCENE-MID-X)
(define GAME-OVER-Y (/ SCENE-HEIGHT 2))


;; ======================
;; Data definitions:

; A UFO is a Posn. 
; interpretation (make-posn x y) is the UFO's location 
; (using the top-down, left-to-right convention)

(define UFO1 (make-posn 20 30))

#;
(define (fn-for-ufo u)
  (... (posn-x u)   ;Number
       (posn-y u))) ;Number

;; Template rules used:
;; - compound: 2 fields
 
(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number). 
; interpretation (make-tank x dx) specifies the position:
; (x, TANK-Y) and the tank's speed: dx pixels/tick

(define T1 (make-tank 30 3)) ; Moving to the right at 3 pixels/tick
(define T2 (make-tank 80 -6)) ; Moving to the left at 6 pixels/tick

#;
(define (fn-for-tank t)
  (... (tank-loc t)   ;Number
       (tank-vel t))) ;Number 

;; Template rules used:
;; - compound: 2 fields
 
; A Missile is a Posn. 
; interpretation (make-posn x y) is the missile's place

(define M1 (make-posn 30 20))

#;
(define (fn-for-MISSILE m)
  (... (posn-x m)   ;Number
       (posn-y m))) ;Number

;; Template rules used:
;; - compound: 2 fields

(define-struct aim [ufo tank])
; An aim is a structure:
;    (make-aim UFO Tank)
; interp. The state when the player is trying to get the tank in position for a shot

(define AIM1 (make-aim (make-posn 10 20) (make-tank 28 -3)))

#;
(define (fn-for-aim a)
  (... (fn-for-ufo (aim-ufo a))     ;UFO
       (fn-for-tank (aim-tank a)))) ;Tank

;; Template rules used:
;; - compound: 2 fields
;; - reference: (aim-ufo a) is UFO
;; - reference: (aim-tank a) is Tank

(define-struct fired [ufo tank missile])
; A fired is a structure:
;   (make-fired UFO Tank MISSILE)
; interp. The state after the missile is fired

(define F1 (make-fired
            (make-posn 20 100)
            (make-tank 100 3)
            (make-posn 22 103)))
(define F2 (make-fired
            (make-posn 10 20)
            (make-tank 28 -3)
            (make-posn 32 (- SCENE-HEIGHT TANK-HEIGHT 10))))

#;
(define (fn-for-fired f)
  (... (fn-for-ufo (fired-ufo f))         ;UFO
       (fn-for-tank (fired-tank f))       ;Tank
       (fn-for-missile (fired-missile f)))) ;Missile

;; Template rules used:
;; - compound: 3 fields
;; - reference: (fired-ufo f) is UFO
;; - reference: (fired-tank f) is Tank
;; - reference: (fired-missile f) is Missile

; A SIGS is one of: 
; – (make-aim UFO Tank)
; – (make-fired UFO Tank Missile)
; interpretation represents the complete state of a 
; space invader game

(define S1 AIM1)
(define S2 F2)

(define (fn-for-sigs s)
  (cond [(aim? s) (... (fn-for-ufo (aim-ufo s))         ; UFO
                       (fn-for-tank (aim-tank s)))]     ; Tank 
        [else (... (fn-for-ufo (fired-ufo s))           ; Ufo
                   (fn-for-tank (fired-tank s))         ; Tank
                   (fn-for-missile (fired-missile s)))])) ; Missile 

;; Template rules used:
;; - one of: 2 cases
;; – compound: (make-aim UFO Tank)
;; - compound: (make-fired UFO Tank Missile)
;; - reference: (aim-ufo s) is UFO
;; - reference: (aim-tank s) is Tank
;; - reference: (fired-ufo s) is UFO
;; - reference: (fired-tank s) is Tank
;; - reference: (fired-missile s) is Missile

;; SIGS -> Boolean
;; stops the game if the UFO lands or if the missile hits the UFO
(check-expect (si-game-over? (make-aim (make-posn 30 UFO-LANDED) ; aim - landed. landing proximity?
                                       (make-tank 60 5))) true) 
(check-expect (si-game-over? (make-aim (make-posn 30 (/ SCENE-HEIGHT 2)) ; aim - not landed
                                       (make-tank 60 5))) false) 

(check-expect (si-game-over? (make-fired (make-posn 30 70)         ; fired - hit. hit proximity? 
                                         (make-tank 60 5)
                                         (make-posn 30 (+ 70 MISSILE-MID-HEIGHT)))) true) 
(check-expect (si-game-over? (make-fired (make-posn 30 UFO-LANDED) ; fired - landed. landing proximity?
                                         (make-tank 60 5)
                                         (make-posn 70 45))) true)
(check-expect (si-game-over? (make-fired (make-posn 30 150) ; fired - neither hit nor landed
                                         (make-tank 60 5)
                                         (make-posn 70 45))) false) 

;(define (si-game-over? s) false) ;stub

; Template from SIGS

(define (si-game-over? s)
  (cond [(aim? s) (ufo-landed? (aim-ufo s))]
        [else (or (ufo-landed? (fired-ufo s))            
                  (missile-hit-ufo? (fired-missile s) (fired-ufo s)))]))

;; UFO -> Boolean
;; produces true if UFO has landed
(check-expect (ufo-landed? (make-posn 30 (/ SCENE-HEIGHT 2))) false)
(check-expect (ufo-landed? (make-posn 30 UFO-LANDED)) true)

;(define (ufo-landed? u) false) ;stub

; template from UFO

(define (ufo-landed? u)
  (>= (ufo-bottom u) SCENE-HEIGHT))

;; UFO -> Number
;; returns the bottom edge y value for the given ufo using the UFO image
(check-expect (ufo-bottom (make-posn 30 70)) (+ 70 UFO-MID-HEIGHT))

;(define (ufo-bottom-edge u) 0) ;stub

; Template from UFO

(define (ufo-bottom u)
  (+ (posn-y u) UFO-MID-HEIGHT))

;; UFO -> Number
;; returns the left edge x value for the given ufo using the UFO image
(check-expect (ufo-left-edge (make-posn 25 80)) (- 25 UFO-MID-WIDTH))

;(define (ufo-left-edge u) 0) ; stub

; Template from UFO

(define (ufo-left-edge u)
  (- (posn-x u) UFO-MID-WIDTH))

;; UFO -> Number
;; returns the right edge x value for the given ufo using the UFO image
(check-expect (ufo-right-edge (make-posn 65 98)) (+ 65 UFO-MID-WIDTH))

;(define (ufo-right-edge u) 0) ; stub

(define (ufo-right-edge u)
  (+ (posn-x u) UFO-MID-WIDTH)) 
  
;; UFO Missile -> Boolean
;; produces true if the top edge of the missile has hit the center of hight of the UFO
;; and x of the missile is within the ufo width. 
(check-expect (missile-hit-ufo? (make-posn 30 (+ 70 MISSILE-MID-HEIGHT)) (make-posn 30 70)) true)                   ; top of missile = mid of UFO width , x of missile = mid of UFO width  - hit
(check-expect (missile-hit-ufo? (make-posn (- 75 UFO-MID-WIDTH) (+ 50 MISSILE-MID-HEIGHT)) (make-posn 75 50)) true) ; top of missile = mid of UFO width,  x of missile = left edge UFO width - hit
(check-expect (missile-hit-ufo? (make-posn (+ 60 UFO-MID-WIDTH) (+ 80 MISSILE-MID-HEIGHT)) (make-posn 60 80)) true) ; top of missile = mid of UFO width,  x of missile = right edge UFO width - hit

(check-expect (missile-hit-ufo? (make-posn 30 100) (make-posn 30 150)) false) ; top of missile below UFO center. x of missile = mid of UFO width - miss
(check-expect (missile-hit-ufo? (make-posn 30 70) (make-posn 150 70)) false) ; missile to the left of UFO. x of missile < left edge of UFO. No overlap - miss
(check-expect (missile-hit-ufo? (make-posn 300 30) (make-posn 20 50)) false) ; missile to the right of UFO. x of missile > right edge of UFO. No overlap - miss

;(define (missile-hit-ufo? u m) false) ; stub

; Template from Missile

(define (missile-hit-ufo? m u)
  (and (= (missile-top m) (posn-y u))
       (<= (ufo-left-edge u) (posn-x m))
       (<= (posn-x m) (ufo-right-edge u))))

;; Missile -> Number
;; returns the top y value for the given missile using the MISSILE image
(check-expect (missile-top (make-posn 50 60)) (- 60 MISSILE-MID-HEIGHT))

;(define (missile-top m) 0) ;stub 

; Template from Missile

(define (missile-top m)
  (- (posn-y m) MISSILE-MID-HEIGHT))

;; SIGS -> Image
;; renders the game over screen
(check-expect (si-render-final (make-fired (make-posn 30 70)         
                          (make-tank 60 5)
                          (make-posn 30 (+ 70 MISSILE-MID-HEIGHT)))) (place-image (text "GAME OVER" 50 "black") GAME-OVER-X GAME-OVER-Y SCENE))

(define (si-render-final s)
  (place-image (text "GAME OVER" 50 "black") GAME-OVER-X GAME-OVER-Y SCENE))


        
           

