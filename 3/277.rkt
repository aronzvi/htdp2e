;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |277|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; ===========================
;; Constants

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
(define UFO-MAX-JUMP-LEN 3)
(define UFO-LANDED (- SCENE-HEIGHT UFO-MID-HEIGHT))

(define MISSILE-COLOR "red")
(define MISSILE-HEIGHT (/ TANK-HEIGHT 1.5))
(define MISSILE-MID-HEIGHT (/ MISSILE-HEIGHT 2))
(define MISSILE (triangle MISSILE-HEIGHT "solid" MISSILE-COLOR))
(define MISSILE-SPEED (* 2 UFO-SPEED))
(define MISSILE-LAUNCH-HEIGHT (- SCENE-HEIGHT TANK-HEIGHT))

(define CHARGE-COLOR "purple")
(define CHARGE-R (/ UFO-R 3))
(define CHARGE (circle CHARGE-R "solid" CHARGE-COLOR))
(define CHARGE-LAUNCH-RATE 75)
(define CHARGE-INIT-DELAY CHARGE-LAUNCH-RATE)
(define CHARGE-HALF-HEIGHT (/ (image-height CHARGE) 2))
(define CHARGE-SPEED 1.5)
(define CHARGE-LANDED (- SCENE-HEIGHT CHARGE-HALF-HEIGHT))

(define GAME-OVER-X SCENE-MID-X)
(define GAME-OVER-Y (/ SCENE-HEIGHT 2))

(define GROUND-PROXIMITY 0)
(define HIT-PROXIMITY (/ (image-width UFO) 3))
(define TANK-HIT-PROXIMITY (/ (image-width TANK) 3))

;; ===========================
;; Data definitions


; A UFO is a Posn
; interp: the coordinates of the UFO

(define UFO1 (make-posn 20 30))
(define UFO2 (make-posn 50 50))

#;
(define (fn-for-ufo u)
  (... (ufo-x u))           
  (ufo-y u))             

;; Template rules used:
;; - compound: 2 fields

(define-struct charges [next-charge-timer fired])
; Charges is (make-charges Number ListOfCharge)
; interp. The state of the charges where:
;  - next-charge-timer is the amount of ticks until the next charge will be fired
;  - fired is the list of charges currently fired

(define CHARGES0 (make-charges 5 empty)) ; charge will fire in 5 ticks. None currently fired
(define CHARGES1 (make-charges 0 empty)) ; charge will fire now. None currently fired
(define CHARGES2 (make-charges 5 (list (make-posn 30 40) (make-posn 15 25)))) ; charge will fire in 5 ticks. charges already fired
(define CHARGES3 (make-charges 0 (list (make-posn 30 40) (make-posn 15 25)))) ; charge will fire now. charges already fired

(define (fn-for-charges c)
  (... (charges-next-charge-timer c)     ; Number
       (fn-for-loc (charges-fired c))))  ; ListOfCharge

; Template rules used:
; - compound: 2 fields
; - reference: (charges-fired c) is ListOfCharge

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

(define-struct sigs [ufo tank missiles charges])
; A SIGS.v2 (short for SIGS version 2) is a structure:
;   (make-sigs UFO Tank Missiles Charges)
; interp: represents the complete state of a
; space invader game

(define S1 (make-sigs UFO1 (make-tank 40 -3) empty (make-charges 5 empty)))                                       ; No missiles, no charges
(define S2 (make-sigs UFO2 (make-tank 90 2) (list (make-posn 50 50) (make-posn 70 50)) empty))                    ; With missiles and no charges
(define S3 (make-sigs UFO2 (make-tank 90 2) empty (list (make-posn 50 50) (make-posn 70 50))))                    ; With charges and no missiles
(define S4 (make-sigs UFO2 (make-tank 90 2) (list (make-posn 50 50) (make-posn 70 50)) (list (make-posn 58 69)))) ; With missiles and charges

#;
(define (fn-for-sigs s)
  (... (fn-for-ufo (sigs-ufo s))           ; UFO
       (fn-for-tank (sigs-tank s))         ; Tank
       (fn-for-missile (sigs-missiles s))) ; Missiles
  (fn-for-charges (sigs-charges s)))  ; Charges 
   
; Template rules used:
; - compound: 3 fields
; - reference: (sigs-ufo s) is UFO
; - reference: (sigs-tank s) is Tank
; - reference: (sigs-missiles s) is Missiles
; - reference: (sigs-charges s) is Charges

; Missile is a Posn
; interp: the location of the center of the missile on the scene

(define MISSILE1 (make-posn 40 60))

(define (fn-for-missile m)
  (... (posn-x m)
       (posn-y m)))

; Template rules used:
; - compound: 2 fields

; Missiles is one of: 
; – empty
; – (cons Missile Missiles)
; interp: missiles that have been fired
; Posn is the location of a missile

(define MISSILES1 empty)            ; No missiles fired
(define MISSILES2 (list (make-posn 40 60) (make-posn 90 60))) ; missiles fired

#;
(define (fn-for-missiles m)
  (cond
    [(empty? m) (...)]
    [else (... (fn-for-missile (first m))     ; Missile
               (fn-for-missiles (rest m)))])) ; Missiles

; Template rules used:
; one-of: 2 cases
; - atomic distinct: empty
; - compound: 2 fields
; - reference: (first m) is Missile
; - self-reference: (rest m) is Missiles

; ListOfCharge is one of: 
; – empty
; – (cons Charge ListOfCharge)
; interp: a list of fired charges

(define LOC0 empty) ; No charges 
(define LOC1 (list (make-posn 40 60) (make-posn 90 60))) ; charges fired

#;
(define (fn-for-loc loc)
  (cond
    [(empty? loc) (...)]
    [else (... (fn-for-charge (first loc))     ; Charge
               (fn-for-loc (rest loc)))]))     ; ListOfCharge

; Template rules used:
; one-of: 2 cases
; - atomic distinct: empty
; - compound: 2 fields
; - reference: (first m) is Charge
; - self-reference: (rest m) is ListOfCharge

; A Charge is a Posn
; interp. the coordinates of a fired charge

(define CHARGE0 (make-posn 35 67))

#;
(define (fn-for-charge c)
  (... (posn-x c)           
       (posn-y c)))             

;; Template rules used:
;; - compound: 2 fields

;;====================
;; Functions

(define SIGS-INIT (make-sigs (make-posn SCENE-MID-X 0) (make-tank SCENE-MID-X TANK-SPEED) empty (make-charges CHARGE-INIT-DELAY empty)))

;; SIGS -> SIGS
;; start the world with (main SIGS-INIT)
;; 
(define (main s)
  (big-bang s                                      ; SIGS
    (on-tick   si-move)                            ; SIGS -> SIGS
    (to-draw   si-render)                          ; SIGS -> Image
    (stop-when si-game-over? si-render-final)      ; SIGS -> Boolean
    (on-key    si-control)))                       ; SIGS KeyEvent -> SIGS

; SIGS -> Image
; renders the given game state on top of SCENE
(check-expect (si-render (make-sigs (make-posn 10 20) (make-tank 28 -3) empty (make-charges 5 empty))) ; no missles, no charges
              (place-image UFO 10 20 (place-image TANK 28 TANK-Y SCENE)))
(check-expect (si-render (make-sigs (make-posn 20 100) (make-tank 100 3) (list (make-posn 22 103) (make-posn 50 50)) (make-charges 5 empty)))  ; missiles with hit
              (place-image MISSILE 22 103 (place-image MISSILE 50 50 (place-image UFO 20 100 (place-image TANK 100 TANK-Y SCENE)))))
(check-expect (si-render (make-sigs (make-posn 10 20) (make-tank 28 -3) (list (make-posn 32 (- SCENE-HEIGHT TANK-HEIGHT 10)) (make-posn 50 50)) (make-charges 5 empty)))  ;missiles no hit
              (place-image MISSILE 32 (- SCENE-HEIGHT TANK-HEIGHT 10) (place-image MISSILE 50 50 (place-image UFO 10 20 (place-image TANK 28 TANK-Y SCENE)))))
(check-expect (si-render (make-sigs (make-posn 10 20) (make-tank 28 -3) (list (make-posn 32 (- SCENE-HEIGHT TANK-HEIGHT 10)) (make-posn 50 50)) (make-charges 5 (list (make-posn 50 70) (make-posn 80 20))))) ; missiles and charges. No hit
              (place-image CHARGE 50 70 (place-image CHARGE 80 20 (place-image MISSILE 32 (- SCENE-HEIGHT TANK-HEIGHT 10) (place-image MISSILE 50 50 (place-image UFO 10 20 (place-image TANK 28 TANK-Y SCENE)))))))
(check-expect (si-render (make-sigs (make-posn 10 20) (make-tank 28 -3) (list (make-posn 32 (- SCENE-HEIGHT TANK-HEIGHT 10)) (make-posn 50 50)) (make-charges 5 (list (make-posn 50 70) (make-posn 28 TANK-Y)))))  ; missiles and charges. Charge hit
              (place-image CHARGE 50 70 (place-image CHARGE 28 TANK-Y (place-image MISSILE 32 (- SCENE-HEIGHT TANK-HEIGHT 10) (place-image MISSILE 50 50 (place-image UFO 10 20 (place-image TANK 28 TANK-Y SCENE)))))))

;(define (si-render s) SCENE) ; stub

; Template from SIGS

(define (si-render s)
  (charges-render (sigs-charges s)
                  (missiles-render (sigs-missiles s)           
                                   (ufo-render (sigs-ufo s)
                                               (tank-render (sigs-tank s) SCENE)))))

; Missles Image -> Image 
; adds an image of missiles m to scene s
(check-expect (missiles-render empty SCENE) SCENE)
(check-expect (missiles-render (list (make-posn 32 (- SCENE-HEIGHT TANK-HEIGHT 10)) (make-posn 50 50)) SCENE) (place-image MISSILE 32 (- SCENE-HEIGHT TANK-HEIGHT 10) (place-image MISSILE 50 50 SCENE)))

;(define (missiles-render m s) s) ;stub

#;
(define (missiles-render m img)
  ;        (Missile Image -> Image)  
  (foldr        ...                img m))

(define (missiles-render m img)  
  (foldr missile-render img m))

; Missile Image -> Image
; renders m on img
(check-expect (missile-render (make-posn 50 50) SCENE) (place-image MISSILE 50 50 SCENE))
              
;(define (missile-render m img) img) ;stub

(define (missile-render m img)
  (place-image MISSILE (posn-x m) (posn-y m) img))

; Tank Image -> Image 
; adds t to the given image im
(check-expect (tank-render (make-tank 30 -3) SCENE) (place-image TANK 30 TANK-Y SCENE))

;(define (tank-render t im) im) ;stub

; Template from Tank

(define (tank-render t im)
  (place-image TANK (tank-loc t) TANK-Y im)) 

; UFO Image -> Image 
; adds u to the given image im
(check-expect (ufo-render (make-posn 50 40) SCENE) (place-image UFO 50 40 SCENE))

;(define (ufo-render u im) im) ;stub

; Template from UFO

(define (ufo-render u im)
  (place-image UFO (posn-x u) (posn-y u) im)) 

;; SIGS -> SIGS
;; moves the objects
;; No test since we use random???

;(define (si-move s) s) ; stub

; not using template

; Charges Image -> Image
; renders charges onto image
(check-expect (charges-render (make-charges 5 empty) SCENE) SCENE)
(check-expect (charges-render (make-charges 5 (list (make-posn 80 20))) SCENE)
              (place-image CHARGE 80 20 SCENE))
(check-expect (charges-render (make-charges 5 (list (make-posn 50 70) (make-posn 80 20))) SCENE)
              (place-image CHARGE 50 70 (place-image CHARGE 80 20 SCENE)))

;(define (charges-render c i) i) ;stub

; Template from charges

(define (charges-render c i)
  (render-fired-charges (charges-fired c) i))

;; ListOfCharge Image -> Image
;; Renders the fired charges onto img
(check-expect (render-fired-charges empty SCENE) SCENE)
(check-expect (render-fired-charges (list (make-posn 80 20)) SCENE)
              (place-image CHARGE 80 20 SCENE))
(check-expect (render-fired-charges (list (make-posn 50 70) (make-posn 80 20)) SCENE)
              (place-image CHARGE 50 70 (place-image CHARGE 80 20 SCENE)))

;(define (render-fired-charges c i) i) ;stub

; Template from ListOfCharge

#;
(define (render-fired-charges loc i)
  ;(Charge Image -> Charge)
  (foldr         ...              i loc))

(define (render-fired-charges loc i)
  (foldr charge-render i loc))

;; Charge Image -> Image
;; renders CHARGE on image at charge
(check-expect (charge-render (make-posn 80 20) SCENE)
              (place-image CHARGE 80 20 SCENE))

;(define (charge-render c i) i) ;stub

; Template from Charge

(define (charge-render c i)
  (place-image CHARGE
               (posn-x c)           
               (posn-y c)
               i))    

(define (si-move s)
  (si-move-proper s (ufo-random-x UFO-MAX-JUMP-LEN)))

;; SIGS Number -> SIGS
;; moves the space-invader objects predictably by ufo-delta
(check-expect (si-move-proper (make-sigs (make-posn 10 20) (make-tank 28 -3) empty (make-charges 5 empty)) 3) ;move ufo, move tank, no missiles, dec charge timer, no charges
              (make-sigs (make-posn (+ 10 3) (+ 20 UFO-SPEED)) (make-tank (+ 28 -3) -3) empty (make-charges 4 empty)))
(check-expect (si-move-proper (make-sigs (make-posn 20 100) (make-tank 100 3) (list (make-posn 22 103) (make-posn 50 50)) (make-charges 5 empty)) -1) ;move ufo, move tank, move missiles, dec charge timer, no charges
              (make-sigs (make-posn (+ 20 -1) (+ 100 UFO-SPEED)) (make-tank (+ 100 3) 3) (list (make-posn 22 (- 103 MISSILE-SPEED)) (make-posn 50 (- 50 MISSILE-SPEED))) (make-charges 4 empty)))
(check-expect (si-move-proper (make-sigs (make-posn 20 100) (make-tank 100 3) (list (make-posn 22 103) (make-posn 50 50)) (make-charges 0 empty)) -1) ;move ufo, move tank, move missiles, reset charge timer, charge fired
              (make-sigs (make-posn (+ 20 -1) (+ 100 UFO-SPEED)) (make-tank (+ 100 3) 3) (list (make-posn 22 (- 103 MISSILE-SPEED)) (make-posn 50 (- 50 MISSILE-SPEED))) (make-charges CHARGE-LAUNCH-RATE (list (make-posn 20 (+ 100 CHARGE-HALF-HEIGHT))))))
(check-expect (si-move-proper (make-sigs (make-posn 20 100) (make-tank 100 3) (list (make-posn 22 103) (make-posn 50 50)) (make-charges 3 (list (make-posn 20 20) (make-posn 25 35)))) -1) ;move ufo, move tank, move missiles, dec charge timer, move charges
              (make-sigs (make-posn (+ 20 -1) (+ 100 UFO-SPEED)) (make-tank (+ 100 3) 3) (list (make-posn 22 (- 103 MISSILE-SPEED)) (make-posn 50 (- 50 MISSILE-SPEED))) (make-charges 2 (list (make-posn 20 (+ 20 CHARGE-SPEED)) (make-posn 25 (+ 35 CHARGE-SPEED))))))

;(define (si-move-proper s ufo-delta) s) ;stub

; Template from SIGS

(define (si-move-proper s ufo-delta)
  (make-sigs (ufo-move (sigs-ufo s) ufo-delta)           
             (tank-move (sigs-tank s))         
             (remove-off-canvas-missiles (missiles-move (sigs-missiles s)))
             (remove-off-canvas-charges (charge-launch-control (sigs-ufo s) (charges-move (sigs-charges s)))))) 

;; Number -> Number
;; creates a random x-coordinate for the ufo with max. Passing in this max since we cannot create function with zero parameters
(check-random (ufo-random-x 3) (- (random (* 3 2)) 3))
(check-random (ufo-random-x 5) (- (random (* 5 2)) 5))

;(define (ufo-random-x max) max) ;stub

(define (ufo-random-x max)
  (- (random (* max 2)) max))

;; UFO Number -> UFO
;; moves given ufo by delta-x and UFO-SPEED
(check-expect (ufo-move (make-posn 10 20) 3) (make-posn (+ 10 3) (+ 20 UFO-SPEED)))
(check-expect (ufo-move (make-posn 10 20) -2) (make-posn (+ 10 -2) (+ 20 UFO-SPEED)))

;(define (ufo-move u delta-x) u) ; stub

; Template from UFO

(define (ufo-move u delta-x)
  (make-posn (+ (posn-x u) delta-x)
             (+ (posn-y u) UFO-SPEED)))

;; Tank -> Tank
;; moves the given tank by its speed
(check-expect (tank-move (make-tank 100 3)) (make-tank (+ 100 3) 3))
(check-expect (tank-move (make-tank 100 -3)) (make-tank (+ 100 -3) -3))

;(define (tank-move t) t) ; stub

; Template from Tank

(define (tank-move t)
  (make-tank (+ (tank-loc t) (tank-vel t))
             (tank-vel t)))

;; Missiles -> Missiles
;; moves missiles by MISSILE-SPEED
(check-expect (missiles-move empty) empty)
(check-expect (missiles-move (list (make-posn 22 103) (make-posn 50 50))) (list (make-posn 22 (- 103 MISSILE-SPEED)) (make-posn 50 (- 50 MISSILE-SPEED))))

;(define (missiles-move m) m) ;stub

#;
(define (missiles-move lom)
  ;(Missile -> Missile)
  (map ...                   lom))

#;
(define (missiles-move lom)
  (map missile-move lom))

(define (missiles-move lom)
  (move-objects missile-move lom))

;; ListOfCharge -> ListOfCharge
;; moves fired charges by CHARGE-SPEED
(check-expect (move-fired-charges empty)
              empty)
(check-expect (move-fired-charges (list (make-posn 20 20) (make-posn 25 35)))
              (list (make-posn 20 (+ 20 CHARGE-SPEED)) (make-posn 25 (+ 35 CHARGE-SPEED))))

;(define (move-fired-charges loc) loc) ;stub

#;
(define (move-fired-charges loc)
  ;(Charge -> Charge)
  (map ...                 loc))

#;
(define (move-fired-charges loc)
  (map charge-move loc))

(define (move-fired-charges loc)
  (move-objects charge-move loc))

(check-expect (move-objects charge-move empty)
              empty)
(check-expect (move-objects charge-move (list (make-posn 20 20) (make-posn 25 35)))
              (list (make-posn 20 (+ 20 CHARGE-SPEED)) (make-posn 25 (+ 35 CHARGE-SPEED))))
(check-expect (move-objects missile-move (list (make-posn 22 103) (make-posn 50 50))) (list (make-posn 22 (- 103 MISSILE-SPEED)) (make-posn 50 (- 50 MISSILE-SPEED))))

(define (move-objects fn-move loo)
  (map fn-move loo))

; Missile -> Missile
; moves m by MISSILE-SPEED
(check-expect (missile-move (make-posn 22 103)) (make-posn 22 (- 103 MISSILE-SPEED)))

;(define (missile-move m) m) ;stub

(define (missile-move m)
  (make-posn (posn-x m)
             (- (posn-y m) MISSILE-SPEED)))

;; Missiles -> Missiles
;; eliminates missiles in the list above the canvas
(check-expect (remove-off-canvas-missiles empty) empty)
(check-expect (remove-off-canvas-missiles (list (make-posn 50 50) (make-posn 80 50))) (list (make-posn 50 50) (make-posn 80 50)))
(check-expect (remove-off-canvas-missiles (list (make-posn 50 50) (make-posn 50 (- MISSILE-MID-HEIGHT)))) (list (make-posn 50 50)))
(check-expect (remove-off-canvas-missiles (list (make-posn 50 50) (make-posn 50 (- (- MISSILE-MID-HEIGHT) 1)))) (list (make-posn 50 50)))
(check-expect (remove-off-canvas-missiles (list (make-posn 50 50) (make-posn 50 (+ (- MISSILE-MID-HEIGHT) 1)))) (list (make-posn 50 50) (make-posn 50 (+ (- MISSILE-MID-HEIGHT) 1))))
              
;(define (remove-off-canvas-missiles m) m) ;stub

#;
(define (remove-off-canvas-missiles m)
  ;(Missile -> Boolean)
  (filter ...                  m))

(define (remove-off-canvas-missiles m)
  (filter missile-on-canvas? m))

;; Missile -> Boolean?
;; produces true if m is on the canvas
(check-expect (missile-on-canvas? (make-posn 50 50)) true)
(check-expect (missile-on-canvas? (make-posn 50 (- MISSILE-MID-HEIGHT))) false)
(check-expect (missile-on-canvas? (make-posn 50 (- (- MISSILE-MID-HEIGHT) 1))) false)
(check-expect (missile-on-canvas? (make-posn 50 (+ (- MISSILE-MID-HEIGHT) 1))) true)

;(define (missile-on-canvas? m) true) ;stub

(define (missile-on-canvas? m)
  (> (posn-y m) (- MISSILE-MID-HEIGHT)))

;; Charges -> Charges
;; moves fired charges by CHARGE-SPEED
(check-expect (charges-move (make-charges 2 empty))
              (make-charges 2 empty))
(check-expect (charges-move (make-charges 2 (list (make-posn 20 20) (make-posn 25 35))))
              (make-charges 2 (list (make-posn 20 (+ 20 CHARGE-SPEED)) (make-posn 25 (+ 35 CHARGE-SPEED)))))

;(define (charges-move c) c) ;stub

;Template from Charges

(define (charges-move c)
  (make-charges (charges-next-charge-timer c) 
                (move-fired-charges (charges-fired c))))

;; UFO Charges -> Charges
;; launches new charge if ready. reset or decrement next charge timer
(check-expect (charge-launch-control (make-posn 50 50) (make-charges 5 empty))
              (make-charges 4 empty))
(check-expect (charge-launch-control (make-posn 20 100) (make-charges 0 empty))
              (make-charges CHARGE-LAUNCH-RATE (list (make-posn 20 (+ 100 CHARGE-HALF-HEIGHT)))))
(check-expect (charge-launch-control (make-posn 20 100) (make-charges 0 (list (make-posn 40 40))))
              (make-charges CHARGE-LAUNCH-RATE (list (make-posn 20 (+ 100 CHARGE-HALF-HEIGHT)) (make-posn 40 40))))

;(define (charge-launch-control u c) c) ;stub

; Template from Charges

(define (charge-launch-control u c)
  (if (= (charges-next-charge-timer c) 0)
      (make-charges CHARGE-LAUNCH-RATE (launch-charge u (charges-fired c)))
      (make-charges (- (charges-next-charge-timer c) 1)
                    (charges-fired c))))  

;; Charges -> Charges
;; removes charges that have gone off the canvas
(check-expect (remove-off-canvas-charges (make-charges 5 empty))
              (make-charges 5 empty))
(check-expect (remove-off-canvas-charges (make-charges 5 (list (make-posn 50 50))))
              (make-charges 5 (list (make-posn 50 50))))
(check-expect (remove-off-canvas-charges  (make-charges 5 (list (make-posn 50 50) (make-posn 50 CHARGE-LANDED))))
              (make-charges 5 (list (make-posn 50 50) (make-posn 50 CHARGE-LANDED))))
(check-expect (remove-off-canvas-charges (make-charges 5 (list (make-posn 50 50) (make-posn 50 (+ CHARGE-LANDED 1)))))
              (make-charges 5 (list (make-posn 50 50))))

;(define (remove-off-canvas-charges c) c) ;stub

; Template from Charges. Wrapper??

(define (remove-off-canvas-charges c)
  (make-charges (charges-next-charge-timer c)
                (remove-off-canvas-fired-charges (charges-fired c))))

;; ListOfCharge -> ListOfCharge
;; removes fired charges that have gone off the canvas
(check-expect (remove-off-canvas-fired-charges empty)
              empty)
(check-expect (remove-off-canvas-fired-charges (list (make-posn 50 50)))
              (list (make-posn 50 50)))
(check-expect (remove-off-canvas-fired-charges (list (make-posn 50 50) (make-posn 50 CHARGE-LANDED)))
              (list (make-posn 50 50) (make-posn 50 CHARGE-LANDED)))
(check-expect (remove-off-canvas-fired-charges (list (make-posn 50 50) (make-posn 50 (+ CHARGE-LANDED 1))))
              (list (make-posn 50 50)))

;(define (remove-off-canvas-fired-charges loc) loc) ;stub

#;
(define (remove-off-canvas-fired-charges loc)
  ;(Charge -> Boolean)
  (filter ...                 loc))

(define (remove-off-canvas-fired-charges loc)
  (filter charge-on-canvas? loc))

;; Charge -> Boolean
;; produces true if charge height <= CHARGE-LANDED
(check-expect (charge-on-canvas? (make-posn 50 50)) true)
(check-expect (charge-on-canvas? (make-posn 50 CHARGE-LANDED)) true)
(check-expect (charge-on-canvas? (make-posn 50 (+ CHARGE-LANDED 1))) false)

;(define (charge-on-canvas? c) false) ;stub

; Template from Charge

(define (charge-on-canvas? c)
  (<= (posn-y c) CHARGE-LANDED))        

;; Charge -> Charge
;; moves a fired charge by CHARGE-SPEED
(check-expect (charge-move (make-posn 20 20)) (make-posn 20 (+ 20 CHARGE-SPEED)))

;(define (charge-move c) c) ;stub

; Template from Charge

(define (charge-move c)
  (make-posn (posn-x c)           
             (+ (posn-y c) CHARGE-SPEED)))

;; UFO ListOfCharge -> ListOfCharge
;; adds a new charge fired from ufo position to loc
(check-expect (launch-charge (make-posn 20 100) empty)
              (list (make-posn 20 (+ 100 CHARGE-HALF-HEIGHT))))
(check-expect (launch-charge (make-posn 20 100) (list (make-posn 40 40)))
              (list (make-posn 20 (+ 100 CHARGE-HALF-HEIGHT)) (make-posn 40 40)))
              
;(define (launch-charge u loc) loc) ;stub

; Template from where? not ListOfCharge... ? 

(define (launch-charge u loc)
  (cons (make-posn (posn-x u) (+ (posn-y u) CHARGE-HALF-HEIGHT)) loc))

;; SIGS -> Boolean
;; stops the game if the UFO lands, if the missile hits the UFO or if charge hit the tank
(check-expect (si-game-over? (make-sigs (make-posn 30 UFO-LANDED) ;landed. no missiles
                                        (make-tank 60 5)
                                        empty
                                        (make-charges 5 empty)))
              true) 
(check-expect (si-game-over? (make-sigs (make-posn 30 (/ SCENE-HEIGHT 2)) ;not landed. no missiles
                                        (make-tank 60 5)
                                        empty
                                        (make-charges 5 empty)))
              false) 
(check-expect (si-game-over? (make-sigs (make-posn 30 70)     ; missiles hit. 
                                        (make-tank 60 5)
                                        (list (make-posn 30 (+ 70 HIT-PROXIMITY)) (make-posn 85 45))
                                        (make-charges 5 empty)))
              true) 
(check-expect (si-game-over? (make-sigs (make-posn 30 UFO-LANDED) ; missiles. no hit. landed. 
                                        (make-tank 60 5)
                                        (list (make-posn 70 45) (make-posn 85 45))
                                        (make-charges 5 empty)))
              true)
(check-expect (si-game-over? (make-sigs (make-posn 30 150) ; missiles. neither hit nor landed
                                        (make-tank 60 5)
                                        (list (make-posn 70 45) (make-posn 85 45))
                                        (make-charges 5 empty)))
              false)

(check-expect (si-game-over? (make-sigs (make-posn 30 70)
                                        (make-tank 60 5)
                                        (list (make-posn 70 45) (make-posn 85 45))
                                        (make-charges 5 (list (make-posn 60 (- TANK-Y TANK-HIT-PROXIMITY)) (make-posn 85 45)))))  ;charges hit
              true)

;(define (si-game-over? s) false) ; stub

; Template from SIGS

(define (si-game-over? s)
  (or (ufo-close-to-ground? (sigs-ufo s))           
      (missiles-hit-ufo? (sigs-ufo s) (sigs-missiles s))
      (charges-hit-tank? (sigs-tank s) (sigs-charges s))))

;; UFO -> Boolean
;; produces true if UFO distance from ground <= GROUND-PROXIMITY
(check-expect (ufo-close-to-ground? (make-posn 30 (/ SCENE-HEIGHT 2))) false)
(check-expect (ufo-close-to-ground? (make-posn 30  (- SCENE-HEIGHT UFO-MID-HEIGHT))) true)
(check-expect (ufo-close-to-ground? (make-posn 30  (- SCENE-HEIGHT UFO-MID-HEIGHT 1))) false)
              
;(define (ufo-close-to-ground? u) false) ;stub

; template from UFO

(define (ufo-close-to-ground? u)
  (<= (posn-distance (make-posn (posn-x u) SCENE-HEIGHT) 
                     (make-posn (posn-x u) (+ (posn-y u) UFO-MID-HEIGHT)))
      GROUND-PROXIMITY))

;; Posn Posn -> Number
;; produces distance between p1 and p2
(check-within (posn-distance (make-posn 1 2) (make-posn 3 4)) 2.8284 0.01)

;(define (posn-distance p1 p2) 0) ;stub

(define (posn-distance p1 p2)
  (sqrt (+ (sqr (- (posn-x p1) (posn-x p2)))
           (sqr (- (posn-y p1) (posn-y p2))))))

;; UFO Missiles -> Boolean
;; produces true if any of the missiles distance from UFO is <= HIT-PROXIMITY
(check-expect (missiles-hit-ufo? (make-posn 30 70) empty) false) ; no missiles
(check-expect (missiles-hit-ufo? (make-posn 30 70) (list (make-posn 70 70) (make-posn 80 80))) false) ; missiles far away             
(check-expect (missiles-hit-ufo? (make-posn 30 70) (list (make-posn 70 70) (make-posn 30 70))) true) ; hit. exact same
(check-expect (missiles-hit-ufo? (make-posn 30 70) (list (make-posn 70 70) (make-posn 30 (- 70 HIT-PROXIMITY)))) true) ; hit. distance equals HIT-PROXIMITY
(check-expect (missiles-hit-ufo? (make-posn 30 70) (list (make-posn 70 70) (make-posn 30 (- 70 (- HIT-PROXIMITY 0.5))))) true)  ; hit. distance less than HIT-PROXIMITY

;(define (missiles-hit-ufo? u lom) false) ; stub

#;
(define (missiles-hit-ufo? u m)
  ;(Missile -> Booelan)
  (ormap ...                   m))

#;
(define (missiles-hit-ufo? u lom)
  (local (;; Missile -> Boolean
          ;; produces true if missile hit ufo 
          (define (missile-hit-ufo? m) false))
    (ormap missile-hit-ufo? lom)))

(define (missiles-hit-ufo? u lom)
  (local (;; Missile -> Boolean
          ;; produces true if missile hit ufo 
          (define (missile-hit-ufo? m)
            (<= (posn-distance u m)
                HIT-PROXIMITY)))
    (ormap missile-hit-ufo? lom)))

;; Tank Charges -> Boolean
;; produces true if any of the fired charges distance from tank is <= TANK-HIT-PROXIMITY
(check-expect (charges-hit-tank? (make-tank 30 5) (make-charges 5 empty)) false) ; no charges
(check-expect (charges-hit-tank? (make-tank 30 5) (make-charges 5 (list (make-posn 70 70) (make-posn 80 80)))) false) ; charges far away             
(check-expect (charges-hit-tank? (make-tank 30 5) (make-charges 5 (list (make-posn 70 70) (make-posn 30 TANK-Y)))) true) ; hit. exact same
(check-expect (charges-hit-tank? (make-tank 30 5) (make-charges 5 (list (make-posn 70 70) (make-posn 30 (- TANK-Y TANK-HIT-PROXIMITY))))) true) ; hit. distance equals TANK-HIT-PROXIMITY
(check-expect (charges-hit-tank? (make-tank 30 5) (make-charges 5 (list (make-posn 70 70) (make-posn 30 (- TANK-Y (- TANK-HIT-PROXIMITY 0.5)))))) true)  ; hit. distance less than TANK-HIT-PROXIMITY

;(define (charges-hit-tank? t c) false) ;stub

; Template from Charges. Not sure? it's just a wrapper

(define (charges-hit-tank? t c)
  (charges-fired-hit-tank? t (charges-fired c)))

;; Tank ListOfCharge -> Boolean
;; produces true if any of the charges distance from tank is <= TANK-HIT-PROXIMITY
(check-expect (charges-fired-hit-tank? (make-tank 30 5) empty) false) ; no charges
(check-expect (charges-fired-hit-tank? (make-tank 30 5) (list (make-posn 70 70) (make-posn 80 80))) false) ; charges far away             
(check-expect (charges-fired-hit-tank? (make-tank 30 5) (list (make-posn 70 70) (make-posn 30 TANK-Y))) true) ; hit. exact same
(check-expect (charges-fired-hit-tank? (make-tank 30 5) (list (make-posn 70 70) (make-posn 30 (- TANK-Y TANK-HIT-PROXIMITY)))) true) ; hit. distance equals TANK-HIT-PROXIMITY
(check-expect (charges-fired-hit-tank? (make-tank 30 5) (list (make-posn 70 70) (make-posn 30 (- TANK-Y (- TANK-HIT-PROXIMITY 0.5))))) true)  ; hit. distance less than TANK-HIT-PROXIMITY

;(define (charges-fired-hit-tank? t loc) false) ; stub

#;
(define (charges-fired-hit-tank? t loc)
  ;(Charge -> Boolean) 
  (ormap ...                 loc))

#;
(define (charges-fired-hit-tank? t loc)
  (local (;; Charge -> Boolean
          ;; produces true if distance between c and tank <= TANK-HIT-PROXIMITY
          (define (charge-hit-tank? c) false))
    (ormap charge-hit-tank? loc)))

(define (charges-fired-hit-tank? t loc)
  (local (;; Charge -> Boolean
          ;; produces true if distance between c and tank <= TANK-HIT-PROXIMITY
          (define (charge-hit-tank? c)
            (<= (posn-distance (make-posn (tank-loc t) TANK-Y) c)
                TANK-HIT-PROXIMITY)))
    (ormap charge-hit-tank? loc)))
       
;; SIGS -> Image
;; renders the game over screen
(check-expect (si-render-final (make-sigs (make-posn 30 70)
                                          (make-tank 60 5)
                                          empty
                                          (make-charges 4 empty)))
              (place-image (text "GAME OVER" 50 "black") GAME-OVER-X GAME-OVER-Y (si-render (make-sigs (make-posn 30 70)
                                                                                                       (make-tank 60 5)
                                                                                                       empty
                                                                                                       (make-charges 4 empty)))))
(check-expect (si-render-final (make-sigs (make-posn 30 70)         
                                          (make-tank 60 5)
                                          (list (make-posn 30 70))
                                          (make-charges 4 (list (make-posn 30 30) (make-posn 40 40)))))
              (place-image (text "GAME OVER" 50 "black") GAME-OVER-X GAME-OVER-Y (si-render (make-sigs (make-posn 30 70)
                                                                                                       (make-tank 60 5)
                                                                                                       (list (make-posn 30  70))
                                                                                                       (make-charges 4 (list (make-posn 30 30) (make-posn 40 40)))))))
;(define (si-render-final s) SCENE) ; stub

(define (si-render-final s)
  (place-image (text "GAME OVER" 50 "black") GAME-OVER-X GAME-OVER-Y (si-render s)))

;; SIGS KeyEvent -> SIGS
;; changes the direction of the tank with the left and right arrows; fires a missile with the space key if it hasn’t been launched yet
(check-expect (si-control (make-sigs (make-posn 30 20) (make-tank 45 3) empty (make-charges 5 empty)) "left")   ; Not fired - going right, left
              (make-sigs (make-posn 30 20) (make-tank 45 -3) empty (make-charges 5 empty)))                                                
(check-expect (si-control (make-sigs (make-posn 30 20) (make-tank 45 -3) empty (make-charges 5 empty)) "right") ; Not fired - going left, right
              (make-sigs (make-posn 30 20) (make-tank 45 3) empty (make-charges 5 empty)))                                              
(check-expect (si-control (make-sigs (make-posn 60 40) (make-tank 50 -4) empty (make-charges 5 empty)) "left")  ; Not fired - going left, left
              (make-sigs  (make-posn 60 40) (make-tank 50 -4) empty (make-charges 5 empty)))                                             
(check-expect (si-control (make-sigs (make-posn 60 40) (make-tank 50 4) empty (make-charges 5 empty)) "right") ; Not fired - going right, right
              (make-sigs (make-posn 60 40) (make-tank 50 4) empty (make-charges 5 empty)))                                             
(check-expect (si-control (make-sigs (make-posn 60 40) (make-tank 50 -4) empty (make-charges 5 empty)) " ") ; Not fired - fire
              (make-sigs (make-posn 60 40) (make-tank 50 -4) (list (make-posn 50 MISSILE-LAUNCH-HEIGHT)) (make-charges 5 empty))) 
(check-expect (si-control (make-sigs (make-posn 60 40) (make-tank 50 -4) empty (make-charges 5 empty)) "up")    ; Not fired - up key
              (make-sigs (make-posn 60 40) (make-tank 50 -4) empty (make-charges 5 empty))) 
(check-expect (si-control (make-sigs (make-posn 70 40) (make-tank 90 1) (list (make-posn 55 33)) (make-charges 5 empty)) "left")   ; Fired - going right, left
              (make-sigs (make-posn 70 40) (make-tank 90 -1) (list (make-posn 55 33)) (make-charges 5 empty))) 
(check-expect (si-control (make-sigs (make-posn 70 40) (make-tank 90 -1) (list (make-posn 55 33)) (make-charges 5 empty)) "right") ; Fired - going left, right
              (make-sigs (make-posn 70 40) (make-tank 90 1) (list (make-posn 55 33)) (make-charges 5 empty)))
(check-expect (si-control (make-sigs (make-posn 60 30) (make-tank 80 -4) (list (make-posn 55 33)) (make-charges 5 empty)) "left")  ; Fired - going left, left
              (make-sigs (make-posn 60 30) (make-tank 80 -4) (list (make-posn 55 33)) (make-charges 5 empty)))
(check-expect (si-control (make-sigs (make-posn 60 30) (make-tank 80 4) (list (make-posn 55 33)) (make-charges 5 empty)) "right")  ; Fired - going right, right
              (make-sigs (make-posn 60 30) (make-tank 80 4) (list (make-posn 55 33)) (make-charges 5 empty)))
(check-expect (si-control (make-sigs (make-posn 100 90) (make-tank 40 2) (list (make-posn 50 50)) (make-charges 5 empty)) " ") ; Fired - fire
              (make-sigs (make-posn 100 90) (make-tank 40 2) (list (make-posn 40 MISSILE-LAUNCH-HEIGHT) (make-posn 50 50)) (make-charges 5 empty))) 
(check-expect (si-control (make-sigs (make-posn 100 90) (make-tank 40 2) (list (make-posn 50 50)) (make-charges 5 empty)) "down") ; Fired - down key
              (make-sigs (make-posn 100 90) (make-tank 40 2) (list (make-posn 50 50)) (make-charges 5 empty)))

;(define (si-control s ke) s) ;stub

#;
(define (fn-for-key-event s kevt)
  (cond [(key=? " " kevt) (... (fn-for-ufo (sigs-ufo s))              ; UFO
                               (fn-for-missiles (sigs-missiles s))      ; Missiles
                               (fn-for-tank (sigs-tank s)))]          ; Tank
        [(key=? "right" kevt) (... (fn-for-ufo (sigs-ufo s))          ; UFO
                                   (fn-for-missiles (sigs-missiles s))  ; Missiles
                                   (fn-for-tank (sigs-tank s)))]      ; Tank
        [(key=? "left" kevt)  (... (fn-for-ufo (sigs-ufo s))          ; UFO
                                   (fn-for-missiles (sigs-missiles s))  ; Missiles
                                   (fn-for-tank (sigs-tank s)))]      ; Tank
        [else
         (... (fn-for-ufo (sigs-ufo s))                               ; UFO
              (fn-for-missile (sigs-missiles s))                       ; Missiles
              (fn-for-tank (sigs-tank s)))]))                         ;Tank

; Template formed using the large enumeration special case


(define (si-control s kevt)
  (cond [(key=? " " kevt) (make-sigs (sigs-ufo s)    
                                     (sigs-tank s)
                                     (cons (tank-fire-missile (sigs-tank s)) (sigs-missiles s))
                                     (sigs-charges s))]        
        [(key=? "right" kevt) (make-sigs (sigs-ufo s)         
                                         (tank-change-dir-right (sigs-tank s))
                                         (sigs-missiles s)
                                         (sigs-charges s))]      
        [(key=? "left" kevt)  (make-sigs (sigs-ufo s)         
                                         (tank-change-dir-left (sigs-tank s))
                                         (sigs-missiles s)
                                         (sigs-charges s))]      
        [else s]))

;; Tank -> Missile
;; produces a fired missile from tank
(check-expect (tank-fire-missile (make-tank 50 -4)) (make-posn 50 MISSILE-LAUNCH-HEIGHT))

;(define (tank-fire-missile t) (make-posn 0 0)) ; stub

; Template from Tank

(define (tank-fire-missile t)
  (make-posn (tank-loc t) MISSILE-LAUNCH-HEIGHT)) 

;; Tank -> Tank
;; changes the direction of a given tank to right
(check-expect (tank-change-dir-right (make-tank 60 -3)) (make-tank 60 3)) ; going left
(check-expect (tank-change-dir-right (make-tank 60 3)) (make-tank 60 3))  ; going right

;(define (tank-change-dir-right t) t) ; stub

; Template from Tank

(define (tank-change-dir-right t)
  (make-tank (tank-loc t)   
             (if (< (tank-vel t) 0)
                 (- (tank-vel t))
                 (tank-vel t))))
      
;; Tank -> Tank
;; changes the direction of a given tank to left
(check-expect (tank-change-dir-left (make-tank 60 2)) (make-tank 60 -2)) ; going right
(check-expect (tank-change-dir-left (make-tank 60 -2)) (make-tank 60 -2)) ; going left

;(define (tank-change-dir-left t) t) ; stub

; Template from tank

(define (tank-change-dir-left t)
  (make-tank (tank-loc t)
             (if (>  (tank-vel t) 0)
                 (- (tank-vel t))
                 (tank-vel t))))  






