;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 102-proximity) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define UFO-LANDED-PROXIMITY 0)
(define UFO-MISSILE-HIT-PROXIMITY (/ (image-width UFO) 5))

(define GAME-OVER-X SCENE-MID-X)
(define GAME-OVER-Y (/ SCENE-HEIGHT 2))

;; ===========================
;; Data definitions

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

(define-struct sigs [ufo tank missile])
; A SIGS.v2 (short for SIGS version 2) is a structure:
;   (make-sigs UFO Tank MissileOrNot)
; interp: represents the complete state of a
; space invader game

(define S1 (make-sigs (make-posn 50 90) (make-tank 40 -3) #false))           ; No missile
(define S2 (make-sigs (make-posn 54 60) (make-tank 90 2) (make-posn 50 50))) ; With missile
(define SIGS-START (make-sigs (make-posn SCENE-MID-X 0) (make-tank SCENE-MID-X TANK-SPEED) #false))

#;
(define (fn-for-sigs s)
  (... (fn-for-ufo (sigs-ufo s))           ; UFO
       (fn-for-tank (sigs-tank s))         ; Tank
       (fn-for-missile (sigs-missile s)))) ; Missile

; Template rules used:
; - compound: 3 fields
; - reference: (sigs-ufo s) is UFO
; - reference: (sigs-tank s) is Tank
; - reference: (sigs-missile s) is Missile


; A MissileOrNot is one of: 
; – #false
; – Posn
; interp: #false means the missile is in the tank;
; Posn says the missile is at that location

(define M1 #false)            ; missile is in the tank
(define M2 (make-posn 40 60)) ; missile is fired

#;
(define (fn-for-missile m)
  (cond
    [(false? m) (...)]
    [else (... (posn-x m)     ; Number
               (posn-y m))])) ; Number

; Template rules used:
; one-of: 2 cases
; - atomic distinct: false
; - compound: 2 fields

;;====================
;; Functions

;; SIGS -> SIGS
;; start the world with (main SIGS-START))
;; 
(define (main s)
  (big-bang s                                      ; SIGS
    (on-tick   si-move)                            ; SIGS -> SIGS
    (to-draw   si-render)                          ; SIGS -> Image
    (stop-when si-game-over? si-render-final)      ; SIGS -> Boolean
    (on-key    si-control)))                       ; SIGS KeyEvent -> SIGS

; SIGS -> Image
; renders the given game state on top of SCENE
(check-expect (si-render (make-sigs (make-posn 10 20) (make-tank 28 -3) #false)) (place-image UFO 10 20 (place-image TANK 28 TANK-Y SCENE)))
(check-expect (si-render (make-sigs (make-posn 20 100) (make-tank 100 3) (make-posn 22 103))) (place-image MISSILE 22 103 (place-image UFO 20 100 (place-image TANK 100 TANK-Y SCENE))))
(check-expect (si-render (make-sigs (make-posn 10 20) (make-tank 28 -3) (make-posn 32 (- SCENE-HEIGHT TANK-HEIGHT 10)))) (place-image MISSILE 32 (- SCENE-HEIGHT TANK-HEIGHT 10) (place-image UFO 10 20 (place-image TANK 28 TANK-Y SCENE))))

;(define (si-render s) SCENE) ; stub

; Template from SIGS

(define (si-render s)
  (missile-render.v2 (sigs-missile s)           
                     (ufo-render (sigs-ufo s)
                                 (tank-render (sigs-tank s) SCENE))))

; MissileOrNot Image -> Image 
; adds an image of missile m to scene s
(check-expect (missile-render.v2 #false SCENE) SCENE)
(check-expect (missile-render.v2 (make-posn 32 (- SCENE-HEIGHT TANK-HEIGHT 10)) SCENE) (place-image MISSILE 32 (- SCENE-HEIGHT TANK-HEIGHT 10) SCENE))
              
#;
(define (missile-render.v2 m s)
  (cond
    [(boolean? m) ...]
    [(posn? m) (... (posn-x m) ... (posn-y m) ...)]))

(define (missile-render.v2 m s)
  (cond
    [(boolean? m) s]
    [(posn? m)
     (place-image MISSILE (posn-x m) (posn-y m) s)]))

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

(define (si-move s)
  (si-move-proper s (ufo-random-x UFO-MAX-JUMP-LEN)))

;; SIGS Number -> SIGS
;; moves the space-invader objects predictably by ufo-delta
(check-expect (si-move-proper (make-sigs (make-posn 10 20) (make-tank 28 -3) #false) 3)
              (make-sigs (make-posn (+ 10 3) (+ 20 UFO-SPEED)) (make-tank (+ 28 -3) -3) #false))
(check-expect (si-move-proper (make-sigs (make-posn 20 100) (make-tank 100 3) (make-posn 22 103)) -1)
              (make-sigs (make-posn (+ 20 -1) (+ 100 UFO-SPEED)) (make-tank (+ 100 3) 3) (make-posn 22 (- 103 MISSILE-SPEED))))

;(define (si-move-proper s ufo-delta) s) ; stub

; Template from SIGS

(define (si-move-proper s ufo-delta)
  (make-sigs (ufo-move (sigs-ufo s) ufo-delta)           
             (tank-move (sigs-tank s))         
             (missile-move (sigs-missile s)))) 

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
; (define (ufo-move u delta-x) u) ; stub

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

;; MissileOrNot -> MissileOrNot
;; moves the given missile by MISSILE-SPEED
(check-expect (missile-move #false) #false)
(check-expect (missile-move (make-posn 22 103)) (make-posn 22 (- 103 MISSILE-SPEED)))

;(define (missile-move m) m) ; stub

; Template from MissileOrNot

(define (missile-move m)
  (cond
    [(false? m) false]
    [else (make-posn (posn-x m)   
                     (- (posn-y m) MISSILE-SPEED))]))


;; SIGS -> Boolean
;; stops the game if the UFO lands or if the missile hits the UFO
(check-expect (si-game-over? (make-sigs (make-posn 30 UFO-LANDED) ; aim - landed. landing proximity?
                                        (make-tank 60 5) #false)) true) 
(check-expect (si-game-over? (make-sigs (make-posn 30 (/ SCENE-HEIGHT 2)) ; aim - not landed
                                        (make-tank 60 5) #false)) false) 

(check-expect (si-game-over? (make-sigs (make-posn 30 70)         ; fired - hit. hit proximity? 
                                        (make-tank 60 5)
                                        (make-posn 30 (+ 70 MISSILE-MID-HEIGHT)))) true) 
(check-expect (si-game-over? (make-sigs (make-posn 30 UFO-LANDED) ; fired - landed. landing proximity?
                                        (make-tank 60 5)
                                        (make-posn 70 45))) true)
(check-expect (si-game-over? (make-sigs (make-posn 30 150) ; fired - neither hit nor landed
                                        (make-tank 60 5)
                                        (make-posn 70 45))) false)

; (define (si-game-over? s) false) ; stub

; Template from SIGS

(define (si-game-over? s)
  (or (ufo-landed? (sigs-ufo s))           
      (missile-hit-ufo? (sigs-ufo s) (sigs-missile s))))

;; UFO -> Boolean
;; produces true if UFO distance from ground <= UFO-LANDED-PROXIMITY
(check-expect (ufo-landed? (make-posn 30 (/ SCENE-HEIGHT 2))) false) ; UFO far from ground
(check-expect (ufo-landed? (make-posn 30  (- SCENE-HEIGHT (+ UFO-LANDED-PROXIMITY UFO-MID-HEIGHT)))) true) ;UFO UFO-LANDED-PROXIMITY from Ground
(check-expect (ufo-landed? (make-posn 30  (- SCENE-HEIGHT (- UFO-LANDED-PROXIMITY UFO-MID-HEIGHT 1)))) false) ;UFO almost UFO-LANDED-PROXIMITY from Ground

;(define (ufo-landed? u) false) ;stub

; template from UFO

(define (ufo-landed? u)
  (<= (posn-distance (make-posn (posn-x u) SCENE-HEIGHT) 
                     (make-posn (posn-x u) (+ (posn-y u) UFO-MID-HEIGHT)))
      UFO-LANDED-PROXIMITY))

;; UFO Missile -> Boolean
;;  produces true distance from UFO to missile is <= UFO-MISSILE-HIT-PROXIMITY
(check-expect (missile-hit-ufo? (make-posn 30 70) #false) #false)
(check-expect (missile-hit-ufo? (make-posn 30 70) (make-posn 80 80)) false) ; missile far away             
(check-expect (missile-hit-ufo? (make-posn 30 70) (make-posn 30 70)) true) ; hit. exact same
(check-expect (missile-hit-ufo? (make-posn 30 70) (make-posn 30 (- 70 UFO-MISSILE-HIT-PROXIMITY))) true) ; hit. distance equals UFO-MISSILE-HIT-PROXIMITY
(check-expect (missile-hit-ufo? (make-posn 30 70) (make-posn 30 (- 70 (- UFO-MISSILE-HIT-PROXIMITY 0.5)))) true)  ; hit. distance less than UFO-MISSILE-HIT-PROXIMITY

; (define (missile-hit-ufo? u m) false) ; stub

(define (missile-hit-ufo? u m)
  (cond
    [(false? m) false]
    [else (<= (posn-distance u m)
      UFO-MISSILE-HIT-PROXIMITY)]))

;; Posn Posn -> Number
;; produces distance between p1 and p2
(check-within (posn-distance (make-posn 1 2) (make-posn 3 4))
              (sqrt (+ (sqr (- 1 3))
                       (sqr (-  2 4))))
              0.01)

;(define (posn-distance p1 p2) 0) ;stub

(define (posn-distance p1 p2)
  (sqrt (+ (sqr (- (posn-x p1) (posn-x p2)))
           (sqr (- (posn-y p1) (posn-y p2))))))

;; SIGS -> Image
;; renders the game over screen
(check-expect (si-render-final (make-sigs (make-posn 30 70)
                                          (make-tank 60 5)
                                          false))
              (place-image (text "GAME OVER" 50 "black") GAME-OVER-X GAME-OVER-Y (si-render (make-sigs (make-posn 30 70)
                                                                                                       (make-tank 60 5)
                                                                                                       false))))
(check-expect (si-render-final (make-sigs (make-posn 30 70)         
                                          (make-tank 60 5)
                                          (make-posn 30 (+ 70 MISSILE-MID-HEIGHT))))
              (place-image (text "GAME OVER" 50 "black") GAME-OVER-X GAME-OVER-Y (si-render (make-sigs (make-posn 30 70)
                                                                                                       (make-tank 60 5)
                                                                                                       (make-posn 30 (+ 70 MISSILE-MID-HEIGHT))))))
;(define (si-render-final s) SCENE) ; stub

(define (si-render-final s)
  (place-image (text "GAME OVER" 50 "black") GAME-OVER-X GAME-OVER-Y (si-render s)))

;; SIGS KeyEvent -> SIGS
;; changes the direction of the tank with the left and right arrows; fires a missile with the space key if it hasn’t been launched yet
(check-expect (si-control (make-sigs (make-posn 30 20) (make-tank 45 3) false) "left")   ; Not fired - going right, left
              (make-sigs (make-posn 30 20) (make-tank 45 -3) false))                                                
(check-expect (si-control (make-sigs (make-posn 30 20) (make-tank 45 -3) false) "right") ; Not fired - going left, right
              (make-sigs (make-posn 30 20) (make-tank 45 3) false))                                              
(check-expect (si-control (make-sigs (make-posn 60 40) (make-tank 50 -4) false) "left")  ; Not fired - going left, left
              (make-sigs  (make-posn 60 40) (make-tank 50 -4) false))                                             
(check-expect (si-control (make-sigs (make-posn 60 40) (make-tank 50 4) false) "right") ; Not fired - going right, right
              (make-sigs (make-posn 60 40) (make-tank 50 4) false))                                             
(check-expect (si-control (make-sigs (make-posn 60 40) (make-tank 50 -4) false) " ") ; Not fired - fire
              (make-sigs (make-posn 60 40) (make-tank 50 -4) (make-posn 50 MISSILE-LAUNCH-HEIGHT))) 
(check-expect (si-control (make-sigs (make-posn 60 40) (make-tank 50 -4) false) "up")    ; Not fired - up key
              (make-sigs (make-posn 60 40) (make-tank 50 -4) false)) 

(check-expect (si-control (make-sigs (make-posn 70 40) (make-tank 90 1) (make-posn 55 33)) "left")   ; Fired - going right, left
              (make-sigs (make-posn 70 40) (make-tank 90 -1) (make-posn 55 33))) 
(check-expect (si-control (make-sigs (make-posn 70 40) (make-tank 90 -1) (make-posn 55 33)) "right") ; Fired - going left, right
              (make-sigs (make-posn 70 40) (make-tank 90 1) (make-posn 55 33)))
(check-expect (si-control (make-sigs (make-posn 60 30) (make-tank 80 -4) (make-posn 55 33)) "left")  ; Fired - going left, left
              (make-sigs (make-posn 60 30) (make-tank 80 -4) (make-posn 55 33)))
(check-expect (si-control (make-sigs (make-posn 60 30) (make-tank 80 4) (make-posn 55 33)) "right")  ; Fired - going right, right
              (make-sigs (make-posn 60 30) (make-tank 80 4) (make-posn 55 33)))
(check-expect (si-control (make-sigs (make-posn 100 90) (make-tank 40 2) (make-posn 50 50)) " ") ; Fired - fire
              (make-sigs (make-posn 100 90) (make-tank 40 2) (make-posn 50 50))) 
(check-expect (si-control (make-sigs (make-posn 100 90) (make-tank 40 2) (make-posn 50 50)) "down") ; Fired - down key
              (make-sigs (make-posn 100 90) (make-tank 40 2) (make-posn 50 50)))

;(define (si-control s ke) s) ;stub

#;
(define (fn-for-key-event s kevt)
  (cond [(key=? " " kevt) (... (fn-for-ufo (sigs-ufo s))              ; UFO
                               (fn-for-missile (sigs-missile s))      ; MissileOrNot
                               (fn-for-tank (sigs-tank s)))]          ; Tank
        [(key=? "right" kevt) (... (fn-for-ufo (sigs-ufo s))          ; UFO
                                   (fn-for-missile (sigs-missile s))  ; MissileOrNot
                                   (fn-for-tank (sigs-tank s)))]      ; Tank
        [(key=? "left" kevt)  (... (fn-for-ufo (sigs-ufo s))          ; UFO
                                   (fn-for-missile (sigs-missile s))  ; MissileOrNot
                                   (fn-for-tank (sigs-tank s)))]      ; Tank
        [else
         (... (fn-for-ufo (sigs-ufo s))                               ; UFO
              (fn-for-missile (sigs-missile s))                       ; MissileOrNot
              (fn-for-tank (sigs-tank s)))]))                         ;Tank

; Template formed using the large enumeration special case

(define (si-control s kevt)
  (cond [(key=? " " kevt) (make-sigs (sigs-ufo s)    
                                     (sigs-tank s)
                                     (tank-fire-missile (sigs-tank s) (sigs-missile s)))]        
        [(key=? "right" kevt) (make-sigs (sigs-ufo s)         
                                         (tank-change-dir-right (sigs-tank s))
                                         (sigs-missile s))]      
        [(key=? "left" kevt)  (make-sigs (sigs-ufo s)         
                                         (tank-change-dir-left (sigs-tank s))
                                         (sigs-missile s))]      
        [else s]))

;; Tank MissileOrNot -> MissileOrNot
;; produces a fired missile at tank x if not already fired
(check-expect (tank-fire-missile (make-tank 50 -4) false) (make-posn 50 MISSILE-LAUNCH-HEIGHT)) ; not fired
(check-expect (tank-fire-missile  (make-tank 40 2) (make-posn 50 50)) (make-posn 50 50))

;(define (tank-fire-missile t m) m) ; stub

; Template from Missile

(define (tank-fire-missile t m)
  (cond
    [(false? m) (make-posn (tank-loc t) MISSILE-LAUNCH-HEIGHT)]
    [else m]))

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






