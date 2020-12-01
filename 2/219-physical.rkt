;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |219|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Constants:

(define HEIGHT 300)
(define WIDTH 500)
(define MTSCN (empty-scene WIDTH HEIGHT))
(define SEGMENT-R 5)
(define SEGMENT-COLOR "red")
(define FOOD-COLOR "green")
(define SEGMENT (circle SEGMENT-R "solid" SEGMENT-COLOR))
(define FOOD (circle SEGMENT-R "solid" FOOD-COLOR))
(define DIAMETER (* SEGMENT-R 2))
(define WORM-POS-START-X (/ WIDTH 2))
(define WORM-POS-START-Y (/ HEIGHT 2))
(define WORM-DIRECTION-START "right")
(define WORM-SPEED DIAMETER)
(define TICK-RATE 0.5)
(define AT-TOP-WALL SEGMENT-R)
(define AT-BOTTOM-WALL (- HEIGHT SEGMENT-R))
(define AT-LEFT-WALL SEGMENT-R)
(define AT-RIGHT-WALL (- WIDTH SEGMENT-R))
(define HIT-WALL-MSG "worm hit wall")
(define RAN-INTO-SELF-MSG "worm ran into self")
(define DISPLAY-STATE #true)
(define MAX-FOOD-X (- WIDTH SEGMENT-R SEGMENT-R))
(define MAX-FOOD-Y (- HEIGHT SEGMENT-R SEGMENT-R)) 

;; Data definitions:

;; A direction is one of:
;; - "up"
;; - "down"
;; - "left"
;; - "right"

(define (fn-for-direction d)
  (cond [(string=? d "up") (...)]
        [(string=? d "down") (...)]
        [(string=? d "left") (...)]
        [(string=? d "right") (...)]))

;; Template rules used:
;; - one of: 4 cases
;; - atomic distinct: "up"
;; - atomic distinct: "down"
;; - atomic distinct: "left"
;; - atomic distinct: "right"

;; NELoP is one of:
;; - (cons Posn empty)
;; - (cons Posn NELoP)
;; interp. A list of Posns with at least one posn
(define LOP1 (make-posn 40 10))
(define LOP2 (list (make-posn 40 10) (make-posn 80 100)))

(define (fn-for-nelop nelop)
  (cond [(empty? (rest nelop)) (... (fn-for-posn (first nelop)))]
        [else
         (... (fn-for-posn (first nelop))     ;Posn
              (fn-for-nelop (rest nelop)))])) ;NELoP

;; Template rules used:
;; - one of: 2 cases
;; - compound: (cons Posn empty)
;; - reference: (first lop) is Posn
;; - compound: (cons Posn NELoP)
;; - reference: (first lop) is Posn
;; - self-reference: (rest lop) is NELoP

(define-struct worm [direction segments])
;; a Worm is (make-worm Direction NELoP)
;; interp. a worm with its direction and segments
;; A worm will alway have at least one segment (it's head).
;; The head is the first of the list. and the tail grows towards the end of the list 
(define W1 (make-worm "right" (list (make-posn 40 10))))                                     ;just a head
(define W2 (make-worm "right" (list (make-posn 40 10) (make-posn (+ 40 WORM-SPEED) 10))))    ;two segments, straight line
(define W3 (make-worm "right"                                                                ;4 segments, straight line
                      (list (make-posn 40 10)
                            (make-posn (+ 40 WORM-SPEED) 10)
                            (make-posn (+ (+ 40 WORM-SPEED) WORM-SPEED) 10)
                            (make-posn (+(+ (+ 40 WORM-SPEED) WORM-SPEED) WORM-SPEED) 10))))
(define W4 (make-worm "up"                                                                    ;5 segments, straight line and up at head
                      (list (make-posn 40 10)
                            (make-posn (+ 40 WORM-SPEED) 10)
                            (make-posn (+ (+ 40 WORM-SPEED) WORM-SPEED) 10)
                            (make-posn (+(+ (+ 40 WORM-SPEED) WORM-SPEED) WORM-SPEED) 10)
                            (make-posn (+ (+ (+ 40 WORM-SPEED) WORM-SPEED) (+ 10 WORM-SPEED)) 10))))
(define WORM-START (make-worm WORM-DIRECTION-START                                   
                              (list (make-posn (+ (+ (+ (+ (+ (+ WORM-POS-START-X WORM-SPEED) WORM-SPEED) WORM-SPEED) WORM-SPEED) WORM-SPEED) WORM-SPEED) (- (- (- WORM-POS-START-Y WORM-SPEED) WORM-SPEED) WORM-SPEED))
                                    (make-posn (+ (+ (+ (+ (+ WORM-POS-START-X WORM-SPEED) WORM-SPEED) WORM-SPEED) WORM-SPEED) WORM-SPEED) (- (- (- WORM-POS-START-Y WORM-SPEED) WORM-SPEED) WORM-SPEED))
                                    (make-posn (+ (+ (+ (+ WORM-POS-START-X WORM-SPEED) WORM-SPEED) WORM-SPEED) WORM-SPEED) (- (- (- WORM-POS-START-Y WORM-SPEED) WORM-SPEED) WORM-SPEED))
                                    (make-posn (+ (+ (+ WORM-POS-START-X WORM-SPEED) WORM-SPEED) WORM-SPEED) (- (- (- WORM-POS-START-Y WORM-SPEED) WORM-SPEED) WORM-SPEED))
                                    (make-posn (+ (+ (+ WORM-POS-START-X WORM-SPEED) WORM-SPEED) WORM-SPEED) (- (- WORM-POS-START-Y WORM-SPEED) WORM-SPEED))
                                    (make-posn (+ (+ (+ WORM-POS-START-X WORM-SPEED) WORM-SPEED) WORM-SPEED) (- WORM-POS-START-Y WORM-SPEED))
                                    (make-posn (+ (+ (+ WORM-POS-START-X WORM-SPEED) WORM-SPEED) WORM-SPEED) WORM-POS-START-Y)
                                    (make-posn (+ (+ WORM-POS-START-X WORM-SPEED) WORM-SPEED) WORM-POS-START-Y)
                                    (make-posn (+ WORM-POS-START-X WORM-SPEED) WORM-POS-START-Y)
                                    (make-posn WORM-POS-START-X WORM-POS-START-Y))))

(define (fn-for-worm w)
  (... (fn-for-direction (worm-direction w)) ;Direction
       (fn-for-lop (worm-segments w))))        ;NELoP

;; Template rules used:
;; - compound: 2 fields
;; - reference: (first w) is Direction
;; - reference: (rest w) is ListOfPosn

(define-struct worm-game [worm food])
;; WormGame is (make-worm-game Worm Posn)
;; interp. The worm game state. the worm and the current position of the food

(define WG1 (make-worm-game (make-worm "right" (list (make-posn 50 50))) (make-posn 45 50)))
(define WG2 (make-worm-game (make-worm "right" (list (make-posn 50 50))) (make-posn 20 50)))

(define (fn-for-worm-game wg)
  (... (fn-for-worm (worm-game-worm wg))   ;Worm
       (fn-for-posn (worm-game-food wg)))) ;Posn

;; Template rules used:
;; - compound: 2 fields:
;; - reference: (worm-game-worm wg) is Worm
;; - reference: (worm-game-food wg) is Posn

;; Functions:

;; Number Boolean -> WormGame
;; start the world with (worm-main TICK-RATE DISPLAY-STATE)
;; 
(define (worm-main rate display-state)
  (big-bang (make-worm-game WORM-START (food-create (make-posn 0 0)))        ; WormGame
    (on-tick   handle-tick rate)              ; WormGame -> WormGame
    (to-draw   render-worm-game)              ; WormGame -> Image
    (stop-when game-over? render-game-over)   ; WormGame -> ???
    (on-key    handle-key)                    ; WormGame KeyEvent -> WormGame
    (state display-state)))                   ; ???

;; WormGame -> WormGame
;; moves the worm and handles eating and food creation
(check-expect (handle-tick (make-worm-game (make-worm "right" (list (make-posn 50 50))) (make-posn 100 200)))      ;worm moves right
              (make-worm-game (make-worm "right" (list (make-posn (+ 50 WORM-SPEED) 50))) (make-posn 100 200)))
(check-random (handle-tick (make-worm-game (make-worm "right" (list (make-posn 50 50))) (make-posn (+ 50 WORM-SPEED) 50))) ;worm will eat food
              (make-worm-game (make-worm "right" (list (make-posn (+ 50 WORM-SPEED) 50) (make-posn 50 50))) (make-posn (+ (random (+ MAX-FOOD-X 1)) SEGMENT-R)
                                                                                                                       (+ (random (+ MAX-FOOD-Y 1)) SEGMENT-R))))
              
;(define (handle-tick wg) wg) ;stub

(define (handle-tick wg)
  (if (will-worm-eat? (worm-game-worm wg) (worm-game-food wg))   
      (make-worm-game (worm-eat (worm-game-worm wg))
                      (food-create (worm-game-food wg)))
      (make-worm-game (move-worm (worm-game-worm wg))
                      (worm-game-food wg))))

;; Worm Posn -> Boolean
;; produces true if worm's head will be located at the same position as the food after moving
(check-expect (will-worm-eat? (make-worm "right" (list (make-posn 50 50))) (make-posn 100 200))
              #false)
(check-expect (will-worm-eat? (make-worm "right" (list (make-posn 50 50))) (make-posn (+ 50 WORM-SPEED) 50))
              #true)
(check-expect (will-worm-eat? (make-worm "left" (list (make-posn 50 50))) (make-posn (- 50 WORM-SPEED) 50))
              #true)
(check-expect (will-worm-eat? (make-worm "up" (list (make-posn 50 50))) (make-posn 50 (- 50 WORM-SPEED)))
              #true)
(check-expect (will-worm-eat? (make-worm "down" (list (make-posn 50 50))) (make-posn 50 (+ 50 WORM-SPEED)))
              #true)

;(define (will-worm-eat? w food) #false) ;stub

(define (will-worm-eat? w food)
  (worm-head-at-posn? (move-worm w) food))

;; Worm Posn -> Boolean
;; produces true if worm's head is at given posn
(check-expect (worm-head-at-posn? (make-worm "right" (list (make-posn 50 50))) (make-posn 50 50))
              #true)
(check-expect (worm-head-at-posn? (make-worm "right" (list (make-posn 50 50))) (make-posn 70 50))
              #false)

;(define (worm-head-at-posn? w p) #false) ;stub

(define (worm-head-at-posn? w p)
  (posn=? (first (worm-segments w)) p))      

;; Posn Posn -> Boolean
;; produces true if posns are equal
(check-expect (posn=? (make-posn 1 1) (make-posn 1 1)) #true)
(check-expect (posn=? (make-posn 1 2) (make-posn 1 1)) #false)
(check-expect (posn=? (make-posn 1 2) (make-posn 4 2)) #false)

;(define (posn=? p1 p2) #false)

;; Template from Posn

(define (posn=? p1 p2)
  (and (= (posn-x p1)
          (posn-x p2))
       (= (posn-y p1)
          (posn-y p2))))

;; Worm -> Worm
;; produces a worm that has eaten. The head moves and we do not remove the tail
(check-expect (worm-eat (make-worm "right" (list (make-posn 50 50))))
              (make-worm "right" (list (make-posn (+ 50 WORM-SPEED) 50) (make-posn 50 50))))
(check-expect (worm-eat (make-worm "left" (list (make-posn 50 50))))
              (make-worm "left" (list (make-posn (- 50 WORM-SPEED) 50) (make-posn 50 50))))
(check-expect (worm-eat (make-worm "up" (list (make-posn 50 50))))
              (make-worm "up" (list (make-posn 50 (- 50 WORM-SPEED)) (make-posn 50 50))))
(check-expect (worm-eat (make-worm "down" (list (make-posn 50 50))))
              (make-worm "down" (list (make-posn 50 (+ 50 WORM-SPEED)) (make-posn 50 50))))

;(define (worm-eat w) w) ;stub

(define (worm-eat w)
  (make-worm (worm-direction w)
             (grow-segments-in-direction (worm-segments w) (worm-direction w))))

;; NELoP Direction -> NELoP
;; grows segments in given direction
(check-expect (grow-segments-in-direction (list (make-posn 50 50)) "right")
              (list (make-posn (+ 50 WORM-SPEED) 50) (make-posn 50 50)))
(check-expect (grow-segments-in-direction (list (make-posn 50 50)) "left")
              (list (make-posn (- 50 WORM-SPEED) 50) (make-posn 50 50)))
(check-expect (grow-segments-in-direction (list (make-posn 50 50)) "up")
              (list (make-posn 50 (- 50 WORM-SPEED)) (make-posn 50 50)))
(check-expect (grow-segments-in-direction (list (make-posn 50 50)) "down")
              (list (make-posn 50 (+ 50 WORM-SPEED)) (make-posn 50 50)))

;(define (grow-segments-in-direction nelop d) nelop) ;stub

(define (grow-segments-in-direction nelop d)
  (cond [(string=? d "up") (grow-segments-up nelop)]
        [(string=? d "down") (grow-segments-down nelop)]
        [(string=? d "left") (grow-segments-left nelop)]
        [(string=? d "right") (grow-segments-right nelop)]))

;; NELoP -> NELoP
;; segments grow upwards by WORM-SPEED
(check-expect (grow-segments-up (list (make-posn 50 50)))
              (list (make-posn 50 (- 50 WORM-SPEED)) (make-posn 50 50)))
(check-expect (grow-segments-up (list (make-posn 50 (- 50 WORM-SPEED)) (make-posn 50 50)))
              (list (make-posn 50 (- (- 50 WORM-SPEED) WORM-SPEED)) (make-posn 50 (- 50 WORM-SPEED)) (make-posn 50 50)))

;(define (grow-segments-up nelop) nelop) ;stub

(define (grow-segments-up nelop)
  (cons (move-segment-up (first nelop) WORM-SPEED) nelop))
        
;; NELoP -> NELoP
;; segments grow downwards by WORM-SPEED
(check-expect (grow-segments-down (list (make-posn 50 50)))
              (list (make-posn 50 (+ 50 WORM-SPEED)) (make-posn 50 50)))
(check-expect (grow-segments-down (list (make-posn 50 (+ 50 WORM-SPEED)) (make-posn 50 50)))
              (list (make-posn 50 (+ (+ 50 WORM-SPEED) WORM-SPEED)) (make-posn 50 (+ 50 WORM-SPEED)) (make-posn 50 50)))

;(define (grow-segments-down nelop) nelop)

(define (grow-segments-down nelop)
  (cons (move-segment-down (first nelop) WORM-SPEED) nelop))

;; NELoP -> NELoP
;; segments grow left by WORM-SPEED
(check-expect (grow-segments-left (list (make-posn 50 50)))
              (list (make-posn (- 50 WORM-SPEED) 50) (make-posn 50 50)))
(check-expect (grow-segments-left (list (make-posn (- 50 WORM-SPEED) 50) (make-posn 50 50)))
              (list (make-posn (- (- 50 WORM-SPEED) WORM-SPEED) 50) (make-posn (- 50 WORM-SPEED) 50) (make-posn 50 50)))

;(define (grow-segments-left nelop) nelop) ;stub

(define (grow-segments-left nelop)
  (cons (move-segment-left (first nelop) WORM-SPEED) nelop))

;; NELoP -> NELoP
;; segments grow right by WORM-SPEED
(check-expect (grow-segments-right (list (make-posn 50 50)))
              (list (make-posn (+ 50 WORM-SPEED) 50) (make-posn 50 50)))
(check-expect (grow-segments-right (list (make-posn (+ 50 WORM-SPEED) 50) (make-posn 50 50)))
              (list (make-posn (+ (+ 50 WORM-SPEED) WORM-SPEED) 50) (make-posn (+ 50 WORM-SPEED) 50) (make-posn 50 50)))

;(define (grow-segments-right nelop) nelop) ;stub

(define (grow-segments-right nelop)
  (cons (move-segment-right (first nelop) WORM-SPEED) nelop))

;; Worm -> Worm
;; Moves the worm in the direction it is moving in by WORM-SPEED
(check-expect (move-worm (make-worm "up" (list (make-posn 50 50))))                                    
              (make-worm "up" (list (make-posn 50 (- 50 WORM-SPEED)))))
(check-expect (move-worm (make-worm "down" (list (make-posn 50 50))))                                    
              (make-worm "down" (list (make-posn 50 (+ 50 WORM-SPEED)))))
(check-expect (move-worm (make-worm "right" (list (make-posn 50 50))))                                    
              (make-worm "right" (list (make-posn (+ 50 WORM-SPEED) 50))))
(check-expect (move-worm (make-worm "left" (list (make-posn 50 50))))                                    
              (make-worm "left" (list (make-posn (- 50 WORM-SPEED) 50))))

(check-expect (move-worm (make-worm "left" (list (make-posn 40 10)
                                                 (make-posn (+ 40 WORM-SPEED) 10))))
              (make-worm "left" (list (make-posn (- 40 WORM-SPEED) 10)
                                      (make-posn 40 10))))
(check-expect (move-worm (make-worm "right"
                                    (list (make-posn (+ 40 WORM-SPEED) 10)
                                          (make-posn 40 10))))
              (make-worm "right" (list (make-posn (+ (+ 40 WORM-SPEED) WORM-SPEED) 10)
                                       (make-posn (+ 40 WORM-SPEED) 10))))
(check-expect (move-worm (make-worm "up" (list (make-posn 30 50)
                                               (make-posn 30 (+ 50 WORM-SPEED)))))                                    
              (make-worm "up" (list (make-posn 30 (- 50 WORM-SPEED))
                                    (make-posn 30 50))))
(check-expect (move-worm (make-worm "down" (list (make-posn 30 (+ 50 WORM-SPEED))
                                                 (make-posn 3 50))))                                    
              (make-worm "down" (list (make-posn 30 (+ (+ 50 WORM-SPEED )WORM-SPEED))
                                      (make-posn 30 (+ 50 WORM-SPEED)))))
(check-expect (move-worm (make-worm "up" (list (make-posn 40 (- (- (- (- 100 WORM-SPEED) WORM-SPEED) WORM-SPEED) WORM-SPEED))
                                               (make-posn 40 (- (- (- 100 WORM-SPEED) WORM-SPEED) WORM-SPEED))
                                               (make-posn 40 (- (- 100 WORM-SPEED) WORM-SPEED))
                                               (make-posn 40 (- 100 WORM-SPEED))
                                               (make-posn 40 100))))
              (make-worm "up" (list (make-posn 40 (- (- (- (- (- 100 WORM-SPEED) WORM-SPEED) WORM-SPEED) WORM-SPEED) WORM-SPEED))
                                    (make-posn 40 (- (- (- (- 100 WORM-SPEED) WORM-SPEED) WORM-SPEED) WORM-SPEED))
                                    (make-posn 40 (- (- (- 100 WORM-SPEED) WORM-SPEED) WORM-SPEED))
                                    (make-posn 40 (- (- 100 WORM-SPEED) WORM-SPEED))
                                    (make-posn 40 (- 100 WORM-SPEED)))))

;(define (move-worm w) w) ;stub

(define (move-worm w)
  (make-worm (worm-direction w)
             (move-segments-in-direction (worm-segments w) (worm-direction w))))

;; NELoP Direction -> NELoP
;; moves the segments in given direction.
(check-expect (move-segments-in-direction (list (make-posn 50 50)) "up")
              (list (make-posn 50 (- 50 WORM-SPEED))))
(check-expect (move-segments-in-direction (list (make-posn 50 50)) "down")
              (list (make-posn 50 (+ 50 WORM-SPEED))))
(check-expect (move-segments-in-direction (list (make-posn 50 50)) "right")                                    
              (list (make-posn (+ 50 WORM-SPEED) 50)))
(check-expect (move-segments-in-direction (list (make-posn 50 50)) "left")                                    
              (list (make-posn (- 50 WORM-SPEED) 50)))
(check-expect (move-segments-in-direction (list (make-posn 40 10)
                                                (make-posn (+ 40 WORM-SPEED) 10))
                                          "left")
              (list (make-posn (- 40 WORM-SPEED) 10)
                    (make-posn 40 10)))
(check-expect (move-segments-in-direction (list (make-posn (+ 40 WORM-SPEED) 10)
                                                (make-posn 40 10))
                                          "right")
              (list (make-posn (+ (+ 40 WORM-SPEED) WORM-SPEED) 10)
                    (make-posn (+ 40 WORM-SPEED) 10)))

(check-expect (move-segments-in-direction (list (make-posn 30 50)
                                                (make-posn 30 (+ 50 WORM-SPEED)))
                                          "up")
              (list (make-posn 30 (- 50 WORM-SPEED))
                    (make-posn 30 50)))
(check-expect (move-segments-in-direction (list (make-posn 30 (+ 50 WORM-SPEED))
                                                (make-posn 3 50))
                                          "down")
              (list (make-posn 30 (+ (+ 50 WORM-SPEED )WORM-SPEED))
                    (make-posn 30 (+ 50 WORM-SPEED))))
(check-expect (move-segments-in-direction (list (make-posn 40 (- (- (- (- 100 WORM-SPEED) WORM-SPEED) WORM-SPEED) WORM-SPEED))
                                                (make-posn 40 (- (- (- 100 WORM-SPEED) WORM-SPEED) WORM-SPEED))
                                                (make-posn 40 (- (- 100 WORM-SPEED) WORM-SPEED))
                                                (make-posn 40 (- 100 WORM-SPEED))
                                                (make-posn 40 100)) "up")
              (list (make-posn 40 (- (- (- (- (- 100 WORM-SPEED) WORM-SPEED) WORM-SPEED) WORM-SPEED) WORM-SPEED))
                    (make-posn 40 (- (- (- (- 100 WORM-SPEED) WORM-SPEED) WORM-SPEED) WORM-SPEED))
                    (make-posn 40 (- (- (- 100 WORM-SPEED) WORM-SPEED) WORM-SPEED))
                    (make-posn 40 (- (- 100 WORM-SPEED) WORM-SPEED))
                    (make-posn 40 (- 100 WORM-SPEED))))

;(define (move-segments-in-direction nelop d) nelop) ;stub

(define (move-segments-in-direction nelop d)
  (cond [(string=? d "up") (move-segments-up nelop)]
        [(string=? d "down") (move-segments-down nelop)]
        [(string=? d "left") (move-segments-left nelop)]
        [(string=? d "right") (move-segments-right nelop)]))

;; NELoP -> NELoP
;; moves segments up
(check-expect (move-segments-up (list (make-posn 50 50)))
              (list (make-posn 50 (- 50 WORM-SPEED))))
(check-expect (move-segments-up (list (make-posn 30 50)
                                      (make-posn 30 (+ 50 WORM-SPEED))))
              (list (make-posn 30 (- 50 WORM-SPEED))
                    (make-posn 30 50)))
(check-expect (move-segments-up (list (make-posn 30 (- 50 WORM-SPEED))
                                      (make-posn 30 50)
                                      (make-posn 30 (+ 50 WORM-SPEED))))
              (list (make-posn 30 (- (- 50 WORM-SPEED) WORM-SPEED))
                    (make-posn 30 (- 50 WORM-SPEED))
                    (make-posn 30 50)))

;(define (move-segments-up nelop) nelop) ;stub

(define (move-segments-up nelop)
  (cond [(empty? (rest nelop)) (list (move-segment-up (first nelop) WORM-SPEED))]
        [else
         (cons (move-segment-up (first nelop) WORM-SPEED)     
               (remove-last-segment nelop))])) 

;; Posn n -> Posn
;; moves posn up by n
(check-expect (move-segment-up (make-posn 50 50) WORM-SPEED)
              (make-posn 50 (- 50 WORM-SPEED)))

;(define (move-segment-up p n) p) ;stub

;; Template from Posn

(define (move-segment-up p n)
  (make-posn (posn-x p)
             (- (posn-y p) n)))

;; NELoP -> ListOfPosn
;; removes last posn from list
(check-expect (remove-last-segment (list (make-posn 80 20))) empty)
(check-expect (remove-last-segment (list  (make-posn 60 70)
                                          (make-posn 80 20)))
              (list (make-posn 60 70)))
(check-expect (remove-last-segment (list (make-posn 50 30)
                                         (make-posn 60 70)
                                         (make-posn 80 20)))
              (list (make-posn 50 30)
                    (make-posn 60 70)))

;(define (remove-last-segment lop) lop) ;stub

(define (remove-last-segment nelop)
  (cond [(empty? (rest nelop)) empty]
        [else
         (cons (first nelop)    
               (remove-last-segment (rest nelop)))])) 

;; NELop -> NELop
;; moves segments down
(check-expect (move-segments-down (list (make-posn 50 50)))
              (list (make-posn 50 (+ 50 WORM-SPEED))))
(check-expect (move-segments-down (list (make-posn 30 (+ 50 WORM-SPEED))
                                        (make-posn 3 50)))
              (list (make-posn 30 (+ (+ 50 WORM-SPEED )WORM-SPEED))
                    (make-posn 30 (+ 50 WORM-SPEED))))

;(define (move-segments-down nelop) nelop) ;stub

(define (move-segments-down nelop)
  (cond [(empty? (rest nelop)) (list (move-segment-down (first nelop) WORM-SPEED))]
        [else
         (cons (move-segment-down (first nelop) WORM-SPEED)     
               (remove-last-segment nelop))]))

;; Posn n -> Posn
;; moves posn down by n
(check-expect (move-segment-down (make-posn 50 50) WORM-SPEED)
              (make-posn 50 (+ 50 WORM-SPEED)))

;(define (move-segment-down p n) p) ;stub

;; Template from Posn

(define (move-segment-down p n)
  (make-posn (posn-x p)
             (+ (posn-y p) n)))

;; NELop -> NELop
;; moves segments left
(check-expect (move-segments-left (list (make-posn 50 50)))
              (list (make-posn (- 50 WORM-SPEED) 50)))
(check-expect (move-segments-left (list (make-posn 40 10)
                                        (make-posn (+ 40 WORM-SPEED) 10)))
              (list (make-posn (- 40 WORM-SPEED) 10)
                    (make-posn 40 10)))

;(define (move-segments-left nelop) nelop) ;stub

(define (move-segments-left nelop)
  (cond [(empty? (rest nelop)) (list (move-segment-left (first nelop) WORM-SPEED))]
        [else
         (cons (move-segment-left (first nelop) WORM-SPEED)     
               (remove-last-segment nelop))]))

;; Posn n -> Posn
;; moves posn left by n
(check-expect (move-segment-left (make-posn 50 50) WORM-SPEED)
              (make-posn (- 50 WORM-SPEED) 50))

;(define (move-segment-left p n) p) ;stub

;; Template from Posn

(define (move-segment-left p n)
  (make-posn (- (posn-x p) n)
             (posn-y p)))

;; ListOfPosn -> ListOfPosn
;; moves segments right
(check-expect (move-segments-right (list (make-posn 50 50)))                                  
              (list (make-posn (+ 50 WORM-SPEED) 50)))
(check-expect (move-segments-right (list (make-posn (+ 40 WORM-SPEED) 10)
                                         (make-posn 40 10)))
              (list (make-posn (+ (+ 40 WORM-SPEED) WORM-SPEED) 10)
                    (make-posn (+ 40 WORM-SPEED) 10)))

;(define (move-segments-right nelop) nelop) ;stub

(define (move-segments-right nelop)
  (cond [(empty? (rest nelop)) (list (move-segment-right (first nelop) WORM-SPEED))]
        [else
         (cons (move-segment-right (first nelop) WORM-SPEED)     
               (remove-last-segment nelop))]))

;; Posn n -> Posn
;; moves posn right by n
(check-expect (move-segment-right (make-posn 50 50) WORM-SPEED)
              (make-posn (+ 50 WORM-SPEED) 50))

;(define (move-segment-right p n) p) ;stub

;; Template from Posn

(define (move-segment-right p n)
  (make-posn (+ (posn-x p) n)
             (posn-y p)))

;; WormGame -> Image
;; renders the worm game on MTSCN
(check-expect (render-worm-game (make-worm-game (make-worm "right" (list (make-posn (+ 50 WORM-SPEED) 50) (make-posn 50 50))) (make-posn 100 100))) ;worm far from food
              (place-image SEGMENT (+ 50 WORM-SPEED) 50 (place-image SEGMENT 50 50 (place-image FOOD 100 100 MTSCN))))
#;
(check-expect (render-worm-game (make-worm-game (make-worm "right" (list (make-posn (+ 50 WORM-SPEED) 50) (make-posn 50 50))) (make-posn (+ 50 WORM-SPEED) 50))) ;worm eating food
              (place-image SEGMENT (+ 50 WORM-SPEED) 50 (place-image SEGMENT 50 50 (place-image FOOD (+ 50 WORM-SPEED) 50 MTSCN))))

;(define (render-worm-game wg) MTSCN) ;stub

(define (render-worm-game wg)
  (render-food (worm-game-food wg) (render-worm (worm-game-worm wg))))

;; Posn -> Image
;; renders food on img
(check-expect (render-food (make-posn 100 100) MTSCN)
              (place-image FOOD 100 100 MTSCN))

;(define (render-food p img) img) ;stub

;; Template from Posn

(define (render-food p img)
  (place-image FOOD (posn-x p) (posn-y p) img))
  
      


;; Worm -> Image
;; render the Worm on MTSCN
(check-expect (render-worm (make-worm WORM-DIRECTION-START                                   
                                      (list (make-posn WORM-POS-START-X WORM-POS-START-Y))))
              (place-image SEGMENT WORM-POS-START-X WORM-POS-START-Y MTSCN))
(check-expect (render-worm (make-worm "right" (list (make-posn 40 10) (make-posn (+ 40 WORM-SPEED) 10))))
              (place-image SEGMENT 40 10 (place-image SEGMENT (+ 40 WORM-SPEED) 10 MTSCN)))

;(define (render-worm w) empty-image) ;stub

(define (render-worm w)
  (render-segments (worm-segments w) MTSCN))

;; ListOfPosn Image -> Image
;; renders segments onto img
(check-expect (render-segments empty MTSCN) MTSCN)
(check-expect (render-segments (list (make-posn WORM-POS-START-X WORM-POS-START-Y)) MTSCN)
              (place-image SEGMENT WORM-POS-START-X WORM-POS-START-Y MTSCN))
(check-expect (render-segments (list (make-posn 40 10) (make-posn (+ 40 WORM-SPEED) 10)) MTSCN)
              (place-image SEGMENT 40 10 (place-image SEGMENT (+ 40 WORM-SPEED) 10 MTSCN)))

;(define (render-segments lop img) img) ;stub

(define (render-segments lop img)
  (cond [(empty? lop) img]
        [else
         (render-segment (first lop)
                         (render-segments (rest lop) img))]))

;; Posn Image -> Image
;; renders SEGMENT on img at pos
(check-expect (render-segment (make-posn 40 10) MTSCN)
              (place-image SEGMENT 40 10 MTSCN))
(check-expect (render-segment (make-posn 70 90) MTSCN)
              (place-image SEGMENT 70 90 MTSCN))

;(define (render-segment p img) img) ;stub

;; Template from Posn

(define (render-segment p img)
  (place-image SEGMENT (posn-x p)
               (posn-y p)
               img))

;; WormGame KeyEvent -> Worm
;; handle the arrow keys
(check-expect (handle-key (make-worm-game (make-worm "right" (list (make-posn 40 50))) (make-posn 100 100)) "up")
              (make-worm-game (make-worm "up" (list (make-posn 40 50))) (make-posn 100 100)))
(check-expect (handle-key (make-worm-game (make-worm "right" (list (make-posn 40 50))) (make-posn 100 100)) "down")
              (make-worm-game (make-worm "down" (list (make-posn 40 50))) (make-posn 100 100)))
(check-expect (handle-key (make-worm-game (make-worm "up" (list (make-posn 40 50))) (make-posn 100 100)) "right")
              (make-worm-game (make-worm "right" (list( make-posn 40 50))) (make-posn 100 100)))
(check-expect (handle-key (make-worm-game (make-worm "up" (list (make-posn 40 50))) (make-posn 100 100)) "left")
              (make-worm-game (make-worm "left" (list( make-posn 40 50))) (make-posn 100 100)))
(check-expect (handle-key (make-worm-game (make-worm "right" (list (make-posn 40 50))) (make-posn 100 100)) "x")
              (make-worm-game (make-worm "right" (list( make-posn 40 50))) (make-posn 100 100)))

;(define (handle-key w ke) w) ;stub

#;
(define (handle-key wg ke)
  (cond [(key=? "up" ke) (... wg)]
        [(key=? "down" ke) (... wg)]
        [(key=? "left" ke) (... wg)]
        [(key=? "right" ke) (... wg)]
        [else
         (... wg)]))
;; Template formed using the large enumeration special case

(define (handle-key wg ke)
  (cond [(key=? "up" ke) (make-worm-game (set-worm-direction-up (worm-game-worm wg)) (worm-game-food wg))]
        [(key=? "down" ke) (make-worm-game (set-worm-direction-down (worm-game-worm wg)) (worm-game-food wg))]
        [(key=? "left" ke) (make-worm-game (set-worm-direction-left (worm-game-worm wg)) (worm-game-food wg))]
        [(key=? "right" ke) (make-worm-game (set-worm-direction-right (worm-game-worm wg)) (worm-game-food wg))]
        [else wg]))

;; Worm -> Worm
;; sets direction of worm to up. can go up only if not going down
(check-expect (set-worm-direction-up (make-worm "right" (list (make-posn 40 50))))
              (make-worm "up" (list (make-posn 40 50))))
(check-expect (set-worm-direction-up (make-worm "left" (list (make-posn 40 50))))
              (make-worm "up" (list (make-posn 40 50))))
(check-expect (set-worm-direction-up (make-worm "up" (list (make-posn 40 50))))
              (make-worm "up" (list (make-posn 40 50))))
(check-expect (set-worm-direction-up (make-worm "down" (list (make-posn 40 50))))
              (make-worm "down" (list (make-posn 40 50))))

;(define (set-worm-direction-up w) w) ;stub

(define (set-worm-direction-up w)
  (if (not (direction-down? (worm-direction w))) 
      (set-worm-direction w "up")
      w))        

;; Worm -> Worm
;; sets direction of worm to down

;; Worm Direction -> Worm
;; sets direction of worm to d
(check-expect (set-worm-direction (make-worm "left" (list (make-posn 40 50))) "up")
              (make-worm "up" (list (make-posn 40 50))))
(check-expect (set-worm-direction (make-worm "left" (list (make-posn 40 50))) "down")
              (make-worm "down" (list (make-posn 40 50))))
(check-expect (set-worm-direction (make-worm "down" (list (make-posn 40 50))) "left")
              (make-worm "left" (list (make-posn 40 50))))
(check-expect (set-worm-direction (make-worm "down" (list (make-posn 40 50))) "right")
              (make-worm "right" (list (make-posn 40 50))))

;(define (set-worm-direction w d) d) ;stub

(define (set-worm-direction w d)
  (make-worm d (worm-segments w)))    

;; Worm -> Worm
;; sets direction of worm to down. can only go down if not going up
(check-expect (set-worm-direction-down (make-worm "right" (list (make-posn 40 50))))
              (make-worm "down" (list (make-posn 40 50))))
(check-expect (set-worm-direction-down (make-worm "left" (list (make-posn 40 50))))
              (make-worm "down" (list (make-posn 40 50))))
(check-expect (set-worm-direction-down (make-worm "down" (list (make-posn 40 50))))
              (make-worm "down" (list (make-posn 40 50))))
(check-expect (set-worm-direction-down (make-worm "up" (list (make-posn 40 50))))
              (make-worm "up" (list (make-posn 40 50))))

;(define (set-worm-direction-down w) w) ;stub

(define (set-worm-direction-down w)
  (if (not (direction-up? (worm-direction w)))
      (set-worm-direction w "down")
      w))

;; Worm -> Worm
;; sets direction of worm to left. can only go left if not going right
(check-expect (set-worm-direction-left (make-worm "up" (list (make-posn 40 50))))
              (make-worm "left" (list (make-posn 40 50))))
(check-expect (set-worm-direction-left (make-worm "down" (list (make-posn 40 50))))
              (make-worm "left" (list (make-posn 40 50))))
(check-expect (set-worm-direction-left (make-worm "left" (list (make-posn 40 50))))
              (make-worm "left" (list (make-posn 40 50))))
(check-expect (set-worm-direction-left (make-worm "right" (list (make-posn 40 50))))
              (make-worm "right" (list (make-posn 40 50))))

;(define (set-worm-direction-left w) w) ;stub

(define (set-worm-direction-left w)
  (if (not (direction-right? (worm-direction w)))
      (set-worm-direction w "left")
      w))

;; Worm -> Worm
;; sets directoin of worm to right. can only go right if not going left
(check-expect (set-worm-direction-right (make-worm "up" (list (make-posn 40 50))))
              (make-worm "right" (list (make-posn 40 50))))
(check-expect (set-worm-direction-right (make-worm "down" (list (make-posn 40 50))))
              (make-worm "right" (list (make-posn 40 50))))
(check-expect (set-worm-direction-right (make-worm "right" (list (make-posn 40 50))))
              (make-worm "right" (list (make-posn 40 50))))
(check-expect (set-worm-direction-right (make-worm "left" (list (make-posn 40 50))))
              (make-worm "left" (list (make-posn 40 50))))

;(define (set-worm-direction-right w) w) ;stub

(define (set-worm-direction-right w)
  (if (not (direction-left? (worm-direction w)))
      (set-worm-direction w "right")
      w))

;; WormGame -> Boolean
;; produces true if the worm has run into the walls of the world or into itself
(check-expect (game-over? (make-worm-game (make-worm "down" (list (make-posn (+ 50 WORM-SPEED) (- 40 WORM-SPEED))
                                                                  (make-posn (+ (+ 50 WORM-SPEED) WORM-SPEED) (- 40 WORM-SPEED))
                                                                  (make-posn (+ (+ 50 WORM-SPEED) WORM-SPEED) 40)
                                                                  (make-posn (+ 50 WORM-SPEED) 40)
                                                                  (make-posn 50 40)))
                                          (make-posn 100 100)))
              #true)
(check-expect (game-over? (make-worm-game (make-worm "right" (list (make-posn AT-RIGHT-WALL 40)))
                          (make-posn 100 100)))
              #true)

;(define (game-over? wg) #false) ;stub

(define (game-over? wg)
  (or (ran-into-walls? (worm-game-worm wg))
      (ran-into-self? (move-worm (worm-game-worm wg)))))

;; Worm -> Boolean
;; produces true if worm ran into wall
(check-expect (ran-into-walls? (make-worm "right" (list (make-posn AT-RIGHT-WALL 40))))
              #true)
(check-expect (ran-into-walls? (make-worm "left" (list (make-posn AT-LEFT-WALL 40))))
              #true)
(check-expect (ran-into-walls? (make-worm "up" (list (make-posn 40 AT-TOP-WALL))))
              #true)
(check-expect (ran-into-walls? (make-worm "down" (list (make-posn 40 AT-BOTTOM-WALL))))
              #true)

;(define (ran-into-walls? w) #false) ;stub

(define (ran-into-walls? w)
  (or (ran-into-right-wall? w)
      (ran-into-left-wall? w)
      (ran-into-top-wall? w)
      (ran-into-bottom-wall? w)))

;; Worm -> Boolean
;; produces true if worm ran into right wall
(check-expect (ran-into-right-wall? (make-worm "right" (list (make-posn AT-RIGHT-WALL 40))))
              #true)
(check-expect (ran-into-right-wall? (make-worm "right" (list (make-posn (+ AT-RIGHT-WALL 1) 40))))
              #true)
(check-expect (ran-into-right-wall? (make-worm "right" (list (make-posn (- AT-RIGHT-WALL 1) 40))))
              #false)
(check-expect (ran-into-right-wall? (make-worm "right" (list (make-posn 50 40))))
              #false)
(check-expect (ran-into-right-wall? (make-worm "up" (list (make-posn AT-RIGHT-WALL 40))))
              #false)

;(define (ran-into-right-wall? w) #false) ;stub

(define (ran-into-right-wall? w)
  (and (direction-right? (worm-direction w))
       (at-or-passed-right-wall? (first (worm-segments w)))))

;; Direction -> Boolean
;; produces true if direction is right direction
(check-expect (direction-right? "right") #true)
(check-expect (direction-right? "left") #false)
(check-expect (direction-right? "up") #false)
(check-expect (direction-right? "down") #false)

;(define (direction-right? d) #false) ;stub

(define (direction-right? d)
  (string=? d "right"))

;; Posn -> Boolean
;; Produces true if posn is at or past right wall
(check-expect (at-or-passed-right-wall? (make-posn WIDTH 40))
              #true)
(check-expect (at-or-passed-right-wall? (make-posn (+ AT-RIGHT-WALL 1) 40))
              #true)
(check-expect (at-or-passed-right-wall? (make-posn (- AT-RIGHT-WALL 1) 40))
              #false)

;(define (at-or-passed-right-wall? p) #false) ;stub

;; Template from Posn

(define (at-or-passed-right-wall? p)
  (>= (posn-x p) AT-RIGHT-WALL))

;; Worm -> Boolean
;; produces true if worm ran into left wall
(check-expect (ran-into-left-wall? (make-worm "left" (list (make-posn AT-LEFT-WALL 40))))
              #true)
(check-expect (ran-into-left-wall? (make-worm "down" (list (make-posn AT-LEFT-WALL 40))))
              #false)
(check-expect (ran-into-left-wall? (make-worm "left" (list (make-posn (+ AT-LEFT-WALL 1) 40))))
              #false)

;(define (ran-into-left-wall? w) #false) ;stub

(define (ran-into-left-wall? w)
  (and (direction-left? (worm-direction w))
       (at-or-passed-left-wall? (first (worm-segments w)))))

;; Direction -> Boolean
;; produces true if direction is left direction
(check-expect (direction-left? "left") #true)
(check-expect (direction-left? "right") #false)
(check-expect (direction-left? "up") #false)
(check-expect (direction-left? "down") #false)

;(define (direction-left? d) #false) ;stub

(define (direction-left? d)
  (string=? d "left"))

;; Posn -> Boolean
;; Produces true if posn is at or past left wall
(check-expect (at-or-passed-left-wall? (make-posn AT-LEFT-WALL 40))
              #true)
(check-expect (at-or-passed-left-wall? (make-posn (- AT-LEFT-WALL 1) 40))
              #true)
(check-expect (at-or-passed-left-wall? (make-posn (+ AT-LEFT-WALL 1) 40))
              #false)

;(define (at-or-passed-left-wall? p) #false) ;stub

;; Template from Posn

(define (at-or-passed-left-wall? p)
  (<= (posn-x p) AT-LEFT-WALL))

;; Worm -> Boolean
;; produces true if worm ran into top wall
(check-expect (ran-into-top-wall? (make-worm "up" (list (make-posn 40 AT-TOP-WALL))))
              #true)
(check-expect (ran-into-top-wall? (make-worm "up" (list (make-posn 40 (+ AT-TOP-WALL 1)))))
              #false)

;(define (ran-into-top-wall? w) #false) ;stub

(define (ran-into-top-wall? w)
  (and (direction-up? (worm-direction w))
       (at-or-passed-top-wall? (first (worm-segments w)))))

;; Direction -> Boolean
;; produces true if direction is up direction
(check-expect (direction-up? "up") #true)
(check-expect (direction-up? "right") #false)
(check-expect (direction-up? "down") #false)
(check-expect (direction-up? "left") #false)

;(define (direction-up? d) #false) ;stub

(define (direction-up? d)
  (string=? d "up"))

;; Posn -> Boolean
;; Produces true if posn is at or past top wall
(check-expect (at-or-passed-top-wall? (make-posn 40 AT-TOP-WALL))
              #true)
(check-expect (at-or-passed-top-wall? (make-posn 40 (- AT-TOP-WALL 1)))
              #true)
(check-expect (at-or-passed-top-wall? (make-posn 40 (+ AT-TOP-WALL 1)))
              #false)

;(define (at-or-passed-top-wall? p) #false) ;stub

;; Template from Posn

(define (at-or-passed-top-wall? p)
  (<= (posn-y p) AT-TOP-WALL))

;; Worm -> Boolean
;; produces true if worm ran into bottom wall
(check-expect (ran-into-bottom-wall? (make-worm "down" (list (make-posn 40 AT-BOTTOM-WALL))))
              #true)
(check-expect (ran-into-bottom-wall? (make-worm "down" (list (make-posn 40 (- AT-BOTTOM-WALL 1)))))
              #false)
(check-expect (ran-into-bottom-wall? (make-worm "up" (list (make-posn 40 AT-BOTTOM-WALL))))
              #false)

;(define (ran-into-bottom-wall? w) #false) ;stub

(define (ran-into-bottom-wall? w)
  (and (direction-down? (worm-direction w))
       (at-or-passed-bottom-wall? (first (worm-segments w)))))

;; Direction -> Boolean
;; produces true if direction is down direction
(check-expect (direction-down? "down") #true)
(check-expect (direction-down? "up") #false)
(check-expect (direction-down? "right") #false)
(check-expect (direction-down? "left") #false)

;(define (direction-down? d) #false) ;stub

(define (direction-down? d)
  (string=? d "down"))

;; Posn -> Boolean
;; Produces true if posn is at or past bottom wall
(check-expect (at-or-passed-bottom-wall? (make-posn 40 AT-BOTTOM-WALL))
              #true)
(check-expect (at-or-passed-bottom-wall? (make-posn 40 (+ AT-BOTTOM-WALL 1)))
              #true)
(check-expect (at-or-passed-bottom-wall? (make-posn 40 (- AT-BOTTOM-WALL 1)))
              #false)

;(define (at-or-passed-bottom-wall? p) #false) ;stub

(define (at-or-passed-bottom-wall? p)
  (>= (posn-y p) AT-BOTTOM-WALL))

;; Worm -> Boolean
;; produces true if worm ran into self
(check-expect (ran-into-self? (make-worm "down" (list (make-posn 50 40)
                                                      (make-posn (+ 50 WORM-SPEED) (- 40 WORM-SPEED))
                                                      (make-posn (+ (+ 50 WORM-SPEED) WORM-SPEED) (- 40 WORM-SPEED))
                                                      (make-posn (+ (+ 50 WORM-SPEED) WORM-SPEED) 40)
                                                      (make-posn (+ 50 WORM-SPEED) 40)
                                                      (make-posn 50 40))))
              #true)
(check-expect (ran-into-self? (make-worm "right" (list (make-posn 50 50)
                                                       (make-posn (- 50 WORM-SPEED) (- 50 WORM-SPEED))
                                                       (make-posn (- 50 WORM-SPEED) (- (- 50 WORM-SPEED) WORM-SPEED))
                                                       (make-posn 50 (- (- 50 WORM-SPEED) WORM-SPEED))
                                                       (make-posn 50 (- 50 WORM-SPEED))
                                                       (make-posn 50 50))))
              #true)
(check-expect (ran-into-self? (make-worm "down" (list (make-posn (- 50 WORM-SPEED) (- (- 50 WORM-SPEED) WORM-SPEED))
                                                      (make-posn 50 (- (- 50 WORM-SPEED) WORM-SPEED))
                                                      (make-posn 50 (- 50 WORM-SPEED))
                                                      (make-posn 50 50))))
              #false)

;(define (ran-into-self? w) #false) ;stub

(define (ran-into-self? w)
  (first-in-rest? (worm-segments w)))

;; NELoP -> Boolean
;; produces true if first posn exists in rest of nelop
(check-expect (first-in-rest? (list (make-posn 50 50)))
              #false)
(check-expect (first-in-rest? (list (make-posn 50 50) (make-posn 50 50)))
              #true)
(check-expect (first-in-rest? (list (make-posn 50 50) (make-posn 30 90) (make-posn 50 50)))
              #true)

;(define (first-in-rest? nelop) #false) ;stub

(define (first-in-rest? nelop)
  (member (first nelop) (rest nelop)))

;; WormGame -> Image
;; Displays the scene and a message to explain whether the program stopped because the worm hit the wall or because it ran into itself
(check-expect (render-game-over (make-worm-game (make-worm "down" (list (make-posn (+ 50 WORM-SPEED) (- 40 WORM-SPEED))
                                                                        (make-posn (+ (+ 50 WORM-SPEED) WORM-SPEED) (- 40 WORM-SPEED))
                                                                        (make-posn (+ (+ 50 WORM-SPEED) WORM-SPEED) 40)
                                                                        (make-posn (+ 50 WORM-SPEED) 40)
                                                                        (make-posn 50 40)))
                                                (make-posn 100 100)))
              (overlay/align "left" "bottom" (text RAN-INTO-SELF-MSG 20 "red") (render-worm-game (make-worm-game (make-worm "down" (list (make-posn (+ 50 WORM-SPEED) (- 40 WORM-SPEED))
                                                                                                                                         (make-posn (+ (+ 50 WORM-SPEED) WORM-SPEED) (- 40 WORM-SPEED))
                                                                                                                                         (make-posn (+ (+ 50 WORM-SPEED) WORM-SPEED) 40)
                                                                                                                                         (make-posn (+ 50 WORM-SPEED) 40)
                                                                                                                                         (make-posn 50 40)))         
                                                                                                                 (make-posn 100 100)))))
(check-expect (render-game-over (make-worm-game (make-worm "right" (list (make-posn AT-RIGHT-WALL 40))) (make-posn 100 100)))
              (overlay/align "left" "bottom" (text HIT-WALL-MSG 20 "red ") (render-worm-game (make-worm-game (make-worm "right" (list (make-posn AT-RIGHT-WALL 40))) (make-posn 100 100)))))
                            
;(define (render-game-over wg) MTSCN) ;stub

(define (render-game-over wg)
  (if (ran-into-walls? (worm-game-worm wg))
      (render-game-over-with-msg wg HIT-WALL-MSG)
      (render-game-over-with-msg wg RAN-INTO-SELF-MSG)))

;; Worm String -> Image
;; renders the game over scene with msg
(check-expect (render-game-over-with-msg (make-worm-game (make-worm "down" (list (make-posn (+ 50 WORM-SPEED) (- 40 WORM-SPEED))
                                                                                 (make-posn (+ (+ 50 WORM-SPEED) WORM-SPEED) (- 40 WORM-SPEED))
                                                                                 (make-posn (+ (+ 50 WORM-SPEED) WORM-SPEED) 40)
                                                                                 (make-posn (+ 50 WORM-SPEED) 40)
                                                                                 (make-posn 50 40)))
                                                         (make-posn 100 100))
                                         RAN-INTO-SELF-MSG)
              (overlay/align "left" "bottom" (text RAN-INTO-SELF-MSG 20 "red") (render-worm-game (make-worm-game (make-worm "down" (list (make-posn (+ 50 WORM-SPEED) (- 40 WORM-SPEED))
                                                                                                                                         (make-posn (+ (+ 50 WORM-SPEED) WORM-SPEED) (- 40 WORM-SPEED))
                                                                                                                                         (make-posn (+ (+ 50 WORM-SPEED) WORM-SPEED) 40)
                                                                                                                                         (make-posn (+ 50 WORM-SPEED) 40)
                                                                                                                                         (make-posn 50 40)))         
                                                                                                                 (make-posn 100 100)))))
(check-expect (render-game-over-with-msg (make-worm-game (make-worm "right" (list (make-posn AT-RIGHT-WALL 40))) (make-posn 100 100))
                                         HIT-WALL-MSG)
              (overlay/align "left" "bottom" (text HIT-WALL-MSG 20 "red ") (render-worm-game (make-worm-game (make-worm "right" (list (make-posn AT-RIGHT-WALL 40))) (make-posn 100 100)))))

;(define (render-game-over-with-msg wg s) MTSCN) ;stub

(define (render-game-over-with-msg wg s)
  (overlay/align "left" "bottom" (text s 20 "red") (render-worm-game wg)))

; Posn -> Posn 
;; produces a random Posn with max MAX-FOOD-X and MAX-FOOD-Y different than given posn
(check-satisfied (food-create (make-posn 1 1)) not=-1-1?)
  
(define (food-create p)
  (food-check-create
   p (make-posn (+ (random (+ MAX-FOOD-X 1)) SEGMENT-R)
                (+ (random (+ MAX-FOOD-Y 1)) SEGMENT-R))))
 
; Posn Posn -> Posn 
; generative recursion 
; produces second posn if different than first. Else, produces a random posn different than first
(define (food-check-create p candidate)
  (if (equal? p candidate) (food-create p) candidate))
 
; Posn -> Boolean
; use for testing only 
(define (not=-1-1? p)
  (not (and (= (posn-x p) 1) (= (posn-y p) 1))))

