;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 219-logical) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Constants:

(define HEIGHT 30) ;# of worm segment widths high 
(define WIDTH 50) ;# of worm segments widths wide

(define SEGMENT-R 5)
(define SEGMENT-COLOR "red")
(define FOOD-COLOR "green")
(define SEGMENT (circle SEGMENT-R "solid" SEGMENT-COLOR))
(define SEGMENT-SIZE (image-width SEGMENT))
(define HALF-SEGMENT-SIZE (/ SEGMENT-SIZE 2))
(define MTSCN (empty-scene (* WIDTH SEGMENT-SIZE) (* HEIGHT SEGMENT-SIZE)))
(define FOOD (circle SEGMENT-R "solid" FOOD-COLOR))
(define DIAMETER (* SEGMENT-R 2))
(define WORM-POS-START-X (/ WIDTH 2))
(define WORM-POS-START-Y (/ HEIGHT 2))
(define WORM-DIRECTION-START "right")
(define TICK-RATE 0.5)
(define AT-TOP-WALL SEGMENT-R)
(define AT-BOTTOM-WALL (- HEIGHT SEGMENT-R))
(define AT-LEFT-WALL SEGMENT-R)
(define AT-RIGHT-WALL (- WIDTH SEGMENT-R))
(define HIT-WALL-MSG "worm hit wall")
(define RAN-INTO-SELF-MSG "worm ran into self")
(define DISPLAY-STATE #true)
(define MAX-FOOD-X (- WIDTH 1))
(define MAX-FOOD-Y (- HEIGHT 1)) 

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

(define-struct segment (x y))
;; A Segment is (make-segment Natural Natural)
;; interp. a logical position of a snake segment.
;; - x is how many (widths of) segments the left side of the segment is from the left of the scene. x is between 0 and WIDTH exclusive
;; - y is how many (widths of) segments the top side of the segment is from the top of the scene. y is between 0 and HEIGHT exclusive

(define S0 (make-segment 5 5))
(define S1 (make-segment 0 0))                     ;at left top edge
(define S2 (make-segment 0 (- HEIGHT 1)))          ;at left bottom edge
(define S3 (make-segment (- WIDTH 1) 0))           ;at right top edge
(define S4 (make-segment (- WIDTH 1) (- HEIGHT 1))) ;at right top edge

(define (fn-for-segment s)
  (... (segment-x s)
       (segment-y s)))

;; Template rules used:
;; compound: 2 field2s

;; A ListOfSegment is one of:
;; - empty
;; (cons Segment ListOfSegment)

(define LOS1 empty)
(define LOS2 (list (make-segment 2 6)))
(define LOS3 (list (make-segment 2 6) (make-segment 2 7)))

#;
(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (fn-for-segment (first los)) ;Segment
              (fn-for-los (rest los)))     ;ListOfSegment
         ]))

;; Template rules used:
;; - one-of: 2 cases
;; - atomic-distinct: empty
;; - compound: (cons Posn ListOfSegment)
;; - reference: (first los) is Segment
;; - self-reference: (rest los) is ListOfSegment

(define-struct worm [direction head tail])
;; a Worm is (make-worm Direction Segment ListOfSegment)
;; interp. a worm with its direction head and tail
;; A worm's head and tail are made of segments
;; - head: The worm will always have a head
;; - tail: a list of segments which may or may not be empty. The first segement in the list is the closest part of the tail to the head and the last is the farthest
;; movement:
;;   move head
;;   move tail - add new segemnt to begining of list
;;             - remove last segment from list
;; eating:
;;   1. move head to where food is
;;   2. grow tail - add new segemnt - where head used to be to begining of tail list
;;   3. remove old food and gen new food

(define W1 (make-worm "right" (make-segment 5 10) empty))                                         ;just a head
(define W2 (make-worm "right" (make-segment 5 10) (list (make-segment 6 10))))                    ;head with one segment tail. straight horizontal line
(define W3 (make-worm "right" (make-segment 5 10) (list (make-segment 6 10) (make-segment 7 10))))   ;head with two segment tail. straight horizontal line
(define W4 (make-worm "right" (make-segment 5 10) (list (make-segment 5 11) (make-segment 5 12))))   ;head with two segment tail. straight vertical line
(define W5 (make-worm "up" (make-segment 5 10) (list (make-segment 5 11) (make-segment 6 11) (make-segment 7 10)))) ;horizontal line with only head going up
;(define WORM-START (make-worm WORM-DIRECTION-START (make-segment WORM-POS-START-X WORM-POS-START-Y) empty ))
(define WORM-START (make-worm "right" (make-segment 10 10) (list (make-segment 9 10)
                                                                 (make-segment 8 10)
                                                                 (make-segment 7 10)
                                                                 (make-segment 6 10)
                                                                 (make-segment 5 10)) ))  
                              
#;
(define (fn-for-worm w)
  (... (fn-for-direction (worm-direction w)) ;Direction
       (fn-for-segment (worm-head w))        ;Segment
       (fn-for-los (worm-tail w))))          ;ListOfSegment

;; Template rules used:
;; - compound: 3 fields
;; - reference: (worm-direction w) is Direction
;; - reference: (worm-head w) is Segment
;; - reference: (worm-tail w) is ListOfSegment

(define-struct worm-game [worm food])
;; WormGame is (make-worm-game Worm Posn)
;; interp. The worm game state. the worm and the current position of the food

(define WG1 (make-worm-game (make-worm "right" (make-segment 5 10) empty) (make-posn 4 6)))
(define WG2 (make-worm-game (make-worm "right" (make-segment 5 10) empty) (make-posn 2 5)))

(define (fn-for-worm-game wg)
  (... (fn-for-worm (worm-game-worm wg))   ;Worm
       (fn-for-segment (worm-game-food wg)))) ;Segment

;; Template rules used:
;; - compound: 2 fields:
;; - reference: (worm-game-worm wg) is Worm
;; - reference: (worm-game-food wg) is Segment

;; Functions:

;; Number Boolean -> WormGame
;; starts the worm game. start with (worm-main TICK-RATE DISPLAY-STATE)
(define (worm-main rate display-state)
  (big-bang (make-worm-game WORM-START (food-create (make-segment 0 0)))        ; WormGame
    (on-tick   handle-tick rate)              ; WormGame -> WormGame
    (to-draw   render-worm-game)              ; WormGame -> Image
    (stop-when game-over? render-game-over)   ; WormGame -> ???
    (on-key    handle-key)                    ; WormGame KeyEvent -> WormGame
    (state display-state)))                   ; ???

;; WormGame -> WormGame
;; moves the worm and handles eating and food creation
(check-expect (handle-tick (make-worm-game (make-worm "right" (make-segment 5 5) (list (make-segment 4 5))) (make-segment 10 20)))      ;worm moves right. did not eat
              (make-worm-game (make-worm "right" (make-segment 6 5) (list (make-segment 5 5))) (make-segment 10 20)))
(check-random (handle-tick (make-worm-game (make-worm "right" (make-segment 5 5) (list (make-segment 4 5))) (make-segment 6 5)))        ;worm will eat food
              (make-worm-game (make-worm "right" (make-segment 6 5) (list (make-segment 5 5) (make-segment 4 5))) (make-segment (random MAX-FOOD-X)
                                                                                                                                (random MAX-FOOD-Y ))))
;(define (handle-tick wg) wg) ;stub

; template from WormGame

(define (handle-tick wg)
  (if (will-worm-eat? (worm-game-worm wg) (worm-game-food wg))
      (make-worm-game (worm-eat (worm-game-worm wg))
                      (food-create (worm-game-food wg)))
      (make-worm-game (move-worm (worm-game-worm wg))
                      (worm-game-food wg))))

;; Worm Segment -> Boolean
;; produces true if worm will eat food
(check-expect (will-worm-eat? (make-worm "right" (make-segment 5 5) (list (make-segment 4 5))) (make-segment 6 5)) ;will eat right
              true)
(check-expect (will-worm-eat? (make-worm "left" (make-segment 4 5) (list (make-segment 5 5))) (make-segment 3 5)) ;will eat left
              true)
(check-expect (will-worm-eat? (make-worm "up" (make-segment 4 5) (list (make-segment 5 5))) (make-segment 4 4)) ;will eat up
              true)
(check-expect (will-worm-eat? (make-worm "down" (make-segment 5 5) (list (make-segment 4 5))) (make-segment 5 6)) ;will eat down
              true)
(check-expect (will-worm-eat? (make-worm "down" (make-segment 5 5) (list (make-segment 4 5))) (make-segment 10 6)) ;far from eating
              false)

;(define (will-worm-eat? w s) false) ;stub

;template from Worm

(define (will-worm-eat? w s)
  (segment=? (move-head-in-direction (worm-head w) (worm-direction w)) 
             s))

;; Segment Segment -> Boolean
;; produces true if s1 = s2
(check-expect (segment=? (make-segment 5 6) (make-segment 5 6)) true)
(check-expect (segment=? (make-segment 5 7) (make-segment 5 6)) false)
(check-expect (segment=? (make-segment 4 7) (make-segment 5 7)) false)

;(define (segment=? s1 s2) false) ;stub

;template from segment

(define (segment=? s1 s2)
  (and (= (segment-x s1) (segment-x s2))
       (= (segment-y s1) (segment-y s2))))

;; Worm -> Worm
;; grows the worm after eating food.
;;   1. move head to where food is. this will be the next position in the direction of the worm
;;   2. grow tail - add new segemnt - where head used to be to begining of tail list
(check-expect (worm-eat (make-worm "right" (make-segment 5 5) (list (make-segment 4 5)))) ;eating to the right
              (make-worm "right" (make-segment 6 5) (list (make-segment 5 5) (make-segment 4 5))))
(check-expect (worm-eat (make-worm "left" (make-segment 4 5) (list (make-segment 5 5)))) ;eating to the left
              (make-worm "left" (make-segment 3 5) (list (make-segment 4 5) (make-segment 5 5))))
(check-expect (worm-eat (make-worm "up" (make-segment 4 5) (list (make-segment 5 5)))) ;eating up
              (make-worm "up" (make-segment 4 4) (list (make-segment 4 5) (make-segment 5 5))))
(check-expect (worm-eat (make-worm "down" (make-segment 4 5) (list (make-segment 5 5)))) ;eating down
              (make-worm "down" (make-segment 4 6) (list (make-segment 4 5) (make-segment 5 5))))

;(define (worm-eat w) w) ;stub

;template from Worm

(define (worm-eat w)
  (make-worm (worm-direction w)
             (move-head-in-direction (worm-head w) (worm-direction w))
             (cons (worm-head w) (worm-tail w))))         

;; Worm -> Worm
;; moves worm.
;   1. move head
;;  2. move tail
;;     - add new segemnt to where head used to be to begining of list
;;     - remove last segment from list
(check-expect (move-worm (make-worm "right" (make-segment 5 5) (list (make-segment 4 5)))) ;move right
              (make-worm "right" (make-segment (+ 5 1) 5) (list (make-segment 5 5))))
(check-expect (move-worm (make-worm "left" (make-segment 4 5) (list (make-segment 5 5)))) ;move left
              (make-worm "left" (make-segment (- 4 1) 5) (list (make-segment 4 5))))
(check-expect (move-worm (make-worm "up" (make-segment 5 5) (list (make-segment 4 5)))) ;move up
              (make-worm "up" (make-segment 5 (- 5 1)) (list (make-segment 5 5))))
(check-expect (move-worm (make-worm "down" (make-segment 5 5) (list (make-segment 4 5)))) ;move down
              (make-worm "down" (make-segment 5 (+ 5 1)) (list (make-segment 5 5))))

;(define (move-worm w) w) ;stub

;template from Worm

(define (move-worm w)
  (make-worm (worm-direction w)
             (move-head-in-direction (worm-head w) (worm-direction w))
             (move-tail (worm-tail w) (worm-head w))))         

;; Segment Direction -> Segment
;; moves segment in given direction
(check-expect (move-head-in-direction (make-segment 5 5) "right")
              (make-segment (+ 5 1) 5))
(check-expect (move-head-in-direction (make-segment 5 5) "left")
              (make-segment (- 5 1) 5))
(check-expect (move-head-in-direction (make-segment 5 5) "up")
              (make-segment 5 (- 5 1)))
(check-expect (move-head-in-direction (make-segment 5 5) "down")
              (make-segment 5 (+ 5 1)))

;(define (move-head-in-direction s d) s) ;stub

;template from direction

(define (move-head-in-direction s d)
  (cond [(string=? d "up") (move-segment-up s)]
        [(string=? d "down") (move-segment-down s)]
        [(string=? d "left") (move-segment-left s)]
        [(string=? d "right") (move-segment-right s)]))

;; Segment -> Segment
;; moves segment up
(check-expect (move-segment-up (make-segment 5 5))
              (make-segment 5 (- 5 1)))

;(define (move-segment-up s) s) ;stub

;template from Segment

(define (move-segment-up s)
  (make-segment (segment-x s)
                (- (segment-y s) 1)))

;; Segment -> Segment
;; moves segment down
(check-expect (move-segment-down (make-segment 5 5))
              (make-segment 5 (+ 5 1)))
              
;(define (move-segment-down s) s) ;stub

; template from Segment

(define (move-segment-down s)
  (make-segment (segment-x s)
                (+ (segment-y s) 1)))

;; Segment -> Segment
;; moves segment left
(check-expect (move-segment-left (make-segment 5 5))
              (make-segment (- 5 1) 5))

;(define (move-segment-left s) s) ;stub

;template from Segment

(define (move-segment-left s)
  (make-segment (- (segment-x s) 1)
                (segment-y s)))

;; Segment -> Segment
;; moves segment right
(check-expect (move-segment-right (make-segment 5 5))
              (make-segment (+ 5 1) 5))

;(define (move-segment-right s) s) ;stub

;template from Segment

(define (move-segment-right s)
  (make-segment (+ (segment-x s) 1)
                (segment-y s)))

;; ListOfSegment Segment -> ListOfSegment
;; moves tail by removing last segment and adding head to begining of the list
(check-expect (move-tail empty (make-segment 5 5))
              empty)
(check-expect (move-tail (list (make-segment 4 5)) (make-segment 5 5))
              (list (make-segment 5 5)))
(check-expect (move-tail (list (make-segment 4 5) (make-segment 3 5)) (make-segment 5 5))
              (list (make-segment 5 5) (make-segment 4 5)))

;(define (move-tail los s) los) ;stub

(define (move-tail los s)
  (if (empty? los)
      empty
      (cons s (reverse (rest (reverse los))))))

;; WormGame -> Image
;; renders the worm game on MTSCN
(check-expect (render-worm-game (make-worm-game (make-worm "right" (make-segment 6 5) (list (make-segment 5 5))) (make-segment 10 10))) ;worm far from food
              (place-image SEGMENT
                           (+ (* 5 SEGMENT-SIZE) HALF-SEGMENT-SIZE)
                           (+ (* 5 SEGMENT-SIZE) HALF-SEGMENT-SIZE)
                           (place-image SEGMENT
                                        (+ (* 6 SEGMENT-SIZE) HALF-SEGMENT-SIZE)
                                        (+ (* 5 SEGMENT-SIZE) HALF-SEGMENT-SIZE)
                                        (place-image FOOD
                                                     (+ (* 10 SEGMENT-SIZE) HALF-SEGMENT-SIZE)
                                                     (+ (* 10 SEGMENT-SIZE) HALF-SEGMENT-SIZE)
                                                     MTSCN))))
(check-expect (render-worm-game (make-worm-game (make-worm "right" (make-segment 6 5) (list (make-segment 5 5))) (make-segment 6 5))) ;worm eating food
              (place-image SEGMENT
                           (+ (* 5 SEGMENT-SIZE) HALF-SEGMENT-SIZE)
                           (+ (* 5 SEGMENT-SIZE) HALF-SEGMENT-SIZE)
                           (place-image SEGMENT
                                        (+ (* 6 SEGMENT-SIZE) HALF-SEGMENT-SIZE)
                                        (+ (* 5 SEGMENT-SIZE) HALF-SEGMENT-SIZE)
                                        (place-image FOOD
                                                     (+ (* 6 SEGMENT-SIZE) HALF-SEGMENT-SIZE)
                                                     (+ (* 5 SEGMENT-SIZE) HALF-SEGMENT-SIZE)
                                                     MTSCN))))

;(define (render-worm-game wg) MTSCN) ;stub

(define (render-worm-game wg)
  (render-worm (worm-game-worm wg) (render-food (worm-game-food wg) MTSCN)))

;; Segment Image -> Image
;; renders food on img
(check-expect (render-food (make-segment 10 10) MTSCN)
              (place-image FOOD
                           (+ (* 10 SEGMENT-SIZE) HALF-SEGMENT-SIZE)
                           (+ (* 10 SEGMENT-SIZE) HALF-SEGMENT-SIZE)
                           MTSCN))
(check-expect (render-food (make-segment 6 5) MTSCN)
              (place-image FOOD
                           (+ (* 6 SEGMENT-SIZE) HALF-SEGMENT-SIZE)
                           (+ (* 5 SEGMENT-SIZE) HALF-SEGMENT-SIZE)
                           MTSCN))

;(define (render-food p img) img) ;stub

;; Template from Segment

(define (render-food s img)
  (place-image FOOD (logical-x-to-physical-x (segment-x s)) (logical-y-to-physical-y (segment-y s)) img))

;; Worm Image -> Image
;; Renders worm on image
(check-expect (render-worm (make-worm "right" (make-segment 6 5) empty) MTSCN)
              (place-image SEGMENT
                           (+ (* 6 SEGMENT-SIZE) HALF-SEGMENT-SIZE)
                           (+ (* 5 SEGMENT-SIZE) HALF-SEGMENT-SIZE)
                           MTSCN))
(check-expect (render-worm (make-worm "right" (make-segment 6 5) (list (make-segment 5 5))) MTSCN)
              (place-image SEGMENT
                           (+ (* 5 SEGMENT-SIZE) HALF-SEGMENT-SIZE)
                           (+ (* 5 SEGMENT-SIZE) HALF-SEGMENT-SIZE)
                           (place-image SEGMENT
                                        (+ (* 6 SEGMENT-SIZE) HALF-SEGMENT-SIZE)
                                        (+ (* 5 SEGMENT-SIZE) HALF-SEGMENT-SIZE)
                                        MTSCN)))
;(define (render-worm w img) img) ;stub

; Template from Worm

(define (render-worm w img)
  (render-segments (worm-tail w) (render-segment (worm-head w) img)))

;; Segment Image -> Image
;; Renders segment on img
(check-expect (render-segment (make-segment 6 5) MTSCN)
              (place-image SEGMENT
                           (+ (* 6 SEGMENT-SIZE) HALF-SEGMENT-SIZE)
                           (+ (* 5 SEGMENT-SIZE) HALF-SEGMENT-SIZE)
                           MTSCN))

;(define (render-segment s img) img) ;stub

; Template from Segment

(define (render-segment s img)
  (place-image SEGMENT
               (logical-x-to-physical-x (segment-x s))
               (logical-y-to-physical-y (segment-y s))
               img))

;; ListOfSegments Image -> Image
;; Renders los on img
(check-expect (render-segments empty MTSCN) MTSCN)
(check-expect (render-segments (list (make-segment 5 5) (make-segment 6 5)) MTSCN)
              (place-image SEGMENT
                           (+ (* 5 SEGMENT-SIZE) HALF-SEGMENT-SIZE)
                           (+ (* 5 SEGMENT-SIZE) HALF-SEGMENT-SIZE)
                           (place-image SEGMENT
                                        (+ (* 6 SEGMENT-SIZE) HALF-SEGMENT-SIZE)
                                        (+ (* 5 SEGMENT-SIZE) HALF-SEGMENT-SIZE)
                                        MTSCN)))

;(define (render-segments s img) img) ;stub

;template from ListOfSegment

(define (render-segments los img)
  (cond [(empty? los) img]
        [else
         (render-segment (first los)
                         (render-segments (rest los) img))]))

;; Natural -> Natural
;; Produces physical x from logical one
(check-expect (logical-x-to-physical-x 6) (+ (* 6 SEGMENT-SIZE) HALF-SEGMENT-SIZE))
(check-expect (logical-x-to-physical-x 5) (+ (* 5 SEGMENT-SIZE) HALF-SEGMENT-SIZE))

;(define (logical-x-to-physical-x n) n) ;stub

(define (logical-x-to-physical-x n)
  (+ (* n SEGMENT-SIZE) HALF-SEGMENT-SIZE))

;; Natural -> Natural
;; Produces physical y from logical one
(check-expect (logical-y-to-physical-y 6) (+ (* 6 SEGMENT-SIZE) HALF-SEGMENT-SIZE))
(check-expect (logical-y-to-physical-y 5) (+ (* 5 SEGMENT-SIZE) HALF-SEGMENT-SIZE))

;(define (logical-y-to-physical-y n) n) ;stub

(define (logical-y-to-physical-y n)
  (+ (* n SEGMENT-SIZE) HALF-SEGMENT-SIZE))

;; WormGame KeyEvent -> WormGame
;; handle the arrow keys
(check-expect (handle-key (make-worm-game (make-worm "right" (make-segment 4 5) empty) (make-segment 10 10)) "up")
              (make-worm-game (make-worm "up" (make-segment 4 5) empty) (make-segment 10 10)))
(check-expect (handle-key (make-worm-game (make-worm "right" (make-segment 4 5) empty) (make-segment 10 10)) "down")
              (make-worm-game (make-worm "down" (make-segment 4 5) empty) (make-segment 10 10)))
(check-expect (handle-key (make-worm-game (make-worm "up" (make-segment 4 5) empty) (make-segment 10 10)) "right")
              (make-worm-game (make-worm "right" (make-segment 4 5) empty) (make-segment 10 10)))
(check-expect (handle-key (make-worm-game (make-worm "up" (make-segment 4 5) empty) (make-segment 10 10)) "left")
              (make-worm-game (make-worm "left" (make-segment 4 5) empty) (make-segment 10 10)))
(check-expect (handle-key (make-worm-game (make-worm "right" (make-segment 4 5) empty) (make-segment 10 10)) "x")
              (make-worm-game (make-worm "right" (make-segment 4 5) empty) (make-segment 10 10)))

;(define (handle-key wg ke) wg) ;stub

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
(check-expect (set-worm-direction-up (make-worm "right" (make-segment 4 5) empty))
              (make-worm "up" (make-segment 4 5) empty))
(check-expect (set-worm-direction-up (make-worm "left" (make-segment 4 5) empty))
              (make-worm "up" (make-segment 4 5) empty))
(check-expect (set-worm-direction-up (make-worm "up" (make-segment 4 5) empty))
              (make-worm "up" (make-segment 4 5) empty))
(check-expect (set-worm-direction-up (make-worm "down" (make-segment 4 5) empty))
              (make-worm "down" (make-segment 4 5) empty))

;(define (set-worm-direction-up w) w) ;stub

(define (set-worm-direction-up w)
  (if (not (direction-down? (worm-direction w))) 
      (set-worm-direction w "up")
      w))        

;; Worm -> Worm
;; sets direction of worm to down

;; Worm Direction -> Worm
;; sets direction of worm to d
(check-expect (set-worm-direction (make-worm "left" (make-segment 4 5) empty) "up")
              (make-worm "up" (make-segment 4 5) empty))
(check-expect (set-worm-direction (make-worm "left" (make-segment 4 5) empty) "down")
              (make-worm "down" (make-segment 4 5) empty))
(check-expect (set-worm-direction (make-worm "down" (make-segment 4 5) empty) "left")
              (make-worm "left" (make-segment 4 5) empty))
(check-expect (set-worm-direction (make-worm "down" (make-segment 4 5) empty) "right")
              (make-worm "right" (make-segment 4 5) empty))

;(define (set-worm-direction w d) d) ;stub

(define (set-worm-direction w d)
  (make-worm d
             (worm-head w)
             (worm-tail w)))    

;; Worm -> Worm
;; sets direction of worm to down. can only go down if not going up
(check-expect (set-worm-direction-down (make-worm "right" (make-segment 4 5) empty))
              (make-worm "down" (make-segment 4 5) empty))
(check-expect (set-worm-direction-down (make-worm "left" (make-segment 4 5) empty))
              (make-worm "down" (make-segment 4 5) empty))
(check-expect (set-worm-direction-down (make-worm "down" (make-segment 4 5) empty))
              (make-worm "down" (make-segment 4 5) empty))
(check-expect (set-worm-direction-down (make-worm "up" (make-segment 4 5) empty))
              (make-worm "up" (make-segment 4 5) empty))

;(define (set-worm-direction-down w) w) ;stub

(define (set-worm-direction-down w)
  (if (not (direction-up? (worm-direction w)))
      (set-worm-direction w "down")
      w))

;; Worm -> Worm
;; sets direction of worm to left. can only go left if not going right
(check-expect (set-worm-direction-left (make-worm "up" (make-segment 4 5) empty))
              (make-worm "left" (make-segment 4 5) empty))
(check-expect (set-worm-direction-left (make-worm "down" (make-segment 4 5) empty))
              (make-worm "left" (make-segment 4 5) empty))
(check-expect (set-worm-direction-left (make-worm "left" (make-segment 4 5) empty))
              (make-worm "left" (make-segment 4 5) empty))
(check-expect (set-worm-direction-left (make-worm "right" (make-segment 4 5) empty))
              (make-worm "right" (make-segment 4 5) empty))

;(define (set-worm-direction-left w) w) ;stub

(define (set-worm-direction-left w)
  (if (not (direction-right? (worm-direction w)))
      (set-worm-direction w "left")
      w))

;; Worm -> Worm
;; sets directoin of worm to right. can only go right if not going left
(check-expect (set-worm-direction-right (make-worm "up" (make-segment 4 5) empty))
              (make-worm "right" (make-segment 4 5) empty))
(check-expect (set-worm-direction-right (make-worm "down" (make-segment 4 5) empty))
              (make-worm "right" (make-segment 4 5) empty))
(check-expect (set-worm-direction-right (make-worm "right" (make-segment 4 5) empty))
              (make-worm "right" (make-segment 4 5) empty))
(check-expect (set-worm-direction-right (make-worm "left" (make-segment 4 5) empty))
              (make-worm "left" (make-segment 4 5) empty))

;(define (set-worm-direction-right w) w) ;stub

(define (set-worm-direction-right w)
  (if (not (direction-left? (worm-direction w)))
      (set-worm-direction w "right")
      w))

;; Direction -> Boolean
;; produces true if direction is down direction
(check-expect (direction-down? "down") #true)
(check-expect (direction-down? "up") #false)
(check-expect (direction-down? "right") #false)
(check-expect (direction-down? "left") #false)

;(define (direction-down? d) #false) ;stub

(define (direction-down? d)
  (string=? d "down"))

;; Direction -> Boolean
;; produces true if direction is up direction
(check-expect (direction-up? "up") #true)
(check-expect (direction-up? "right") #false)
(check-expect (direction-up? "down") #false)
(check-expect (direction-up? "left") #false)

;(define (direction-up? d) #false) ;stub

(define (direction-up? d)
  (string=? d "up"))

;; Direction -> Boolean
;; produces true if direction is right direction
(check-expect (direction-right? "right") #true)
(check-expect (direction-right? "left") #false)
(check-expect (direction-right? "up") #false)
(check-expect (direction-right? "down") #false)

;(define (direction-right? d) #false) ;stub

(define (direction-right? d)
  (string=? d "right"))

;; Direction -> Boolean
;; produces true if direction is left direction
(check-expect (direction-left? "left") #true)
(check-expect (direction-left? "right") #false)
(check-expect (direction-left? "up") #false)
(check-expect (direction-left? "down") #false)

;(define (direction-left? d) #false) ;stub

(define (direction-left? d)
  (string=? d "left"))

;; WormGame -> Boolean
;; produces true if the worm has run into the walls of the world or into itself
;; - run into the walls means that head has passed a wall. < 0 or = HEIGHT or = WIDTH. not sure I like this
;; - into itself means that head segment also exists in tail
(check-expect (game-over? (make-worm-game (make-worm "left" (make-segment -1 5) empty) (make-segment 5 5))) ;ran into left wall
              true)
(check-expect (game-over? (make-worm-game (make-worm "right" (make-segment WIDTH 5) empty) (make-segment 5 5))) ;ran into right wall
              true)
(check-expect (game-over? (make-worm-game (make-worm "up" (make-segment 5 -1) empty) (make-segment 5 5))) ;ran into top wall
              true)
(check-expect (game-over? (make-worm-game (make-worm "down" (make-segment 5 HEIGHT) empty) (make-segment 5 5))) ;ran into bottom wall
              true)
(check-expect (game-over? (make-worm-game (make-worm "down" (make-segment 5 5) empty) (make-segment 10 5))) ;did not hit wall
              false)
(check-expect (game-over? (make-worm-game (make-worm "up" ;hit self
                                                     (make-segment 5 5)
                                                     (list (make-segment 5 6) (make-segment 6 6) (make-segment 6 5) (make-segment 5 5) (make-segment 4 5)))
                                          (make-segment 10 5))) 
              true)

;(define (game-over? wg) #false) ;stub

; template from WormGame

(define (game-over? wg)
  (or (worm-ran-into-wall? (worm-game-worm wg))
      (worm-ran-into-self? (worm-game-worm wg)))) 
       
;; Worm -> Boolean
;; produces true if worm ran into wall meaning that head has passed a wall. x = -1 or WIDTH. y = -1 or HEIGHT
(check-expect (worm-ran-into-wall? (make-worm "left" (make-segment -1 5) empty)) ;ran into left wall
              true)
(check-expect (worm-ran-into-wall? (make-worm "right" (make-segment WIDTH 5) empty)) ;ran into right wall
              true)
(check-expect (worm-ran-into-wall? (make-worm "up" (make-segment 5 -1) empty)) ;ran into top wall
              true)
(check-expect (worm-ran-into-wall? (make-worm "down" (make-segment 5 HEIGHT) empty)) ;ran into bottom wall
              true)
(check-expect (worm-ran-into-wall? (make-worm "down" (make-segment 5 5) empty)) ;did not hit wall
              false) 
              
;(define (worm-ran-into-wall? w) false) ;stub

;template from Worm

(define (worm-ran-into-wall? w)
  (or (segment-passed-left-wall? (worm-head w))
      (segment-passed-right-wall? (worm-head w))
      (segment-passed-top-wall? (worm-head w))
      (segment-passed-bottom-wall? (worm-head w))))

;; Segment -> Boolean
;; produces true if segment passed left wall. x <= -1
(check-expect (segment-passed-left-wall? (make-segment -1 5)) ;passed left wall
              true)
(check-expect (segment-passed-left-wall? (make-segment 0 5)) ;border left wall
              false)

;(define (segment-passed-left-wall? s) false) ;stub

;template from Segment

(define (segment-passed-left-wall? s)
  (<= (segment-x s) -1))

;; Segment -> Boolean
;; produces true if segment passed right wall. x >= WIDTH
(check-expect (segment-passed-right-wall? (make-segment WIDTH 5)) ;passed right wall
              true)
(check-expect (segment-passed-right-wall? (make-segment (- WIDTH 1) 5)) ;border right wall
              false)

;(define (segment-passed-right-wall? s) false) ;stub

;template from Segment

(define (segment-passed-right-wall? s)
  (>= (segment-x s) WIDTH))

;; Segment -> Boolean
;; produces true if segment passed top wall. y <= -1
(check-expect (segment-passed-top-wall? (make-segment 5 -1)) ;passed top wall
              true)
(check-expect (segment-passed-top-wall? (make-segment 5 0)) ;border top wall
              false)

;(define (segment-passed-top-wall? s) false) ;stub

;template from Segment

(define (segment-passed-top-wall? s)
  (<= (segment-y s) -1))

;; Segment -> Boolean
;; produces true if segment passed bottom wall. y >= HEIGHT
(check-expect (segment-passed-bottom-wall? (make-segment 5 HEIGHT)) ;passed bottom wall
              true)
(check-expect (segment-passed-bottom-wall? (make-segment 5 (- HEIGHT 1))) ;border bottom wall
              false)

;(define (segment-passed-bottom-wall? s) false) ;stub

;template from Segment

(define (segment-passed-bottom-wall? s)
  (>= (segment-y s) HEIGHT))

;; Worm -> Boolean
;; produces true if worm ran into self meaning that head segment is also member of tail
(check-expect (worm-ran-into-self? (make-worm "up" ;hit self
                                              (make-segment 5 5)
                                              (list (make-segment 5 6) (make-segment 6 6) (make-segment 6 5) (make-segment 5 5) (make-segment 4 5))))                        
              true)
(check-expect (worm-ran-into-self? (make-worm "up" ;did not hit self
                                              (make-segment 5 5)
                                              (list (make-segment 5 6) (make-segment 5 7) (make-segment 5 8))))                        
              false)

;(define (worm-ran-into-self? w) false) ;stub

;template from Worm

(define (worm-ran-into-self? w)
  (member? (worm-head w) (worm-tail w)))         

;; WormGame -> Image
;; Displays the scene and a message to explain whether the program stopped because the worm hit the wall or because it ran into itself
(check-expect (render-game-over (make-worm-game (make-worm "left" (make-segment -1 5) empty) (make-segment 5 5))) ;ran into wall
              (overlay/align "left" "bottom" (text HIT-WALL-MSG 20 "red") (render-worm-game (make-worm-game (make-worm "left" (make-segment -1 5) empty) (make-segment 5 5)))))
(check-expect (render-game-over (make-worm-game (make-worm "up" ;hit self
                                                           (make-segment 5 5)
                                                           (list (make-segment 5 6) (make-segment 6 6) (make-segment 6 5) (make-segment 5 5) (make-segment 4 5)))
                                                (make-segment 10 5)))
              (overlay/align "left" "bottom" (text RAN-INTO-SELF-MSG 20 "red") (render-worm-game (make-worm-game (make-worm "up" ;hit self
                                                                                                                       (make-segment 5 5)
                                                                                                                       (list (make-segment 5 6) (make-segment 6 6) (make-segment 6 5) (make-segment 5 5) (make-segment 4 5)))
                                                                                                            (make-segment 10 5)))))

;(define (render-game-over wg) MTSCN) ;stub

;template from WormGame

(define (render-game-over wg)
  (if (worm-ran-into-wall? (worm-game-worm wg))
       (render-game-over-msg HIT-WALL-MSG (render-worm-game wg))
       (render-game-over-msg RAN-INTO-SELF-MSG (render-worm-game wg))))

;; Image String -> Image
;; renders msg on bottom left of img
(check-expect (render-game-over-msg HIT-WALL-MSG MTSCN)
              (overlay/align "left" "bottom" (text HIT-WALL-MSG 20 "red") MTSCN))

;(define (render-game-over-msg s img) img) ;stub

;template from ??

(define (render-game-over-msg s img)
  (overlay/align "left" "bottom" (text s 20 "red") img))

; Segment -> Segment 
;; produces a random Segment with max MAX-FOOD-X and MAX-FOOD-Y different than given posn
(check-satisfied (food-create (make-segment 1 1)) not=-1-1?)
  
(define (food-create p)
  (food-check-create
   p (make-segment (random MAX-FOOD-X) (random MAX-FOOD-Y))))
 
; Segment Segment -> Segment 
; generative recursion 
; produces second if different than first. Else, produces a random posn different than first
(define (food-check-create p candidate)
  (if (equal? p candidate) (food-create p) candidate))
 
; Segment -> Boolean
; use for testing only 
(define (not=-1-1? p)
  (not (and (= (segment-x p) 1) (= (segment-y p) 1))))


