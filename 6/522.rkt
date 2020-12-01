;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |522|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define MAX-PEOPLE-IN-GROUP 3)

(define PERSON-HEIGHT 10)
(define PERSON-WIDTH PERSON-HEIGHT)
(define SPACER (square (/ PERSON-HEIGHT 4) "solid" "white"))
(define RIVER-SIDE-WIDTH (+ (* PERSON-WIDTH 2) (* 3 (image-width SPACER))))
(define RIVER-SIDE-HEIGHT (+ (* MAX-PEOPLE-IN-GROUP PERSON-HEIGHT)
                             (* 6 (image-width SPACER)))) ;why are 4 SPACERS not enough??
(define RIVER-SIDE (rectangle RIVER-SIDE-WIDTH RIVER-SIDE-HEIGHT "outline" "black"))

(define PERSON-R (/ PERSON-HEIGHT 2))
(define MISSIONARY (circle PERSON-R "solid" "black"))
(define INVISIBLE-MISSIONARY (circle PERSON-R "solid" "white"))
(define CANNIBAL (circle PERSON-R "outline" "black"))

(define BOAT-WIDTH 15)
(define BOAT-BASE-HEIGHT (/ BOAT-WIDTH 3))
(define BOAT (above (triangle BOAT-BASE-HEIGHT "solid" "black")
                    (rectangle BOAT-WIDTH BOAT-BASE-HEIGHT "solid" "black")))

(define WAVE (text "~" BOAT-WIDTH "black"))
(define 2-WAVES (beside WAVE SPACER WAVE))
(define BOAT-WAVES-SPACER (square BOAT-WIDTH "solid" "white"))
(define RIVER-WIDTH (* (image-width BOAT) 4))
(define RIVER (overlay/align "middle" "bottom"
                             2-WAVES
                             (overlay/align "middle" "top" 2-WAVES (rectangle RIVER-WIDTH RIVER-SIDE-HEIGHT "outline" "black"))))
(define RIVER-WITH-BOAT-LEFT (overlay/align "left" "middle" (beside SPACER BOAT BOAT-WAVES-SPACER 2-WAVES) RIVER))
(define RIVER-WITH-BOAT-RIGHT (overlay/align "right" "middle" (beside 2-WAVES BOAT-WAVES-SPACER BOAT SPACER) RIVER))


(define-struct num-missionaries-cannibals [m c])
; NumMissionariesAndCannibals is a structure:
; (make-num-missionaries-cannibals Natural[0 MAX-PEOPLE-IN-GROUP] Natural[0 MAX-PEOPLE-IN-GROUP])
; interp: the number of missionaries and cannibals on a side of the river. Anywhere from 0 to MAX-PEOPLE-IN-GROUP each

(define MC1 (make-num-missionaries-cannibals 0 0))
(define MC2 (make-num-missionaries-cannibals 1 2))
(define MC3 (make-num-missionaries-cannibals 3 3))

(define (fn-for-num-missionaries-cannibals mc)
  (... (num-missionaries-cannibals-m mc)
       (num-missionaries-cannibals-c mc)))

; A BoatLocation is one of:
; - "left"
; - "right"
;interp: the location of the boat

(define (fn-for-boat-location l)
  (cond [(string=? l "left") ...]
        [(string=? l "right") ...]))

(define-struct puzzle-state [num-mc-left num-mc-right boat-location traversed])
; A PuzzleState is a structure:
; (make-puzzle-state NumMissionariesAndCannibals NumMissionariesAndCannibals BoatLocation [List-of PuzzleState])
; interp: a state for the missionary-and-cannibal puzzle
; - num-mc-left: the number of missionaries and cannibals on the left side of the river
; - num-mc-right: the number of missionaries and cannibals on the right side of the river
; - boat-location: the location of the boat
; - traversed: accumulator: the sequence of previous states traversed to get to this state in reverse order
                
(define PS-INIT (make-puzzle-state (make-num-missionaries-cannibals MAX-PEOPLE-IN-GROUP MAX-PEOPLE-IN-GROUP)
                                   (make-num-missionaries-cannibals 0 0)
                                   "left"
                                   empty))
(define PS-INTER1 (make-puzzle-state (make-num-missionaries-cannibals 2 2)
                                     (make-num-missionaries-cannibals 1 1)
                                     "right"
                                     (list PS-INIT)))
(define PS-INTER2 (make-puzzle-state (make-num-missionaries-cannibals 3 2)
                                     (make-num-missionaries-cannibals 1 1)
                                     "left"
                                     (list (make-puzzle-state (make-num-missionaries-cannibals 2 2)
                                                              (make-num-missionaries-cannibals 1 1)
                                                              "right"
                                                              empty)
                                           PS-INIT)))
(define PS-FINAL (make-puzzle-state (make-num-missionaries-cannibals 0 0)
                                    (make-num-missionaries-cannibals MAX-PEOPLE-IN-GROUP MAX-PEOPLE-IN-GROUP)
                                    "right"
                                    (list (make-puzzle-state (make-num-missionaries-cannibals 1 1)
                                                             (make-num-missionaries-cannibals 2 2)
                                                             "left"
                                                             empty)
                                          (make-puzzle-state (make-num-missionaries-cannibals 0 1)
                                                             (make-num-missionaries-cannibals 3 2)
                                                             "right"
                                                             empty)
                                          (make-puzzle-state (make-num-missionaries-cannibals 0 3)
                                                             (make-num-missionaries-cannibals 3 0)
                                                             "left"
                                                             empty)
                                          (make-puzzle-state (make-num-missionaries-cannibals 0 2)
                                                             (make-num-missionaries-cannibals 3 1)
                                                             "right"
                                                             empty)
                                          (make-puzzle-state (make-num-missionaries-cannibals 2 2)
                                                             (make-num-missionaries-cannibals 1 1)
                                                             "left"
                                                             empty)
                                          (make-puzzle-state (make-num-missionaries-cannibals 1 1)
                                                             (make-num-missionaries-cannibals 2 2)
                                                             "right"
                                                             empty)
                                          (make-puzzle-state (make-num-missionaries-cannibals 3 1)
                                                             (make-num-missionaries-cannibals 0 2)
                                                             "left"
                                                             empty)
                                          (make-puzzle-state (make-num-missionaries-cannibals 3 0)
                                                             (make-num-missionaries-cannibals 0 3)
                                                             "right"
                                                             empty)
                                          (make-puzzle-state (make-num-missionaries-cannibals 3 2)
                                                             (make-num-missionaries-cannibals 0 1)
                                                             "left"
                                                             empty)
                                          (make-puzzle-state (make-num-missionaries-cannibals 2 2)
                                                             (make-num-missionaries-cannibals 1 1)
                                                             "right"
                                                             empty)
                                          PS-INIT)))



(define (fn-for-puzzle-state s)
  (... (fn-for-num-missionaries-cannibals (puzzle-state-num-mc-left s))        ;NumMissionariesAndCannibals
       (fn-for-num-missionaries-cannibals (puzzle-state-num-mc-right s))       ;NumMissionariesAndCannibals
       (fn-for-boat-location (puzzle-state-boat-location s))                   ;BoatLocation
       (fn-for-lops (puzzle-state-traversed s))))                              ;[List-of PuzzleState]

; PuzzleState -> Boolean
; produces true if all people are on the right river bank in the given state
(check-expect (final? PS-INIT) #false)
(check-expect (final? PS-INTER1) #false)
(check-expect (final? PS-FINAL) #true)

;(define (final? s) #false) ;stub

(define (final? s)
  (all-people-present? (puzzle-state-num-mc-right s)))

; NumMissionariesAndCannibals -> Boolean
; produces true if we have MAX-PEOPLE-IN-GROUP for both groups
(check-expect (all-people-present? (make-num-missionaries-cannibals 0 0)) #false)
(check-expect (all-people-present? (make-num-missionaries-cannibals 2 2)) #false)
(check-expect (all-people-present? (make-num-missionaries-cannibals 3 3)) #true)

;(define (all-people-present? mc) #false) ;stub

(define (all-people-present? mc)
  (and (= (num-missionaries-cannibals-m mc) MAX-PEOPLE-IN-GROUP)
       (= (num-missionaries-cannibals-c mc) MAX-PEOPLE-IN-GROUP)))

; PuzzleState -> Image
; renders a missionary-and-cannibal puzzle state to an image
(check-expect (render-mc PS-INIT) (beside (render-river-side-and-people (make-num-missionaries-cannibals MAX-PEOPLE-IN-GROUP MAX-PEOPLE-IN-GROUP))
                                          (render-river-and-boat "left")
                                          (render-river-side-and-people (make-num-missionaries-cannibals 0 0))))
(check-expect (render-mc PS-FINAL) (beside (render-river-side-and-people (make-num-missionaries-cannibals  0 0))
                                           (render-river-and-boat "right")
                                           (render-river-side-and-people (make-num-missionaries-cannibals MAX-PEOPLE-IN-GROUP MAX-PEOPLE-IN-GROUP))))
(check-expect (render-mc PS-INTER1) (beside (render-river-side-and-people (make-num-missionaries-cannibals  2 2))
                                            (render-river-and-boat "right")
                                            (render-river-side-and-people (make-num-missionaries-cannibals 1 1))))
;(define (render-mc s) empty-image) ;stub

(define (render-mc s)
  (beside (render-river-side-and-people (puzzle-state-num-mc-left s))
          (render-river-and-boat (puzzle-state-boat-location s))
          (render-river-side-and-people (puzzle-state-num-mc-right s))))

; NumMissionariesAndCannibals -> Image
; renders the side and the people
(check-expect (render-river-side-and-people (make-num-missionaries-cannibals MAX-PEOPLE-IN-GROUP MAX-PEOPLE-IN-GROUP))
              (overlay (beside/align "top"
                                     (render-group MAX-PEOPLE-IN-GROUP MISSIONARY)
                                     SPACER
                                     (render-group MAX-PEOPLE-IN-GROUP CANNIBAL))
                       RIVER-SIDE))
(check-expect (render-river-side-and-people (make-num-missionaries-cannibals 0 0))
              (overlay (beside/align "top"
                                     INVISIBLE-MISSIONARY
                                     SPACER
                                     (render-group 0 CANNIBAL))
                       RIVER-SIDE))
(check-expect (render-river-side-and-people (make-num-missionaries-cannibals 2 1))
              (overlay (beside/align "top"
                                     (render-group 2 MISSIONARY)
                                     SPACER
                                     (render-group 1 CANNIBAL))
                       RIVER-SIDE))
(check-expect (render-river-side-and-people (make-num-missionaries-cannibals 0 MAX-PEOPLE-IN-GROUP))
              (overlay (beside/align "top"
                                     INVISIBLE-MISSIONARY
                                     SPACER
                                     (render-group MAX-PEOPLE-IN-GROUP CANNIBAL))
                       RIVER-SIDE))

;(define (render-river-side-and-people mc) empty-image) ;stub

(define (render-river-side-and-people mc)
  (local ((define missionaries-img (render-group (num-missionaries-cannibals-m mc) MISSIONARY)))
    (overlay (beside/align  "top"
                            (if (equal? missionaries-img empty-image)
                                INVISIBLE-MISSIONARY
                                missionaries-img)
                            SPACER
                            (render-group (num-missionaries-cannibals-c mc) CANNIBAL))
             RIVER-SIDE)))
 

; BoatLocation -> Image
; renders the river and the boat either at left or right  
(check-expect (render-river-and-boat "left")
              RIVER-WITH-BOAT-LEFT)
(check-expect (render-river-and-boat "right")
              RIVER-WITH-BOAT-RIGHT)

;(define (render-river-and-boat l) empty-image) ;stub

(define (render-river-and-boat l)
  (cond [(string=? l "left") RIVER-WITH-BOAT-LEFT]
        [(string=? l "right") RIVER-WITH-BOAT-RIGHT]))

; Natural Image -> Image
; renders given amount of people using the given img
(check-expect (render-group 0 MISSIONARY) empty-image)
(check-expect (render-group 2 CANNIBAL) (above CANNIBAL
                                               SPACER
                                               CANNIBAL
                                               empty-image))
(check-expect (render-group 3 MISSIONARY) (above MISSIONARY
                                                 SPACER
                                                 MISSIONARY
                                                 SPACER
                                                 MISSIONARY
                                                 empty-image))

;(define (render-group n img) empty-image) ;stub

#;
(define (render-group n img)
  (cond [(zero? n) empty-image]
        [else
         (... n
              (render-group (sub1 n)))]))

(define (render-group n img)
  (cond [(zero? n) empty-image]
        [else
         (local ((define res (render-group (sub1 n) img)))
           (if (equal? res empty-image)
               (above img
                      res)
               (above img
                      SPACER
                      res)))]))
