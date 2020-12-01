;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |523|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define MAX-PEOPLE-IN-GROUP 3)

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
                                     (list PS-INIT PS-INTER1)))
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

; a PuzzleStateOrFalse is one of:
; - PuzzleState
; - false

; [List-of PuzzleState] -> [List-of PuzzleState]
; generates the list of all states that a boat ride can reach from the states in lops
; Ignores the accumulator, does not generate states where the cannibals can eat the missionaries

(check-expect (create-next-states empty) empty)
(check-expect (create-next-states (list PS-INIT))
              (list (make-puzzle-state (make-num-missionaries-cannibals 3 2)
                                       (make-num-missionaries-cannibals 0 1)
                                       "right"
                                       empty)
                    (make-puzzle-state (make-num-missionaries-cannibals 3 1)
                                       (make-num-missionaries-cannibals 0 2)
                                       "right"
                                       empty)
                    (make-puzzle-state (make-num-missionaries-cannibals 2 2)
                                       (make-num-missionaries-cannibals 1 1)
                                       "right"
                                       empty)))
(check-expect (create-next-states (list (make-puzzle-state (make-num-missionaries-cannibals 2 2) ; will have a state that we have already seen before
                                                           (make-num-missionaries-cannibals 1 1)
                                                           "right"
                                                           empty)))
              (list (make-puzzle-state (make-num-missionaries-cannibals 3 3)
                                       (make-num-missionaries-cannibals 0 0)
                                       "left"
                                       empty)
                    (make-puzzle-state (make-num-missionaries-cannibals 3 2)
                                       (make-num-missionaries-cannibals 0 1)
                                       "left"
                                       empty)))
(check-expect (create-next-states (list (make-puzzle-state (make-num-missionaries-cannibals 3 2)
                                                           (make-num-missionaries-cannibals 0 1)
                                                           "right"
                                                           empty)
                                        (make-puzzle-state (make-num-missionaries-cannibals 3 1)
                                                           (make-num-missionaries-cannibals 0 2)
                                                           "right"
                                                           empty)
                                        (make-puzzle-state (make-num-missionaries-cannibals 2 2)
                                                           (make-num-missionaries-cannibals 1 1)
                                                           "right"
                                                           empty)))
              (list (make-puzzle-state (make-num-missionaries-cannibals 3 3)
                                       (make-num-missionaries-cannibals 0 0)
                                       "left"
                                       empty)
                    (make-puzzle-state (make-num-missionaries-cannibals 3 3)
                                       (make-num-missionaries-cannibals 0 0)
                                       "left"
                                       empty)
                    (make-puzzle-state (make-num-missionaries-cannibals 3 3)
                                       (make-num-missionaries-cannibals 0 0)
                                       "left"
                                       empty)
                    (make-puzzle-state (make-num-missionaries-cannibals 3 2)
                                       (make-num-missionaries-cannibals 0 1)
                                       "left"
                                       empty)))

;(define (create-next-states lops) empty) ;stub

#;
(define (create-next-states lops)
  (cond [(empty? lops) ...]
        [else
         (... (fn-for-ps (first lops))             ;PuzzleState
              (create-next-states (rest lops)))])) ;[List-of PuzzleState]

(define (create-next-states lops)
  (cond [(empty? lops) empty]
        [else
         (local (; PuzzleState -> Boolean
                 ; produces true if cannibals cannot eat missionaries on both sides of the river
                 (define (c-cannot-eat-m? s) #false)
                 
                 (define state-all-next-states (create-all-next-states/state (first lops)))
                 (define state-next-states-c-cannot-eat-m (filter c-cannot-eat-m? state-all-next-states)))
           (append state-next-states-c-cannot-eat-m            
                   (create-next-states (rest lops))))]))

; PuzzleState -> [List-of PuzzleState]
; creates all possible next states for given state
(check-expect (create-all-next-states/state PS-INIT)
              (list (make-puzzle-state (make-num-missionaries-cannibals 3 2)
                                       (make-num-missionaries-cannibals 0 1)
                                       "right"
                                       empty)
                    (make-puzzle-state (make-num-missionaries-cannibals 3 1)
                                       (make-num-missionaries-cannibals 0 2)
                                       "right"
                                       empty)
                    (make-puzzle-state (make-num-missionaries-cannibals 2 2)
                                       (make-num-missionaries-cannibals 1 1)
                                       "right"
                                       empty)
                    (make-puzzle-state (make-num-missionaries-cannibals 1 3)
                                       (make-num-missionaries-cannibals 2 0)
                                       "right"
                                       empty)
                    (make-puzzle-state (make-num-missionaries-cannibals 2 3)
                                       (make-num-missionaries-cannibals 1 0)
                                       "right"
                                       empty)))
(check-expect (create-all-next-states/state (make-puzzle-state (make-num-missionaries-cannibals 2 2) 
                                                               (make-num-missionaries-cannibals 1 1)
                                                               "right"
                                                               empty))
              (list (make-puzzle-state (make-num-missionaries-cannibals 2 3)
                                       (make-num-missionaries-cannibals 1 0)
                                       "left"
                                       empty)
                    (make-puzzle-state (make-num-missionaries-cannibals 3 3)
                                       (make-num-missionaries-cannibals 0 0)
                                       "left"
                                       empty)
                    (make-puzzle-state (make-num-missionaries-cannibals 3 2)
                                       (make-num-missionaries-cannibals 0 1)
                                       "left"
                                       empty)))

;(define (create-all-next-states/state s) empty) ;stub

; Template from PuzzleState

(define (create-all-next-states/state s)    
  (if (boat-at-left? (puzzle-state-boat-location s))
      (create-all-possible-next-states-from-to (puzzle-state-num-mc-left s) (puzzle-state-num-mc-right s) (puzzle-state-boat-location s))       
      (create-all-possible-next-states-from-to (puzzle-state-num-mc-right s) (puzzle-state-num-mc-left s) (puzzle-state-boat-location s))))

; NumMissionariesAndCannibals NumMissionariesAndCannibals BoatLocation -> [List-of PuzzleState]
; produces all possible states moving from the l side with nmc-from and nmc-to people
(check-expect (create-all-possible-next-states-from-to (make-num-missionaries-cannibals MAX-PEOPLE-IN-GROUP MAX-PEOPLE-IN-GROUP)
                                                       (make-num-missionaries-cannibals 0 0)
                                                       "left")
              (list (make-puzzle-state (make-num-missionaries-cannibals 3 2)
                                       (make-num-missionaries-cannibals 0 1)
                                       "right"
                                       empty)
                    (make-puzzle-state (make-num-missionaries-cannibals 3 1)
                                       (make-num-missionaries-cannibals 0 2)
                                       "right"
                                       empty)
                    (make-puzzle-state (make-num-missionaries-cannibals 2 2)
                                       (make-num-missionaries-cannibals 1 1)
                                       "right"
                                       empty)
                    (make-puzzle-state (make-num-missionaries-cannibals 1 3)
                                       (make-num-missionaries-cannibals 2 0)
                                       "right"
                                       empty)
                    (make-puzzle-state (make-num-missionaries-cannibals 2 3)
                                       (make-num-missionaries-cannibals 1 0)
                                       "right"
                                       empty)))
(check-expect (create-all-possible-next-states-from-to (make-num-missionaries-cannibals 2 2) 
                                                       (make-num-missionaries-cannibals 1 1)
                                                       "right")
              (list (make-puzzle-state (make-num-missionaries-cannibals 2 3)
                                       (make-num-missionaries-cannibals 1 0)
                                       "left"
                                       empty)
                    (make-puzzle-state (make-num-missionaries-cannibals 3 3)
                                       (make-num-missionaries-cannibals 0 0)
                                       "left"
                                       empty)
                    (make-puzzle-state (make-num-missionaries-cannibals 3 2)
                                       (make-num-missionaries-cannibals 0 1)
                                       "left"
                                       empty)))

;(define (create-all-next-states-from-to nmc-from nmc-to l) empty) ;stub

(define (create-all-possible-next-states-from-to nmc-from nmc-to l)
  (filter puzzle-state? (list (move-cannibals 1 nmc-from nmc-to l)
                              (move-cannibals 2 nmc-from nmc-to l)
                              (move-one-each nmc-from nmc-to l)
                              (move-missionaries 2 nmc-from nmc-to l)
                              (move-missionaries 1 nmc-from nmc-to l))))

; Natural NumMissionariesAndCannibals NumMissionariesAndCannibals BoatLocation -> PuzzleStateOrFalse
; tries to move n cannibals from nmc-from people going to the l side to nmc-to people. false if not enough cannibals to move
; !!!
(define (move-cannibals n nmc-from nmc-to l) (make-puzzle-state nmc-from  ;stub
                                                                 nmc-to
                                                                 l
                                                                 empty))

; Natural NumMissionariesAndCannibals NumMissionariesAndCannibals BoatLocation -> PuzzleStateOrFalse
; tries to move n missionaries from nmc-from people going to the l side to nmc-to people. false if not enough missionaries to move
; !!!
(define (move-missionaries n nmc-from nmc-to l) (make-puzzle-state nmc-from  ;stub
                                                                 nmc-to
                                                                 l
                                                                 empty))

; NumMissionariesAndCannibals NumMissionariesAndCannibals BoatLocation -> PuzzleStateOrFalse
; tries to move 1 cannibal and 1 missionary from nmc-from people going to the l side to nmc-to people. false if not enough missionaries and cannibals to move
; !!!
(define (move-one-each nmc-from nmc-to l) (make-puzzle-state nmc-from  ;stub
                                                                 nmc-to
                                                                 l
                                                                 empty))

; BoatLocation -> Boolean
; produces true if boat is at left side of the river
; !!!
(define (boat-at-left? l) #false) ;stub