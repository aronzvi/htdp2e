;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |229|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; Data definitions:

; An FSM is one of:
;   – '()
;   – (cons Transition.v2 FSM)

; interpretation An FSM represents the transitions that a
; finite state machine can take from one state to another 
; in reaction to keystrokes

(define (fn-for-fsm fsm)
  (cond
    [(empty? fsm) (...)]
    [else
     (...
      (fn-for-ktransition (first fsm)) ;Transition.v2
      (fn-for-fsm (rest fsm)))]))     ;FSM
 
(define-struct ktransition [current key next])
; A Transition.v2 is a structure:
;   (make-ktransition FSM-State KeyEvent FSM-State)

(define (fn-for-ktransition t)
  (... (ktransition-current t)
       (ktransition-key t)
       (ktransition-next t)))

(define fsm-kpattern (list (make-ktransition "white" "a" "yellow")
                           (make-ktransition "yellow" "b" "yellow")
                           (make-ktransition "yellow" "c" "yellow")
                           (make-ktransition "yellow" "d" "green")))

; FSM-State is a Color.

(define-struct fs [fsm current])
; A SimulationState.v2 is a structure: 
;   (make-fs FSM FSM-State)

(define (fn-for-fs fs)
  (... (fn-for-fsm (fs-fsm fs)) ;FSM
       (fs-current fs)))        ;FSM-State

; Functions:

; FSM FSM-State -> SimulationState.v2 
; match the keys pressed with the given FSM 
(define (simulate.v2 an-fsm s0)
  (big-bang (make-fs an-fsm s0)
    [to-draw state-as-colored-square]
    [on-key find-next-state]))

; SimulationState.v2 -> Image 
; renders current world state as a colored square 
(check-expect (state-as-colored-square
               (make-fs fsm-kpattern "red"))
              (square 100 "solid" "red"))
 
(define (state-as-colored-square an-fsm)
  (square 100 "solid" (fs-current an-fsm)))

; SimulationState.v2 KeyEvent -> SimulationState.v2
; finds the next state from ke and cs
(check-expect
 (find-next-state (make-fs fsm-kpattern "white") "a")
 (make-fs fsm-kpattern "yellow"))
(check-expect
 (find-next-state (make-fs fsm-kpattern "yellow") "b")
 (make-fs fsm-kpattern "yellow"))
(check-expect
 (find-next-state (make-fs fsm-kpattern "yellow") "c")
 (make-fs fsm-kpattern "yellow"))
(check-expect
 (find-next-state (make-fs fsm-kpattern "yellow") "d")
 (make-fs fsm-kpattern "green"))

;(define (find-next-state an-fsm current) an-fsm) ;stub

(define (find-next-state an-fsm ke)
  (make-fs
   (fs-fsm an-fsm)
   (find (fs-fsm an-fsm) (fs-current an-fsm) ke)))

; FSM FSM-State KeyEvent -> FSM-State
; finds the state representing current in transitions
; and retrieves the next field 
(check-expect (find fsm-kpattern "white" "a") "yellow")
(check-expect (find fsm-kpattern "yellow" "b") "yellow")
(check-expect (find fsm-kpattern "yellow" "c") "yellow")
(check-expect (find fsm-kpattern "yellow" "d") "green")

;(define (find transitions current ke) current) ;stub

(define (find fsm current ke)
  (cond
    [(empty? fsm) (error (string-append "not found: " current))]
    [else
     (if (should-ktransition? (first fsm) current ke)
         (ktransition-next (first fsm))
         (find (rest fsm) current ke))]))

; Transition FSM-State -> Boolean
; produces true if current transition state is equal to state and ke is equal to ke
(check-expect (should-ktransition? (make-ktransition "white" "a" "yellow") "white" "a") #true)
(check-expect (should-ktransition? (make-ktransition "yellow" "b" "yellow") "yellow" "b") #true)
(check-expect (should-ktransition? (make-ktransition "yellow" "c" "yellow") "yellow" "c") #true)
(check-expect (should-ktransition? (make-ktransition "yellow" "d" "green") "yellow" "d") #true)

;(define (should-ktransition? t s ke) #false) ;stub

(define (should-ktransition? t s ke)
  (and (state=? s (ktransition-current t))
       (string=? ke (ktransition-key t))))

; FSM-State FSM-State -> Boolean
; produces true if state1 is equal to state2
(check-expect (state=? "red" "red") #true)
(check-expect (state=? "red" "green") #false)

;(define (state=? state1 state2) #false)

(define (state=? state1 state2)
  (string=? state1 state2))