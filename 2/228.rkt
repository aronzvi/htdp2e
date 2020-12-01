;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |228|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; Data definitions:

; An FSM is one of:
;   – '()
;   – (cons Transition FSM)

; interpretation An FSM represents the transitions that a
; finite state machine can take from one state to another 
; in reaction to keystrokes

(define (fn-for-fsm fsm)
  (cond
    [(empty? fsm) (...)]
    [else
     (...
      (fn-for-transtion (first fsm)) ;Transition
      (fn-for-fsm (rest fsm)))])) ;FSM
 
(define-struct transition [current next])
; A Transition is a structure:
;   (make-transition FSM-State FSM-State)

(define (fn-for-transition t)
  (... (transition-current t)
       (transition-next t)))

(define fsm-traffic
  (list (make-transition "red" "green")
        (make-transition "green" "yellow")
        (make-transition "yellow" "red")))

(define fsm-bw
  (list (make-transition "black" "white")
        (make-transition "white" "black")))

; FSM-State is a Color.

(define-struct fs [fsm current])
; A SimulationState.v2 is a structure: 
;   (make-fs FSM FSM-State)

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
                (make-fs fsm-traffic "red"))
              (square 100 "solid" "red"))
 
(define (state-as-colored-square an-fsm)
  (square 100 "solid" (fs-current an-fsm)))

; SimulationState.v2 KeyEvent -> SimulationState.v2
; finds the next state from ke and cs
(check-expect
  (find-next-state (make-fs fsm-traffic "red") "n")
  (make-fs fsm-traffic "green"))
(check-expect
  (find-next-state (make-fs fsm-traffic "red") "a")
  (make-fs fsm-traffic "green"))
(check-expect
  (find-next-state (make-fs fsm-traffic "green") "q")
  (make-fs fsm-traffic "yellow"))

;(define (find-next-state an-fsm current) an-fsm) ;stub

(define (find-next-state an-fsm ke)
  (make-fs
    (fs-fsm an-fsm)
    (find (fs-fsm an-fsm) (fs-current an-fsm))))

; FSM FSM-State -> FSM-State
; finds the state representing current in transitions
; and retrieves the next field 
(check-expect (find fsm-traffic "red") "green")
(check-expect (find fsm-traffic "green") "yellow")
(check-expect (find fsm-traffic "yellow") "red")
(check-error (find fsm-traffic "black")
             "not found: black")

;(define (find transitions current) current) ;stub

(define (find fsm current)
  (cond
    [(empty? fsm) (error (string-append "not found: " current))]
    [else
     (if (transition-current-state=? (first fsm) current)
         (transition-next (first fsm))
         (find (rest fsm) current))]))

; Transition FSM-State -> Boolean
; produces true if current transition state is equal to state
(check-expect (transition-current-state=? (make-transition "red" "green") "red") #true)
(check-expect (transition-current-state=? (make-transition "red" "green") "yellow") #false)

;(define (transition-current-state=? t s) #false) ;stub

(define (transition-current-state=? t s)
  (state=? s
           (transition-current t)))

; FSM-State FSM-State -> Boolean
; produces true if state1 is equal to state2
(check-expect (state=? "red" "red") #true)
(check-expect (state=? "red" "green") #false)

;(define (state=? state1 state2) #false)

(define (state=? state1 state2)
  (string=? state1 state2))

