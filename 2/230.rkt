;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |230|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; Data definitions:

; A LOT is one of: 
; – '() 
; – (cons Transition.v3 LOT)

(define (fn-for-lot lot)
  (cond
    [(empty? lot) (...)]
    [else
     (... (fn-for-transition (first lot))  ;Transition.v3
          (fn-for-lot (rest lot)))]))      ;LOT

(define-struct transition [current key next])
; A Transition.v3 is a structure: 
;   (make-transition FSM-State KeyEvent FSM-State)

(define (fn-for-transition t)
  (... (transition-current t)
       (transition-key t)
       (transition-next t)))

(define-struct fsm [initial transitions final])
; An FSM.v2 is a structure: 
;   (make-fsm FSM-State LOT FSM-State)

(define kpattern-transitions (list (make-transition "white" "a" "yellow")
                                   (make-transition "yellow" "b" "yellow")
                                   (make-transition "yellow" "c" "yellow")
                                   (make-transition "yellow" "d" "green")))
(define fsm-kpattern (make-fsm "white"
                               kpattern-transitions
                               "green"))

(define (fn-for-fsm f)
  (... (fsm-initial f)
       (fn-for-lot (fsm-transitions f))
       (fsm-final f)))

; FSM-State is a Color.

; Functions:

; FSM.v2 -> FSM.v2 
; match the keys pressed with the given FSM 
(define (fsm-simulate an-fsm)
  (big-bang an-fsm
    [to-draw state-as-colored-square]
    [on-key find-next-state]
    [stop-when reached-final? state-as-colored-square]))

; FSM.v2 -> Image 
; renders current world state as a colored square
(check-expect (state-as-colored-square
               fsm-kpattern)
              (square 100 "solid" "white"))

;(define (state-as-colored-square an-fsm) (square 0 "solid" "white")) ;stub

(define (state-as-colored-square f)
  (square 100 "solid" (fsm-initial f)))

; FSM.v2 KeyEvent -> FSM.v2
; finds the next state from ke and cs
(check-expect (find-next-state (make-fsm "white"
                                         kpattern-transitions
                                         "green") "a")
              (make-fsm "yellow"
                        kpattern-transitions
                        "green"))
(check-expect (find-next-state (make-fsm "yellow"
                                         kpattern-transitions
                                         "green") "b")
              (make-fsm "yellow"
                        kpattern-transitions
                        "green"))
(check-expect (find-next-state (make-fsm "yellow"
                                         kpattern-transitions
                                         "green") "c")
              (make-fsm "yellow"
                        kpattern-transitions
                        "green"))
(check-expect (find-next-state (make-fsm "yellow"
                                         kpattern-transitions
                                         "green") "d")
              (make-fsm "green"
                        kpattern-transitions
                        "green"))

;(define (find-next-state an-fsm ke) an-fsm) ;stub

(define (find-next-state f ke)
  (make-fsm (find (fsm-transitions f) (fsm-initial f)  ke)
       (fsm-transitions f)
       (fsm-final f)))

; LOT FSM-State KeyEvent -> FSM-State
; finds the state representing current in transitions
; and retrieves the next field
(check-expect (find kpattern-transitions "white" "a") "yellow")
(check-expect (find kpattern-transitions "yellow" "b") "yellow")
(check-expect (find kpattern-transitions "yellow" "c") "yellow")
(check-expect (find kpattern-transitions "yellow" "d") "green")

;(define (find transitions current ke) current) ;stub

(define (find transitions current ke)
  (cond
    [(empty? transitions) (error (string-append "not found: " current))]
    [else
     (if (should-transition? (first transitions) current ke)
         (transition-next (first transitions))
         (find (rest transitions) current ke))]))

; Transition FSM-State -> Boolean
; produces true if current transition state is equal to state and ke is equal to ke
(check-expect (should-transition? (make-transition "white" "a" "yellow") "white" "a") #true)
(check-expect (should-transition? (make-transition "yellow" "b" "yellow") "yellow" "b") #true)
(check-expect (should-transition? (make-transition "yellow" "c" "yellow") "yellow" "c") #true)
(check-expect (should-transition? (make-transition "yellow" "d" "green") "yellow" "d") #true)

;(define (should-transition? t s ke) #false) ;stub

(define (should-transition? t s ke)
  (and (state=? s (transition-current t))
       (string=? ke (transition-key t))))

; FSM-State FSM-State -> Boolean
; produces true if state1 is equal to state2
(check-expect (state=? "red" "red") #true)
(check-expect (state=? "red" "green") #false)

;(define (state=? state1 state2) #false)

(define (state=? state1 state2)
  (string=? state1 state2))

; FSM.v2 -> Boolean
; produces true if reached final state
(check-expect (reached-final? (make-fsm "white"
                                         kpattern-transitions
                                         "green"))
              #false)
(check-expect (reached-final? (make-fsm "yellow"
                                         kpattern-transitions
                                         "green"))
              #false)
(check-expect (reached-final? (make-fsm "green"
                                         kpattern-transitions
                                         "green"))
              #true)

;(define (reached-final? an-fsm) #false) ;stub

(define (reached-final? f)
  (state=? (fsm-initial f)
       (fsm-final f)))