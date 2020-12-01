;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |381|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An FSM-State is a String that specifies a color

; An XMachine is a nested list of this shape:
;   `(machine ((initial ,FSM-State)) [List-of X1T])
; An X1T is a nested list of this shape:
;   `(action ((state ,FSM-State) (next ,FSM-State)))


; An Xexpr.v2 is a list: 
; – (cons Symbol Body)
; – (cons Symbol (cons [List-of Attribute] Body))
; where Body is short for [List-of Xexpr.v2]
; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

(define XM0
  '(machine ((initial "red"))
            (action ((state "red") (next "green")))
            (action ((state "green") (next "yellow")))
            (action ((state "yellow") (next "red")))))

; An XMachine.v1 is a nested list of this shape:
;   (list 'machine (list (list 'initial FSM-State)) [List-of X1T.v1])
; An X1T.v1 is a nested list of this shape:
;   (list 'action (list (list 'state FSM-State) (list 'next FSM-State)))

(define XM.v1-0
  (list 'machine (list (list 'initial "red"))
        (list 'action (list (list 'state "red") (list 'next "green")))
        (list 'action (list (list 'state "green") (list 'next "yellow")))
        (list 'action (list (list 'state "yellow") (list 'next "red")))))

; An XMachine.v2 is a nested list of this shape:
; (cons 'machine (cons (cons (cons 'initial (cons FSM-State empty)) empty) (cons [List-of X1T.v2] empty)))
; An X1T.v2 is a nested list of this shape:
; (cons 'action (cons (cons (cons 'state (cons FSM-State empty)) (cons (cons 'next (cons FSM-State empty)) empty)) empty))

(define XM.v2-0
  (cons 'machine  (cons (cons (cons 'initial (cons "red" empty)) empty)
                        (cons (cons 'action (cons (cons (cons 'state (cons "red" empty)) (cons (cons 'next (cons "green" empty)) empty)) empty))
                              (cons (cons 'action (cons (cons (cons 'state (cons "green" empty)) (cons (cons 'next (cons "yellow" empty)) empty)) empty))
                                    (cons (cons 'action (cons (cons (cons 'state (cons "yellow" empty)) (cons (cons 'next (cons "red" empty)) empty)) empty)) empty))))))
