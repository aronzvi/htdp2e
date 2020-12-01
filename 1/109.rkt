;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |109|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; ==================
;; Constants:

(define HEIGHT 100)
(define WIDTH 100)
(define START (rectangle WIDTH HEIGHT "solid" "white"))
(define EXPECT (rectangle WIDTH HEIGHT "solid" "yellow"))
(define FINISHED (rectangle WIDTH HEIGHT "solid" "green"))
(define ERROR (rectangle WIDTH HEIGHT "solid" "red"))

(define AA "start, expect an 'a'")
(define BB "expect 'b', 'c', or 'd'")
(define DD "finished")
(define ER "error, illegal key")


;; ==================
;; Data definitions:

;; ExpectsToSee is one of:
;; – AA
;; – BB
;; – DD 
;; – ER
;; interp. state machine for recognizing the sequence of: a (b|c)* d
;;   AA is the initial state before any keyevent has occured
;;   BB is after 'a' or any number of 'b' or 'c' keyevent occurances
;;   DD is after 'd' - the final letter occured
;;   ERR is if any “bad” key event occurs - first not 'a' or second and onwards not 'b' nor 'c' nor 'd'

(define ETS1 AA) ; initial state
(define ETS2 BB) ; expecting - 'a', 'b' or 'c' occured
(define ETS3 DD) ; finished - 'd' occured
(define ETS4 ER) ; “bad” key event occured

(define (fn-for-expected-to-see ets)
  (cond [(equal? ets AA) (...)]
        [(equal? ets BB) (...)]
        [(equal? ets DD) (...)]
        [(equal? ets ER) (...)]))

;; Template rules used:
;; - one of: 4 cases
;; - atomic distinct: AA
;; - atomic distinct: BB
;; - atomic distinct: DD
;; - atomic distinct: ER

;; ==================
;; Functions:

;; ExpectsToSee -> ExpectsToSee
;; start the world with (main AA)
;; 
(define (main ets)
  (big-bang ets                         ; ExpectsToSee
            (to-draw   render)          ; ExpectsToSee -> Image
            (on-key    transition)))    ; ExpectsToSee KeyEvent -> ExpectsToSee

;; ExpectsToSee -> Image
;; displays the state
(check-expect (render AA) START)
(check-expect (render BB) EXPECT)
(check-expect (render DD) FINISHED)
(check-expect (render ER) ERROR)

;(define (render ets) (square 0 "solid" "white")) ; stub

; Template from ExpectsToSee

(define (render ets)
  (cond [(equal? ets AA) START]
        [(equal? ets BB) EXPECT]
        [(equal? ets DD) FINISHED]
        [(equal? ets ER) ERROR]))

;; ExpectsToSee KeyEvent -> ExpectsToSee
;; transitions to the next state
(check-expect (transition AA "a") BB)
(check-expect (transition AA "b") ER)
(check-expect (transition AA "c") ER)
(check-expect (transition AA "d") ER)
(check-expect (transition AA "z") ER)

(check-expect (transition BB "b") BB)
(check-expect (transition BB "c") BB)
(check-expect (transition BB "d") DD)
(check-expect (transition BB "a") ER)
(check-expect (transition BB "y") ER)

(check-expect (transition ER "a") ER) ; no transition from error
(check-expect (transition DD "a") DD) ; no transition from finished

; (define (transition ets ke) ets) ; stub

; Template from ExpectsToSee

(define (transition ets ke)
  (cond [(equal? ets AA) (transition-from-start ke)]
        [(equal? ets BB) (transition-from-expect ke)]
        [(equal? ets DD) DD]
        [(equal? ets ER) ER]))

;; ExpectsToSee KeyEvent -> ExpectsToSee
;; transition from AA
(check-expect (transition AA "a") BB)
(check-expect (transition AA "b") ER)
(check-expect (transition AA "c") ER)
(check-expect (transition AA "d") ER)
(check-expect (transition AA "z") ER)

; (define (transition-from-start ke) ER) ; stub

#;
(define (transition-from-start ke)
  (cond [(key=? "a" ke) (...)]
        [else
         (...)]))
;; Template formed using the large enumeration special case

(define (transition-from-start ke)
  (cond [(key=? "a" ke) BB]
        [else ER]))

;; ExpectsToSee KeyEvent -> ExpectsToSee
;; transition from BB
(check-expect (transition BB "b") BB)
(check-expect (transition BB "c") BB)
(check-expect (transition BB "d") DD)
(check-expect (transition BB "a") ER)
(check-expect (transition BB "y") ER)

; (define (transition-from-expect ke) ER) ; stub

#;
(define (transition-from-expect ke)
  (cond [(key=? "b" ke) (...)]
        [(key=? "c" ke) (...)]
        [(key=? "d" ke) (...)]
        [else
         (...)]))
;; Template formed using the large enumeration special case

(define (transition-from-expect ke)
  (cond [(key=? "b" ke) BB]
        [(key=? "c" ke) BB]
        [(key=? "d" ke) DD]
        [else ER]))
