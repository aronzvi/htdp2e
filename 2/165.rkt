;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |165|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Constants:

(define ROBOT-SUBST "r2d2")

;; Data definitions:

;; List-of-strings is one of:
;; - empty
;; (cons String List-of-numbers)
;; interp. a list of numbers

(define LOS1 empty)
(define LOS2 (cons "robot" LOS1))
(define LON3 (cons "Booboo" LOS2))

#;
(define (fn-for-los los)
  (cond
    [(empty? los) (...)]
    [else
     (... (first los)                 ;String
          (fn-for-los (rest los)))])) ;List-of-strings

;; Template rules used:
;; - atomic distinct: empty
;; - compound: (cons String List-of-numbers)
;; - self-reference: (rest los) is List-of-strings

;; Functions:

;; List-of-strings -> List-of-strings
;; replaces all occurrences of "robot" with "r2d2" in given toy descriptions; all other descriptions remain the same
(check-expect (subst-robot empty) empty)
(check-expect (subst-robot (cons "robot" empty)) (cons ROBOT-SUBST empty))
(check-expect (subst-robot (cons "boo" empty)) (cons "boo" empty))
(check-expect (subst-robot (cons "robot" (cons "booo" (cons "robot" empty)))) (cons ROBOT-SUBST (cons "booo" (cons ROBOT-SUBST empty))))

;(define (subst-robot toy-descripts) toy-descripts) ;stub

(define (subst-robot toy-descripts)
  (cond
    [(empty? toy-descripts) empty]
    [else
     (cons
      (if (string=? (first toy-descripts) "robot")
          ROBOT-SUBST
          (first toy-descripts))
      (subst-robot (rest toy-descripts)))])) 

;; String String List-of-strings -> List-of-strings
;; produces a new list of strings by substituting all occurrences of old with new
(check-expect (substitute "new" "old" empty) empty)
(check-expect (substitute "new" "old" (cons "old" empty)) (cons "new" empty))
(check-expect (substitute "new" "old" (cons "old" (cons "pee" (cons "old" empty)))) (cons "new" (cons "pee" (cons "new" empty))))

;(define (substitute new old los) los) ;stub

(define (substitute new old los)
  (cond
    [(empty? los) empty]
    [else
     (cons
      (if (string=? (first los) old)
          new
          (first los))                 
      (substitute new old (rest los)))]))

;; List-of-strings -> List-of-strings
;; replaces all occurrences of "robot" with "r2d2" in given toy descriptions; all other descriptions remain the same
(check-expect (subst-robot.v2 empty) empty)
(check-expect (subst-robot.v2 (cons "robot" empty)) (cons ROBOT-SUBST empty))
(check-expect (subst-robot.v2 (cons "boo" empty)) (cons "boo" empty))
(check-expect (subst-robot.v2 (cons "robot" (cons "booo" (cons "robot" empty)))) (cons ROBOT-SUBST (cons "booo" (cons ROBOT-SUBST empty))))

;(define (subst-robot.v2 toy-descripts) toy-descripts) ;stub

(define (subst-robot.v2 toy-descripts)
  (substitute ROBOT-SUBST "robot" toy-descripts)) 




