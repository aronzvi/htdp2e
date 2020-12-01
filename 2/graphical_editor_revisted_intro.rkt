;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname graphical_editor_revisted_intro) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An Lo1S is one of: 
; – '()
; – (cons 1String Lo1S)
(define good
  (cons "g" (cons "o" (cons "o" (cons "d" '())))))
(define all
  (cons "a" (cons "l" (cons "l" '()))))
(define lla
  (cons "l" (cons "l" (cons "a" '()))))

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor Lo1S Lo1S)  
; data example 1: 
(make-editor all good)
; data example 2:
(make-editor lla good)

; Lo1s -> Lo1s 
; produces a reverse version of the given list 
(check-expect (rev empty) empty)
(check-expect (rev (cons "a" empty)) (cons "a" empty))
(check-expect
  (rev (cons "a" (cons "b" (cons "c" '()))))
  (cons "c" (cons "b" (cons "a" '()))))
 
;(define (rev l) l)

#;
(define (rev l)
  (cond
    [(empty? l) ...]
    [else (... (first l) ...
           ... (rev (rest l)) ...)]))
#;
(define (rev l)
  (cond
    [(empty? l) empty]
    [else
     (append (rev (rest l))
             (cons (first l) empty))]))

(define (rev l)
  (cond
    [(empty? l) empty]
    [else
     (add-at-end (rev (rest l)) (first l))]))

; Lo1s 1String -> Lo1s
; creates a new list by adding s to the end of l
(check-expect (add-at-end empty "a") (cons "a" empty))
(check-expect
  (add-at-end (cons "c" (cons "b" '())) "a")
  (cons "c" (cons "b" (cons "a" '()))))
 
;(define (add-at-end l s) l) ;stub

#;
(define (add-at-end l s)
  (cond
    [(empty? l) ...]
    [else (... (first l) ...
           ... (add-at-end (rest l) s) ...)]))

(define (add-at-end l s)
  (cond
    [(empty? l) (cons s empty)]
    [else
     (cons (first l) 
           (add-at-end (rest l) s))]))



