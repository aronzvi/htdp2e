;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |177|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Data definitions:

; An Lo1S is one of: 
; – '()
; – (cons 1String Lo1S)
; interp. a list of 1String
(define GOOD
  (cons "g" (cons "o" (cons "o" (cons "d" '())))))
(define ALL
  (cons "a" (cons "l" (cons "l" '()))))
(define LLA
  (cons "l" (cons "l" (cons "a" '()))))

(define (fn-for-lo1s lo1s)
  (cond
    [(empty? lo1s) (...)]
    [else
     (... (first lo1s)                  ;1String
          (fn-for-lo1s (rest lo1s)))])) ;Lo1S 

;; Template rukes used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons 1String Lo1S)

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor Lo1S Lo1S)
; interp. a text editor with
;  - pre. The text to the left of the cursor. stored in reverse order.
;         The cursor is at the begining of pre followed by last letter before cursor
;  - post. The text to the right of the cursor
(define E1 (make-editor ALL GOOD))
(define E2 (make-editor LLA GOOD))

(define (fn-for-editor e)
  (... (fn-for-lo1s (editor-pre e))     ;Lo1S
       (fn-for-lo1s (editor-post e))))  ;Lo1S

;; Template rules used:
;; - compound: 2 fields
;; - reference: (editor-pre e) is Lo1S
;; - reference: (editor-pre e) is Lo1S

;; Functions:

;; String String -> Editor
;; produces an Editor from strings left and right to cursor.
(check-expect (create-editor "" "") (make-editor empty empty))
(check-expect (create-editor "left" "right") (make-editor (cons "t" (cons "f" (cons "e" (cons "l" empty))))
                                                          (cons "r" (cons "i" (cons "g" (cons "h" (cons "t" empty)))))))

;(define (create-editor left right) (make-editor empty empty)) ;stub

#;
(define (create-editor left right) ;Template
  (... left right))

;; Template rules used:
;; - atomic non disitinct

(define (create-editor left right)
  (make-editor (reverse (explode left)) (explode right)))

