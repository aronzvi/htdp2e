;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |180|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Constants:

(define HEIGHT 20) ; the height of the editor 
(define WIDTH 200) ; its width 
(define FONT-SIZE 16) ; the font size 
(define FONT-COLOR "black") ; the font color 
 
(define MT (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))

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
(define ED1 (make-editor LLA GOOD))

(define (fn-for-editor ed)
  (... (fn-for-lo1s (editor-pre ed))     ;Lo1S
       (fn-for-lo1s (editor-post ed))))  ;Lo1S

;; Template rules used:
;; - compound: 2 fields
;; - reference: (editor-pre e) is Lo1S
;; - reference: (editor-pre e) is Lo1S

;; Functions:

; main : String -> Editor
; launches the editor given some initial string 
(define (main s)
  (big-bang (create-editor s "")
    [on-key editor-kh]
    [to-draw editor-render]))

; Editor -> Image
; renders an editor as an image of the two texts 
; separated by the cursor
(check-expect (editor-render (create-editor "" "post"))
              (place-image/align
               (beside (text "" FONT-SIZE FONT-COLOR)
                       CURSOR
                       (text "post" FONT-SIZE FONT-COLOR))
               1 1
               "left" "top"
               MT))
(check-expect (editor-render (create-editor "pre" ""))
              (place-image/align
               (beside (text "pre" FONT-SIZE FONT-COLOR)
                       CURSOR
                       (text "" FONT-SIZE FONT-COLOR))
               1 1
               "left" "top"
               MT))
(check-expect (editor-render (create-editor "" ""))
              (place-image/align
               (beside (text "" FONT-SIZE FONT-COLOR)
                       CURSOR
                       (text "" FONT-SIZE FONT-COLOR))
               1 1
               "left" "top"
               MT))
(check-expect (editor-render (create-editor "pre" "post"))
              (place-image/align
               (beside (text "pre" FONT-SIZE FONT-COLOR)
                       CURSOR
                       (text "post" FONT-SIZE FONT-COLOR))
               1 1
               "left" "top"
               MT))
;(define (editor-render ed) MT) ;stub

(define (editor-render ed)
  (place-image/align
   (beside (editor-text (reverse (editor-pre ed)))
           CURSOR
           (editor-text (editor-post ed)))
   1 1
   "left" "top"
   MT))

;; Lo1S -> Image
;; produces a text image from the given lo1s
(check-expect
  (editor-text
   (cons "p" (cons "o" (cons "s" (cons "t" '())))))
  (text "post" FONT-SIZE FONT-COLOR))
(check-expect
  (editor-text
   (cons "e" (cons "r" (cons "p" empty))))
  (text "erp" FONT-SIZE FONT-COLOR))

;(define (editor-text s) (text "" FONT-SIZE FONT-COLOR)) ;stub

(define (editor-text s)
  (cond
    [(empty? s) (text "" FONT-SIZE FONT-COLOR)]
    [else
     (beside  (text (first s) FONT-SIZE FONT-COLOR)                  
          (editor-text (rest s)))]))  
 
; Editor KeyEvent -> Editor
; deals with a key event, given some editor
(check-expect (editor-kh (create-editor "" "") "e")
              (create-editor "e" ""))
(check-expect
 (editor-kh (create-editor "cd" "fgh") "e")
 (create-editor "cde" "fgh"))
(check-expect
 (editor-kh (create-editor "" "") "\b")
 (create-editor "" ""))
(check-expect
 (editor-kh (create-editor "" "fgh") "\b")
 (create-editor "" "fgh"))
(check-expect
 (editor-kh (create-editor "cdfgh" "") "\b")
 (create-editor "cdfg" ""))
(check-expect
 (editor-kh (create-editor "cd" "fgh") "\b")
 (create-editor "c" "fgh"))
(check-expect
 (editor-kh (create-editor "" "") "left")
 (create-editor "" ""))
(check-expect
 (editor-kh (create-editor "" "fgh") "left")
 (create-editor "" "fgh"))
(check-expect
 (editor-kh (create-editor "cdfgh" "") "left")
 (create-editor "cdfg" "h"))
(check-expect
 (editor-kh (create-editor "cd" "fgh") "left")
 (create-editor "c" "dfgh"))
(check-expect
 (editor-kh (create-editor "" "") "right")
 (create-editor "" ""))
(check-expect
 (editor-kh (create-editor "" "fgh") "right")
 (create-editor "f" "gh"))
(check-expect
 (editor-kh (create-editor "cdfgh" "") "right")
 (create-editor "cdfgh" ""))
(check-expect
 (editor-kh (create-editor "cd" "fgh") "right")
 (create-editor "cdf" "gh"))

;(define (editor-kh ed ke) ed) ;stub

#;
(define (editor-kh ed k) ;Template
  (cond
    [(key=? k "left") ...]
    [(key=? k "right") ...]
    [(key=? k "\b") ...]
    [(key=? k "\t") ...] 
    [(key=? k "\r") ...]
    [(= (string-length k) 1) ...]
    [else ...]))

(define (editor-kh ed k)
  (cond
    [(key=? k "left") (editor-lft ed)]
    [(key=? k "right") (editor-rgt ed)]
    [(key=? k "\b") (editor-del ed)]
    [(key=? k "\t") ed]
    [(key=? k "\r") ed]
    [(= (string-length k) 1) (editor-ins ed k)]
    [else ed]))

;; Editor -> Editor
;; moves the cursor position one 1String left, 
;; if possible 
(check-expect
 (editor-lft (make-editor empty empty))
 (make-editor empty empty))
(check-expect
 (editor-lft (make-editor empty (explode "fgh")))
 (make-editor empty (explode "fgh")))
(check-expect
 (editor-lft (make-editor (explode "cdfgh") empty))
 (make-editor (explode "dfgh") (cons "c" empty)))
(check-expect
 (editor-lft (make-editor (explode "cd") (explode "fgh")))
 (make-editor (cons "d" empty ) (explode "cfgh")))

;(define (editor-lft ed) ed) ;stub

(define (editor-lft ed)
  (if (empty? (editor-pre ed))
      ed
      (make-editor (rest (editor-pre ed))    
                   (cons (first (editor-pre ed))
                         (editor-post ed)))))  

;; Editor -> Editor
;; moves the cursor position one 1String right, 
;; if possible 
(check-expect
 (editor-rgt (make-editor empty empty))
 (make-editor empty empty))
(check-expect
 (editor-rgt (make-editor (explode "cdfgh") empty))
 (make-editor (explode "cdfgh") empty))
(check-expect
 (editor-rgt (make-editor empty (explode "fgh")))
 (make-editor (cons "f" empty) (explode "gh")))
(check-expect
 (editor-rgt (make-editor (explode "cd") (explode "fgh")))
 (make-editor (explode "fcd") (explode "gh")))

;(define (editor-rgt ed) ed) ;stub

(define (editor-rgt ed)
  (if (empty? (editor-post ed))
      ed
      (make-editor (cons (first (editor-post ed)) (editor-pre ed))    
                   (rest (editor-post ed)))))  

;; Editor -> Editor
;; deletes a 1String to the left of the cursor,
;; if possible 
(check-expect
 (editor-del (make-editor empty empty))
 (make-editor empty empty))
(check-expect
 (editor-del (make-editor empty (explode "fgh")))
 (make-editor empty (explode "fgh")))
(check-expect
 (editor-del (make-editor (explode "cdfgh") empty))
 (make-editor (explode "dfgh") empty))
(check-expect
 (editor-del (make-editor (explode "cd") (explode "fgh")))
 (make-editor (explode "d") (explode "fgh")))

;(define (editor-del ed) ed) ;stub

(define (editor-del ed)
  (if (empty? (editor-pre ed))
      ed
      (make-editor (rest (editor-pre ed))    
                   (editor-post ed))))  

;; Editor 1String -> Editor
;; insert the 1String k between pre and post
(check-expect (editor-ins (make-editor empty empty) "e")
              (make-editor (cons "e" empty) empty))
(check-expect (editor-ins
               (make-editor (cons "d" empty)
                            (cons "f" (cons "g" empty)))
               "e")
              (make-editor (cons "e" (cons "d" empty))
                           (cons "f" (cons "g" empty))))

;(define (editor-ins ed k) ed) ;stub

(define (editor-ins ed k)
  (make-editor (cons k (editor-pre ed))
               (editor-post ed)))  

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