;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |86|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor String String)
; interpretation (make-editor s t) describes an editor
; whose visible text is (string-append s t) with 
; the cursor displayed between s and t

#;
(define (fn-for-editor ed)
  (... (editor-pre ed)     ; String
       (editor-post ed)))  ; String

(define EDITOR1 (make-editor "hello" "world"))
(define EDITOR2 (make-editor "goodbye" " pizza plooza"))
(define EDITOR-CURSOR-AT-BEGINING (make-editor "" "popo"))
(define EDITOR-CURSOR-AT-END (make-editor "peepee" ""))
(define EMPTY-EDITOR (make-editor "" ""))

(define BUFFER-WIDTH 200)
(define BUFFER-HEIGHT 20)
(define MTSCN (empty-scene BUFFER-WIDTH BUFFER-HEIGHT))
(define CURSOR (rectangle 1 20 "solid" "red"))
(define TEXT-SIZE 16)
(define TEXT-COLOR "black")

; Editor -> Image
; Renders the editor's text and cursor
(check-expect (render EDITOR1) (overlay/align "left" "center"
                                              (beside (text "hello" TEXT-SIZE TEXT-COLOR)
                                                      CURSOR
                                                      (text "world" TEXT-SIZE TEXT-COLOR))
                                              MTSCN))

(check-expect (render EDITOR2) (overlay/align "left" "center"
                                              (beside (text "goodbye" TEXT-SIZE TEXT-COLOR)
                                                      CURSOR
                                                      (text " pizza plooza" TEXT-SIZE TEXT-COLOR))
                                              MTSCN))

; (define (render editor) MTSCN) ;stub

#;
(define (render e) ; template
  (... (editor-pre e)     
       (editor-post e)))

(define (render e)
  (overlay/align "left" "center"
                 (beside (text (editor-pre e) TEXT-SIZE TEXT-COLOR)
                         CURSOR
                         (text (editor-post e) TEXT-SIZE TEXT-COLOR))
                 MTSCN))

; Editor KeyEvent -> Editor
; produces a new editor from given editor and keyevent
(check-expect (edit EDITOR1 "f") (make-editor  "hellof"  "world"))
(check-expect (edit EMPTY-EDITOR "f") (make-editor  "f"  ""))
(check-expect (edit EDITOR1 "\b") (make-editor  "hell"  "world"))
(check-expect (edit EDITOR-CURSOR-AT-BEGINING "\b") EDITOR-CURSOR-AT-BEGINING)
(check-expect (edit EDITOR1 "\t") EDITOR1)
(check-expect (edit EDITOR1 "\r") EDITOR1)
(check-expect (edit EDITOR1 "left") (make-editor "hell" "oworld"))
(check-expect (edit EDITOR-CURSOR-AT-BEGINING "left") EDITOR-CURSOR-AT-BEGINING)
(check-expect (edit EDITOR1 "right") (make-editor "hellow" "orld"))
(check-expect (edit EDITOR-CURSOR-AT-END "right") EDITOR-CURSOR-AT-END)
(check-expect (edit EDITOR1 "up") EDITOR1)
(check-expect (edit EDITOR1 "down") EDITOR1)

; (define (edit ed ke) EMPTY-EDITOR) ; stub

(define (edit ed ke)
  (if (= (string-length ke) 1)
      (cond ; single char
        [(string=? ke "\b") (editor-delete-left ed)]
        [(string=? ke "\t") ed]
        [(string=? ke "\r") ed]
        [else
         (if (editor-fits-canvas? (editor-add-to-end ed ke) BUFFER-WIDTH)
             (editor-add-to-end ed ke)
             ed)])
      (cond ; arrows
        [(string=? ke "left") (editor-move-cursor-left ed)]
        [(string=? ke "right") (editor-move-cursor-right ed)]
        [else ed])))

; Editor -> Boolean
; returns true if rendered editor width is smaller than canvase, else returns false
(check-expect (editor-fits-canvas? EDITOR1 BUFFER-WIDTH) #true)
(check-expect (editor-fits-canvas? EDITOR1 75)(<= (image-width (render EDITOR1))
                                                 75))

(define (editor-fits-canvas? ed canvas-width)
  (<= (image-width (render ed))
     canvas-width))

; Editor -> Editor
; deletes the character immediately to the left of the cursor if there are any
(check-expect (editor-delete-left EDITOR1) (make-editor  "hell"  "world"))
(check-expect (editor-delete-left EDITOR-CURSOR-AT-BEGINING) EDITOR-CURSOR-AT-BEGINING)

;(define (editor-delete-left ed) ed) ; stub

(define (editor-delete-left ed)
  (make-editor (string-remove-last (editor-pre ed))     
               (editor-post ed)))  

; Editor KeyEvent -> Editor
; adds a single-character KeyEvent to the end of the pre field of ed
(check-expect (editor-add-to-end EDITOR1 "f") (make-editor "hellof"  "world"))
(check-expect (editor-add-to-end EMPTY-EDITOR "f") (make-editor  "f"  ""))

;(define (editor-add-to-end ed ke) ed) ; stub

(define (editor-add-to-end ed ke)
  (make-editor (string-append (editor-pre ed) ke)    
               (editor-post ed)))  

; Editor -> Editor
; moves the cursor one character to the left if any
(check-expect (editor-move-cursor-left EDITOR1) (make-editor "hell" "oworld"))
(check-expect (editor-move-cursor-left EDITOR-CURSOR-AT-BEGINING) EDITOR-CURSOR-AT-BEGINING)

;(define (editor-move-cursor-left ed) ed) ;stub

(define (editor-move-cursor-left ed)
  (make-editor (string-remove-last (editor-pre ed))     
               (string-append (string-last (editor-pre ed)) (editor-post ed))))  

; Editor -> Editor
; moves the cursor one character to the right if any
(check-expect (editor-move-cursor-right EDITOR1) (make-editor "hellow" "orld"))
(check-expect (editor-move-cursor-right EDITOR-CURSOR-AT-END) EDITOR-CURSOR-AT-END)

; (define (editor-move-cursor-right ed) ed) ;stub

(define (editor-move-cursor-right ed)
  (make-editor (string-append (editor-pre ed) (string-first (editor-post ed)))    
               (string-rest (editor-post ed))))  


; String -> String
; Given a non-empty string, produces a string like the given one with the last character removed
; Given an empty string, produces an empty string
(check-expect (string-remove-last "hello") "hell")
(check-expect (string-remove-last "") "")

(define (string-remove-last s)
  (if ( > (string-length s) 0)
      (substring s
                 0
                 (-  (string-length s) 1))
      s))

; String -> String
; extracts the last character from s.
; Given an empty string, produces an empty string
(check-expect (string-last "hello") "o")
(check-expect (string-last "") "")

(define (string-last s)
  (if ( > (string-length s) 0)
      (string-ith s (- (string-length s) 1))
      s))

; String -> String
; extracts the first character from a string
; given an empty string, produces an empty string
(check-expect(string-first "hello") "h")
(check-expect(string-first "") "")

(define (string-first s)
  (if ( > (string-length s) 0)
      (string-ith s 0)
      s))

; String -> String
; produces a string like the given one with the first character removed
; given an empty string, produces an empty string
(check-expect (string-rest "world") "orld")
(check-expect (string-rest "") "")

(define (string-rest s)
  (if ( > (string-length s) 0)
      (substring s 1)
      s))

; String -> Editor
(define (run pre)
  (big-bang (make-editor pre "") 
    (to-draw render)   
    (on-key edit)))    