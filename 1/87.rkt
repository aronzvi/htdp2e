;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |87|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define-struct editor [s ci])
; An Editor is a structure:
;   (make-editor String Number)
; interpretation (make-editor s ci) describes an editor
; whose visible text is s with the cursor positioned after ci characters from the left 

#;
(define (fn-for-editor ed)
  (... (editor-s ed)    ; String
       (editor-ci ed))) ; Number

(define EDITOR1 (make-editor "helloworld" 5))
(define EDITOR2 (make-editor "goodbyepizza plooza" 7))
(define EDITOR-CURSOR-AT-BEGINING (make-editor "popo" 0))
(define EDITOR-CURSOR-AT-END (make-editor "peepee" 6))
(define EMPTY-EDITOR (make-editor "" 0))

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
                                                      (text "pizza plooza" TEXT-SIZE TEXT-COLOR))
                                              MTSCN))


; (define (render editor) MTSCN) ;stub

#;
(define (render e) ; template
  (... (editor-s e)     
       (editor-ci e)))

(define (render e)
  (overlay/align "left" "center"
                 (beside (text (editor-pre e) TEXT-SIZE TEXT-COLOR)
                         CURSOR
                         (text (editor-post e) TEXT-SIZE TEXT-COLOR))
                 MTSCN))

; Editor -> String
; produces the editor text to the left of the cursor
(check-expect (editor-pre EDITOR1) (substring (editor-s EDITOR1) 0 (editor-ci EDITOR1)))
(check-expect (editor-pre EDITOR-CURSOR-AT-BEGINING) (substring (editor-s EDITOR-CURSOR-AT-BEGINING) 0 (editor-ci EDITOR-CURSOR-AT-BEGINING)))
(check-expect (editor-pre EMPTY-EDITOR) "")

; (define (editor-pre e) "") ;stub

(define (editor-pre ed)
  (substring (editor-s ed)
             0
             (editor-ci ed))) 

; Editor -> String
; produces the editor text to the right the cursor
(check-expect (editor-post EDITOR1) (substring (editor-s EDITOR1) (editor-ci EDITOR1)))
(check-expect (editor-post EDITOR-CURSOR-AT-END) "")
(check-expect (editor-post EMPTY-EDITOR) "")

;(define (editor-post e) "") ; stub

(define (editor-post ed)
  (substring (editor-s ed) 
             (editor-ci ed)))

; Editor KeyEvent -> Editor
; produces a new editor from given editor and keyevent
(check-expect (edit EDITOR1 "f") (make-editor  "hellofworld" 6))
(check-expect (edit EMPTY-EDITOR "f") (make-editor  "f"  1))
(check-expect (edit EDITOR1 "\b") (make-editor  "hellworld" 4))
(check-expect (edit EDITOR-CURSOR-AT-BEGINING "\b") EDITOR-CURSOR-AT-BEGINING)
(check-expect (edit EDITOR1 "\t") EDITOR1)
(check-expect (edit EDITOR1 "\r") EDITOR1)
(check-expect (edit EDITOR1 "left") (make-editor "helloworld" 4))
(check-expect (edit EDITOR-CURSOR-AT-BEGINING "left") EDITOR-CURSOR-AT-BEGINING)
(check-expect (edit EDITOR1 "right") (make-editor "helloworld" 6))
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

; Editor -> Editor
; deletes the character immediately to the left of the cursor if there are any
(check-expect (editor-delete-left EDITOR1) (make-editor  "hellworld" 4))
(check-expect (editor-delete-left EDITOR-CURSOR-AT-BEGINING) EDITOR-CURSOR-AT-BEGINING)

;(define (editor-delete-left ed) ed) ; stub

(define (editor-delete-left ed)
  (if (> (editor-ci ed) 0)
      (make-editor (string-delete (editor-s ed) (- (editor-ci ed) 1))
                   (- (editor-ci ed) 1))
      ed))

; String Number -> String
; deletes the ith position from str
(check-expect (string-delete "hello" 0) "ello")
(check-expect (string-delete "hello" 3) "helo")
(check-expect (string-delete "" 0) "")

; (define (string-delete str i) str) ;stub

(define (string-delete str i)
  (if (= (string-length str) 0)
      ""
      (string-append (substring str 0 i) (substring str (+ i 1)))))

; Editor -> Boolean
; returns true if rendered editor width is smaller than canvase, else returns false
(check-expect (editor-fits-canvas? EDITOR1 BUFFER-WIDTH) #true)
(check-expect (editor-fits-canvas? EDITOR1 75)(<= (image-width (render EDITOR1))
                                                  75))

;(define (editor-fits-canvas? ed canvas-width) #false) ;stub

(define (editor-fits-canvas? ed canvas-width)
  (<= (image-width (render ed))
      canvas-width))

; Editor KeyEvent -> Editor
; adds a single-character KeyEvent to the end of the pre field of ed
(check-expect (editor-add-to-end EDITOR1 "f") (make-editor "hellofworld" 6))
(check-expect (editor-add-to-end EMPTY-EDITOR "f") (make-editor  "f"  1))

;(define (editor-add-to-end ed ke) ed) ; stub

(define (editor-add-to-end ed ke)
  (make-editor (string-insert ke (editor-s ed) (editor-ci ed)) 
               (+ (editor-ci ed) 1)))

; String String Number -> String
; inserts strin into str at pos shifting str at pos forward
(check-expect (string-insert "f" "hello" 4) "hellfo")
(check-expect (string-insert "f" "" 0) "f")

;(define (string-insert strinin str pos) str) ; stub

(define (string-insert strin str i)
  (if ( = (string-length str) 0)
      strin
      (string-append (substring str 0 i) strin (substring str i))))

; Editor -> Editor
; moves the cursor one character to the left if any
(check-expect (editor-move-cursor-left EDITOR1) (make-editor "helloworld" 4))
(check-expect (editor-move-cursor-left EDITOR-CURSOR-AT-BEGINING) EDITOR-CURSOR-AT-BEGINING)

; (define (editor-move-cursor-left ed) ed) ;stub

(define (editor-move-cursor-left ed)
  (if (= (editor-ci ed) 0)
      ed
      (make-editor (editor-s ed)    
                   (- (editor-ci ed) 1)))) 

; Editor -> Editor
; moves the cursor one character to the right if any
(check-expect (editor-move-cursor-right EDITOR1) (make-editor "helloworld" 6))
(check-expect (editor-move-cursor-right EDITOR-CURSOR-AT-END) EDITOR-CURSOR-AT-END)

; (define (editor-move-cursor-right ed) ed) ;stub

(define (editor-move-cursor-right ed)
  (if (= (editor-ci ed) (string-length (editor-s ed)))
      ed
      (make-editor (editor-s ed)    
                   (+ (editor-ci ed) 1))))

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

; String -> Editor
(define (run s)
  (big-bang (make-editor s 0) 
    (to-draw render)   
    (on-key edit))) 
