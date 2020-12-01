;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |83|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor String String)
; interpretation (make-editor s t) describes an editor
; whose visible text is (string-append s t) with 
; the cursor displayed between s and t

#;
(define (fn-for-editor e)
  (... (editor-pre e)     ; String
       (editor-post e)))  ; String

(define EDITOR1 (make-editor "hello" "world"))
(define EDITOR2 (make-editor "goodbye" " pizza plooza"))

(define MTSCN (empty-scene 200 20))
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