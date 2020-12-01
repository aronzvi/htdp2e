;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |193|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; Constants:

(define MT (empty-scene 50 50))

;; Data definitions:

(define (fn-for-posn p)
  (... (posn-x p)
       (posn-y p)))

; A Polygon is one of:
; – (list Posn Posn Posn)
; – (cons Posn Polygon)
(define triangle-p
  (list
   (make-posn 20 10)
   (make-posn 20 20)
   (make-posn 30 20)))
(define square-p
  (list
   (make-posn 10 10)
   (make-posn 20 10)
   (make-posn 20 20)
   (make-posn 10 20)))

(define (fn-for-polygon p)
  (cond
    [(empty? (rest (rest (rest p))))
     (... (fn-for-posn (first p))   ;Posn
          (fn-for-posn (second p))  ;Posn
          (fn-for-posn (third p)))] ;Posn
    [else
     (... (fn-for-posn (first p))        ;Posn
          (fn-for-polygon (rest p)))]))  ;Polygon

;; Template rules used:
;; - one of: 2 cases
;; - compound: (list Posn Posn Posn)
;; - reference: (first p) is Posn
;; - reference: (second p) is Posn
;; - reference: (third p) is Posn
;; - compound: (cons Posn Polygon)
;; - reference: (first p) is Posn
;; - self-reference: (rest p) is Polygon

; An NELoP is one of: 
; – (cons Posn '())
; – (cons Posn NELoP)
(define NELOP1 (list (make-posn 20 10)))
(define NELOP2 (list (make-posn 20 10) (make-posn 20 20) (make-posn 30 20)))

(define (fn-for-nelop nelop)
  (cond
    [(empty? (rest nelop)) (... (fn-for-posn (first nelop)))]
    [else
     (... (fn-for-posn (first nelop))      ;Posn
          (fn-for-nelop (rest nelop)))]))  ;NELoP

;; Template rules used:
;; - one of: 2 cases
;; - compound: (cons Posn '())
;; - reference: (first nelop) is Posn
;; - compound: (cons Posn NELoP)
;; - reference: (first nelop) is Posn
;; - self-reference: (rest nelop) is NELoP

;; Functions:

; Image Polygon -> Image 
; adds an image of p to img
(check-expect (render-poly MT triangle-p)
              (scene+line
               (scene+line
                (scene+line MT 20 10 20 20 "red")
                20 20 30 20 "red")
               30 20 20 10 "red"))
(check-expect
 (render-poly MT square-p)
 (scene+line
  (scene+line
   (scene+line
    (scene+line MT 10 10 20 10 "red")
    20 10 20 20 "red")
   20 20 10 20 "red")
  10 20 10 10 "red"))

#;
(define (render-poly img p)
  (render-line (connect-dots img p)
               (first p)
               (last p)))
#;
(define (render-poly img p)
  (connect-dots img (cons (last p) p)))

(define (render-poly img p)
  (connect-dots img (add-at-end p (first p))))

;; Polygon Posn -> Polygon
;; Adds posn to end of polygon
(check-expect (add-at-end (list
                           (make-posn 20 10)
                           (make-posn 20 20)
                           (make-posn 30 20))
                          (make-posn 15 15))
              (list
               (make-posn 20 10)
               (make-posn 20 20)
               (make-posn 30 20)
               (make-posn 15 15)))

(check-expect (add-at-end (list
                           (make-posn 10 10)
                           (make-posn 20 10)
                           (make-posn 20 20)
                           (make-posn 10 20))
                          (make-posn 15 15))
              (list
               (make-posn 10 10)
               (make-posn 20 10)
               (make-posn 20 20)
               (make-posn 10 20)
               (make-posn 15 15)))

;(define (add-at-end p psn) poly) ;stub

#;
(define (add-at-end p psn)
  (cond
    [(empty? (rest (rest (rest p))))
     (list (first p)  
           (second p)  
           (third p)
           psn)] 
    [else
     (cons (first p)
           (add-at-end (rest p) psn))]))

(define (add-at-end nelop psn)
  (cond
    [(empty? (rest nelop)) (list (first nelop) psn)]
    [else
     (cons (first nelop)     
          (add-at-end (rest nelop) psn))]))  


; Image NELoP -> Image 
; connects the dots in p by rendering lines in img
(check-expect (connect-dots MT (list (make-posn 10 10))) MT)
(check-expect (connect-dots MT triangle-p)
              (scene+line
               (scene+line MT 20 10 20 20 "red")
               20 20 30 20 "red"))
(check-expect(connect-dots MT square-p)
             (scene+line
              (scene+line
               (scene+line MT 10 10 20 10 "red")
               20 10 20 20 "red")
              20 20 10 20 "red"))

;(define (connect-dots img p) MT) ;stub

(define (connect-dots img nelop)
  (cond
    [(empty? (rest nelop)) img]
    [else
     (render-line
      (connect-dots img (rest nelop))
      (first nelop)
      (second nelop))]))

; Image Posn Posn -> Image 
; draws a red line from Posn p to Posn q into im
(check-expect (render-line MT (make-posn 20 10) (make-posn 20 20))
              (scene+line MT 20 10 20 20 "red"))

;(define (render-line img p q) img) ;stub

(define (render-line img p q)
  (scene+line img
              (posn-x p) (posn-y p) (posn-x q) (posn-y q)
              "red"))

;; NELoP -> Posn
;; extracts the last item from p
(check-expect (last (list (make-posn 10 20))) (make-posn 10 20))
(check-expect (last (list (make-posn 10 10)
                          (make-posn 20 10)
                          (make-posn 20 20)
                          (make-posn 10 20) )) (make-posn 10 20))

;(define (last p) (first p)) ; It is acceptable to use first here because p - a NELoP contains at least one Posn

(define (last nelop)
  (cond
    [(empty? (rest nelop)) (first nelop)]
    [else
     (last (rest nelop))])) 

(define (last-poly p)
  (cond
    [(empty? (rest (rest (rest p)))) (third p)]
    [else (last (rest p))]))


