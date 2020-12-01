;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |320|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Data definitions

; An S-expr is one of: 
; – Number
; – String
; – Symbol
; – [List-of S-expr]

(define SEXP0 3)
(define SEXP1 "eff")
(define SEXP2 's)
(define SEXP3 '())
(define SEXP4 (cons 'hello (cons 20.12 (cons "world" '()))))
(define SEXP5 (cons (cons 'hello (cons 20.12 (cons "world" '())))
                    '()))

#;
(define (fn-for-sexp sexp)
  (cond [(number? sexp) (... sexp)]
        [(string? sexp) (... sexp)]
        [(symbol? sexp) (... sexp)]
        [else
         (... (fn-for-sl sexp))])) ;[List-of S-expr]

#;
(define (fn-for-sl sl)
  (cond [(empty? sl) (...)]
        [else
         (... (fn-for-sexp (first sl))   ;S-expr
              (fn-for-sl (rest sl)))]))  ;SL

; S-expr Symbol -> N
; counts all occurrences of sy in sexp
(check-expect (count 'world 'hello) 0)
(check-expect (count '(world hello) 'hello) 1)
(check-expect (count '(((world) hello) hello) 'hello) 2)

;(define (count sexp sy) 0) ;stub

(define (count sexp sy)
  (local ((define (count-sl sl)
            (cond [(empty? sl) 0]
                  [else
                   (+ (count (first sl) sy)   
                      (count-sl (rest sl)))])))
    (cond [(number? sexp) 0]
          [(string? sexp) 0]
          [(symbol? sexp) (if (symbol=? sexp sy) 1 0)]
          [else
           (count-sl sexp)]))) 

; An S-expr.v2 is one of: 
; – Number
; – String
; – Symbol
; – '()
; – (cons S-expr.v2 S-expr.v2)

(define SEXP-V2-0 4)
(define SEXP-V2-1 's)
(define SEXP-V2-2 "ddd")
(define SEXP-V2-3 empty)
(define SEXP-V2-4 '(d kk (3 "4" (1 "gg" f))))
(define SL0 empty)
(define SL1 '(1 s "d"))
(define SL2 '(1 s (ff 4 ("dd" f)) "d"))

#;
(define (fn-for-sexp.v2 sexp)
  (cond [(number? sexp) (... sexp)]
        [(string? sexp) (... sexp)]
        [(symbol? sexp) (... sexp)]
        [(empty? sexp) (...)]
        [else
         (... (first sexp)    ;S-expr.v2
              (rest sexp))])) ;S-expr.v2

; S-expr.v2 Symbol -> N
; counts all occurrences of sy in sexp
(check-expect (count.v2 'world 'hello) 0)
(check-expect (count.v2 '(world hello) 'hello) 1)
(check-expect (count.v2 '(((world) hello) hello) 'hello) 2)

;(define (count.v2 sexp sy) 0) ;stub

(define (count.v2 sexp sy)
  (cond [(number? sexp) 0]
        [(string? sexp) 0]
        [(symbol? sexp) (if (symbol=? sexp sy) 1 0)]
        [(empty? sexp) 0]
        [else
         (+  (count.v2 (first sexp) sy)    
             (count.v2 (rest sexp) sy))]))

; S-expr Symbol -> N
; counts all occurrences of sy in sexp
(check-expect (count.v3 'world 'hello) 0)
(check-expect (count.v3 '(world hello) 'hello) 1)
(check-expect (count.v3 '(((world) hello) hello) 'hello) 2)

(define (count.v3 sexp sy)
  (cond [(number? sexp) 0]
        [(string? sexp) 0]
        [(symbol? sexp) (if (symbol=? sexp sy) 1 0)]
        [else 
         (foldl (lambda (si c)
                  (+ (count.v3 si sy) c))
                0
                sexp)]))


