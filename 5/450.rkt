;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |450|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define ε 0.1)

; [Number -> Number] Number Number -> Number
; determines R such that f has a root in [R,(+ R ε)]
; assume f is continuous
; assume f is monotonically increasing - if (< a b) then (<= (f a) (f b))
; assume (<= (f left) 0 (f right))
; generative divides interval in half, the root is in one of the two
; halves, picks according to assumption
(check-satisfied (find-root-mono-inc-no-recalc mono 5 15) (lambda (n) (<= n 10 (+ n ε))))

#;
(define (find-root-mono-inc f left right)
  (cond
    [(<= (- right left) ε) left]
    [else
     (local ((define mid (/ (+ left right) 2))
             (define f@mid (f mid))
             (define f@left (f left))
             (define f@right (f right)))
       (cond
         [(<= f@left 0 f@mid)    ;(<= f@mid 0 f@left) cannot happen in monotonically increasing function since f(mid) always >= f(left) so we don't need to check for it
          (find-root f left mid)]
         [(<= f@mid 0 f@right)   ;(<= f@right 0 f@mid) also cannot happen since f(right) always >= f(mid)
          (find-root f mid right)]))]))

(define (find-root-mono-inc-no-recalc f left right)
  (local ((define (find-root-with-f left right f@left f@right)
            (cond
              [(<= (- right left) ε) left]
              [else
               (local ((define mid (/ (+ left right) 2))
                       (define f@mid (f mid)))
                 (cond
                   [(<= f@left 0 f@mid)                         ;(<= f@mid 0 f@left) cannot happen in monotonically increasing function since f(mid) always >= f(left) so we don't need to check for it
                    (find-root-with-f left mid f@left f@mid)]
                   [(<= f@mid 0 f@right)                        ;(<= f@right 0 f@mid) also cannot happen since f(right) always >= f(mid)
                    (find-root-with-f mid right f@mid f@right)]))])))
    (find-root-with-f left right (f left) (f right))))

; Number -> Number
; mono in monotonically increasing has one root
; (mono 10) = 0

(define (mono x)
  (- x 10))