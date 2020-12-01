;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |261|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
	
(define-struct ir [name price])
; An IR is a structure:
;   (make-ir String Number)

; Inventory is listof IR

; Inventory -> Inventory
; creates an Inventory from an-inv for all
; those items that cost less than a dollar
(check-expect (extract1-local (list (make-ir "aaa" 1.0)
                                    (make-ir "bbb" 2.0)
                                    (make-ir "ccc" 0.5)
                                    (make-ir "ddd" 0.6)
                                    (make-ir "eee" 5.0)
                                    (make-ir "fff" 7.0)
                                    (make-ir "ggg" 0.3)
                                    (make-ir "hhh" 0.3)))
              (list (make-ir "aaa" 1.0)
                    (make-ir "ccc" 0.5)
                    (make-ir "ddd" 0.6)
                    (make-ir "ggg" 0.3)
                    (make-ir "hhh" 0.3)))

(define (extract1-local an-inv)
  (cond
    [(empty? an-inv) '()]
    [else
     (local ((define less-than-1.0 (extract1 (rest an-inv))))
       (cond
         [(<= (ir-price (first an-inv)) 1.0)
          (cons (first an-inv) less-than-1.0)]
         [else less-than-1.0]))]))

; Inventory -> Inventory
; creates an Inventory from an-inv for all
; those items that cost less than a dollar
(define (extract1 an-inv)
  (cond
    [(empty? an-inv) '()]
    [else
     (cond
       [(<= (ir-price (first an-inv)) 1.0)
        (cons (first an-inv) (extract1 (rest an-inv)))]
       [else (extract1 (rest an-inv))])]))

(time (extract1-local (list (make-ir "aaa" 1.0)
                            (make-ir "bbb" 2.0)
                            (make-ir "ccc" 0.5)
                            (make-ir "ddd" 0.6)
                            (make-ir "eee" 5.0)
                            (make-ir "fff" 7.0)
                            (make-ir "ggg" 0.3)
                            (make-ir "hhh" 0.3))))

(time (extract1 (list (make-ir "aaa" 1.0)
                      (make-ir "bbb" 2.0)
                      (make-ir "ccc" 0.5)
                      (make-ir "ddd" 0.6)
                      (make-ir "eee" 5.0)
                      (make-ir "fff" 7.0)
                      (make-ir "ggg" 0.3)
                      (make-ir "hhh" 0.3))))

; I think that the local does not affect the speed at all here since we only call extract1 once per call depending on the clause
