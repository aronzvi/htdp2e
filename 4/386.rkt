;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |386|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; A [Maybe X] is one of: 
; – #false 
; – X

; An Xexpr.v3 is one of:
;  – Symbol
;  – String
;  – Number
;  – (cons Symbol (cons Attribute*.v3 [List-of Xexpr.v3]))
;  – (cons Symbol [List-of Xexpr.v3])

(define a0 '(initial "X"))
(define a1 '(corona 3))

(define loa0 `(,a0))
(define loa1 `(,a0 ,a1))
 
(define e0 '(machine))
(define e1 `(machine ,loa0)) 
(define e2 '(machine (action)))
(define e3 '(machine () (action)))
(define e4 `(machine ,loa0 (action) (action)))

#;
(define (fn-for-xexpr.v3 x)
  (cond [(symbol? x) (... x)]
        [(string? x) (... x)]
        [(number? x) (... x)]
        [else
         (local ((define loa (xexpr-attr x))
                 (define content (xexpr-content x)))
           (if (empty? loa)
               (... (first x)                   ;Symbol
                    (fn-for-lox content))       ;[List-of Xexpr.v3]
               (... (first x)                   ;Symbol
                    (fn-for-loa loa)            ;Attribute*.v3
                    (fn-for-lox content))))]))  ;[List-of Xexpr.v3]

(define XE0 's)
(define XE1 "F")
(define XE2 5)
(define XE3 (cons 's (cons (list (list 'a1 "ssss") (list 'a2 "ffff")) (list 's "ff"))))
(define XE4 (cons 's (list 's "ff")))

; An Attribute.v3 is a list of two items:
;   (list Symbol String)
(define A0 (list 'a "val"))
(define A1 (list 'b "1"))

; An Attribute*.v3 is a [List-of Attribute.v3].
(define LOA0 (list A0 A1))

; Xexpr.v3 String -> String
; retrieves the value of the "content" attribute 
; from a 'meta element that has attribute "itemprop"
; with value s
(check-expect
 (get '(meta ((content "+1") (itemprop "F"))) "F")
 "+1")
(check-expect
 (get '(meta ((content "+4") (itemprop "TSL"))) "TSL")
 "+4")
(check-error
 (get '(meta ((content "+4") (itemprop "G"))) "TSL")
 "not found")
 
(define (get x s)
  (local ((define result (get-xexpr x s)))
    (if (string? result)
        result
        (error "not found"))))

; Xexpr.v3 String -> [Maybe String]
; retrieves the value of the "content" attribute 
; from a 'meta element that has attribute "itemprop"
; with value s. Produces false if not found
(check-expect (get-xexpr 's "F") false)
(check-expect (get-xexpr "ffff" "F") false)
(check-expect (get-xexpr 3 "F") false)
(check-expect (get-xexpr '(meta (1 's "4")) "F") false)
(check-expect
 (get-xexpr '(boba ((content "+4") (itemprop "TSL"))) "TSL")
 false)
(check-expect
 (get-xexpr '(meta ((content "+1") (itemprop "F"))) "F")
 "+1")
(check-expect
 (get-xexpr '(meta ((content "+1") (itemprop "F"))(blah)) "F")
 "+1")
(check-expect
 (get-xexpr '(meta ((content "+4") (itemprop "TSL"))) "TSL")
 "+4")
(check-expect
 (get-xexpr '(meta ((content "+4") (itempeep "TSL"))) "TSL")
 false)
(check-expect
 (get-xexpr '(meta ((cookoo "+4") (itemprop "TSL"))) "TSL")
 false)
(check-expect
 (get-xexpr '(meta ((content "+4") (itemprop "G"))) "TSL")
 false)
(check-expect
 (get-xexpr (read-xexpr "teststock.html") "price")
 "15.24")
(check-expect
 (get-xexpr (read-xexpr "teststock.html") "priceChange")
 "+0.16")

;(define (get-xexpr xe s) false) ;stub

(define (get-xexpr x s)
  (cond [(symbol? x) false]
        [(string? x) false]
        [(number? x) false]
        [else
         (local ((define loa (xexpr-attr x))
                 (define content (xexpr-content x)))
           (if (empty? loa)
               (get-lox content s)      ;[List-of Xexpr.v3]
               (if (and (symbol=? (xexpr-name x) 'meta)                 
                        (string? (find-attr loa 'itemprop))
                        (string=? (find-attr loa 'itemprop) s)
                        (string? (find-attr loa 'content)))
                   (find-attr loa 'content)
                   (get-lox content s))))]))

;[List-of Xexpr.v3] String -> [Maybe String]
; retrieves the value of the "content" attribute 
; from a 'meta element that has attribute "itemprop"
; with value s. Produces false if not found
(check-expect (get-lox empty "F") false)
(check-expect (get-lox (list '(meta ((content "+1") (itemprop "F")))) "F")
              "+1")
(check-expect (get-lox (list  '(meta ((cookoo "+4") (itemprop "TSL")))
                              '(meta ((content "+4") (itempeep "TSL"))) 
                              '(meta ((content "+1") (itemprop "F"))))
                       "F")
              "+1")
(check-expect (get-lox (list  '(meta ((cookoo "+4") (itemprop "TSL")))
                              '(meta ((content "+4") (itempeep "TSL"))) 
                              '(meta ((content "+1") (itemprop "F"))))
                       "X")
              false)

;(define (get-lox lox s) false) ;stub

(define (get-lox lox s)
  (cond [(empty? lox) false]
        [else
         (local ((define get-x (get-xexpr (first lox) s)))
         (if (string? get-x)
             get-x
             (get-lox (rest lox) s)))]))

; Xexpr.v2 -> [List-of Attribute]
; retrieves the list of attributes of xe
(check-expect (xexpr-attr e0) '())
(check-expect (xexpr-attr e1) '((initial "X")))
(check-expect (xexpr-attr e2) '())
(check-expect (xexpr-attr e3) '())
(check-expect (xexpr-attr e4) '((initial "X")))

;(define (xexpr-attr xe) '()) ;stub

(define (xexpr-attr xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else
       (local ((define loa-or-x
                 (first optional-loa+content)))
         (if (list-of-attributes? loa-or-x)
             loa-or-x
             '()))])))

; [List-of Attribute] or Xexpr.v3 -> Boolean
; is x a list of attributes
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [(symbol? x) false]
    [(string? x) false]
    [(number? x) false]
    [else
     (local ((define possible-attribute (first x)))
       (cons? possible-attribute))]))

; Xexpr.v2 -> [List-of Xexpr.v2]
; retrieves the content of xe
(check-expect (xexpr-content e0) empty)
(check-expect (xexpr-content e1) empty)
(check-expect (xexpr-content e2) '((action)))
(check-expect (xexpr-content e3) '((action)))
(check-expect (xexpr-content e4) '((action) (action)))

;(define (xexpr-content xe) empty) ;stub

(define (xexpr-content xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) empty]
      [else
       (if (list-of-attributes? (first optional-loa+content))
           (rest optional-loa+content)
           optional-loa+content)])))

; [List-of Attribute] Symbol -> String or false
(check-expect (find-attr empty 'dood) false)
(check-expect (find-attr loa1 'initial) "X")
(check-expect (find-attr loa1 'corona) false)
(check-expect (find-attr loa1 'dad) false)

;(define (find-attr loa s) false) ;stub

(define (find-attr loa s)
  (local ((define found-attrib (assq s loa)))
    (if (and (cons? found-attrib) (string? (second found-attrib)))
        (second found-attrib)
        false)))

; Xexpr.v2 -> Symbol
; retrieves the name of xe
(check-expect (xexpr-name e0) 'machine)
(check-expect (xexpr-name e1) 'machine)
(check-expect (xexpr-name e2) 'machine)
(check-expect (xexpr-name e3) 'machine)
(check-expect (xexpr-name e4) 'machine)

;(define (xexpr-name xe) 'x) ;stub

; Template from??

(define (xexpr-name xe)
  (first xe))