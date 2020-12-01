;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |376|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Data definitions

; An Xexpr.v2 is a list: 
; – (cons Symbol Body)
; – (cons Symbol (cons [List-of Attribute] Body))
; where Body is short for [List-of Xexpr.v2]
; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

(define a0 '(initial "X"))
(define a1 '(corona 3))

(define loa0 `(,a0))
(define loa1 `(,a0 ,a1))
 
(define e0 '(machine))
(define e1 `(machine ,loa0)) 
(define e2 '(machine (action)))
(define e3 '(machine () (action)))
(define e4 `(machine ,loa0 (action) (action)))

; An XWord is '(word ((text String)))

(define XW0 '(word ((text "test1"))))
(define XW1 '(word ((text "test2"))))
(define XW2 '(word ((text "test3"))))

; An XItem.v2 is one of: 
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons [List-of Attribute] (list XWord)))
; – (cons 'li (cons XEnum.v2 '()))
; – (cons 'li (cons [List-of Attribute] (list XEnum.v2)))
; 
; An XEnum.v2 is one of:
; – (cons 'ul [List-of XItem.v2])
; – (cons 'ul (cons [List-of Attribute] [List-of XItem.v2]))

(define ITEM0 (cons 'li (cons '(word ((text "test1"))) '())))                                          ; word
(define ITEM1 (cons 'li (cons (list '(initial "X") '(corona 3)) (cons '(word ((text "test1"))) '())))) ; attributes and word
(define ITEM2 (cons 'li (cons (cons 'ul (list (cons 'li (cons '(word ((text "booboo"))) '()))          ; enum    
                                              (cons 'li (cons '(word ((text "booc"))) '())))) '())))
(define ITEM3 (cons 'li (cons (list '(initial "X") '(corona 3))
                              (list (cons 'ul (list (cons 'li (cons '(word ((text "booboo"))) '()))          ; attributes and enum     
                                                    (cons 'li (cons '(word ((text "booc"))) '()))))))))

(define ENUM0 (cons 'ul (list (cons 'li (cons '(word ((text "r000"))) '()))          
                              (cons 'li (cons '(word ((text "cccc"))) '())))))
(define ENUM1 (cons 'ul (cons (list '(initial "X") '(corona 3))
                              (list (cons 'li (cons '(word ((text "xxx"))) '()))          
                                    (cons 'li (cons '(word ((text "xxzzz"))) '()))))))
(define ENUM2 (cons 'ul (cons (list '(initial "X") '(corona 3))
                              (list (cons 'li (cons (cons 'ul (list (cons 'li (cons '(word ((text "booboo"))) '()))             
                                                                    (cons 'li (cons '(word ((text "booc"))) '())))) '()))          
                                    (cons 'li (cons '(word ((text "xxzzz"))) '()))))))

#;
(define (fn-for-item xi)
  (local ((define content (first (xexpr-content xi))))
    (cond [(word? content) (... (fn-for-word content))] ;XWord
          [else
           (... (fn-for-enum content))]))) ;XEnum.v2 

#;
(define (fn-for-enum xe)
  (local ((define content (xexpr-content xe)))
    (... (fn-for-loi content))))   ;[List-of XItem.v2]

; Functions

; XEnum.v2 -> Number
; XItem.v2 -> Number
; counts all "hello"s in xe
(check-expect (count-hellos (cons 'ul (list (cons 'li (cons '(word ((text "r000"))) '()))          
                                            (cons 'li (cons '(word ((text "hello"))) '())))))
              1)
(check-expect (count-hellos (cons 'ul (list (cons 'li (cons '(word ((text "hello"))) '()))          
                                            (cons 'li (cons '(word ((text "hello"))) '())))))
              2)
(check-expect (count-hellos (cons 'ul (cons (list '(initial "X") '(corona 3))
                                            (list (cons 'li (cons (cons 'ul (list (cons 'li (cons '(word ((text "hello"))) '()))             
                                                                                  (cons 'li (cons '(word ((text "hello"))) '())))) '()))          
                                                  (cons 'li (cons '(word ((text "hello"))) '()))))))
              3)
(check-expect (count-hellos (cons 'ul (cons (list '(initial "X") '(corona 3))
                                            (list (cons 'li (cons (cons 'ul (list (cons 'li (cons '(word ((text "booboo"))) '()))             
                                                                                  (cons 'li (cons '(word ((text "ff"))) '())))) '()))          
                                                  (cons 'li (cons '(word ((text "nn"))) '()))))))
              0)
(check-expect (count-hellos-item (cons 'li (cons '(word ((text "hello"))) '())))
              1)
(check-expect (count-hellos-item (cons 'li (cons '(word ((text "test1"))) '())))
              0)
(check-expect (count-hellos-item (cons 'li (cons (list '(initial "X") '(corona 3))
                                                 (list (cons 'ul (list (cons 'li (cons '(word ((text "booboo"))) '()))            
                                                                       (cons 'li (cons '(word ((text "booc"))) '()))))))))
              (count-hellos (list (cons 'ul (list (cons 'li (cons '(word ((text "booboo"))) '()))          
                                                  (cons 'li (cons '(word ((text "booc"))) '())))))))

;(define (count-hellos xe) 0) ;stub

(define (count-hellos xe)
  (local ((define content (xexpr-content xe))
          ; XItem.v2 Natural -> Natural
          (define (sum-item i so-far) (+ (count-hellos-item i) so-far)))
    (foldr sum-item 0 content)))  

(define (count-hellos-item xi)
  (local ((define content (first (xexpr-content xi))))
    (cond [(word? content)
           (if (string=? "hello" (word-text content)) 1 0)]
          [else
           (count-hellos content)])))

; XWord -> String
; extracts the value of the only attribute of xw
(check-expect (word-text XW0) "test1")
(check-expect (word-text XW1) "test2")

;(define (word-text xw) "") ;stub

(define (word-text xw)
  (find-attr (second xw) 'text))

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

; [List-of Attribute] or Xexpr.v2 -> ???
; determines whether x is an element of [List-of Attribute]
; #false otherwise
;(define (list-of-attributes? x) #false) ;stub

; [List-of Attribute] or Xexpr.v2 -> Boolean
; is x a list of attributes
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else
     (local ((define possible-attribute (first x)))
       (cons? possible-attribute))]))

; ??? -> Boolean
; is x an  XWord
(check-expect (word? XW0) true)
(check-expect (word? 1) false)
(check-expect (word? "d") false)
(check-expect (word? 'd) false)
(check-expect (word? '(wrd)) false)
(check-expect (word? '(wrd ((text "test1")))) false)
(check-expect (word? '(word ((txt "test1")))) false)
(check-expect (word? '(word ((text 1)))) false)
(check-expect (word? '(word ((text dd)))) false)

;(define (word? x) false) ;stub

(define (word? x)
  (and (cons? x)
       (= 2 (length x))
       (local ((define word-tag (first x)))
         (and (symbol? word-tag)
              (symbol=? word-tag 'word)))
       (local ((define attrib-list (first (rest x))))
         (and (cons? attrib-list)
              (= 1 (length attrib-list))))
       (local ((define attrib (first (first (rest x)))))
         (and (cons? attrib)
              (= 2 (length attrib))
              (symbol=? (first attrib) 'text)
              (string? (second attrib))))))

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
