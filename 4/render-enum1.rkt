;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname render-enum1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; Constants:

(define BT (circle 3 "solid" "black"))

(define e0-rendered
  (above/align
   'left
   (beside/align 'center BT (text "one" 12 'black))
   (above/align
    'left
    (beside/align 'center BT (text "two" 12 'black))
    empty-image)))
  
; Data definitions:

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

; An XEnum.v1 is one of: 
; – (cons 'ul [List-of XItem.v1])
; – (cons 'ul (cons Attributes [List-of XItem.v1]))
; An XItem.v1 is one of:
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons Attributes (cons XWord '())))

(define en0
  '(ul
    (li (word ((text "one"))))
    (li (word ((text "two"))))))

; Functions:

; XEnum.v1 -> Image 
; renders a simple enumeration as an image 
(check-expect (render-enum1 en0) e0-rendered)

;(define (render-enum1 xe) empty-image) ;stub

#;
(define (render-enum1 xe)
  (... (xexpr-content xe) ...)) ; [List-of XItem.v1]

#;
(define (render-enum1 xe)
   (local ((define content (xexpr-content xe))
           ; XItem.v1 Image -> Image 
           (define (deal-with-one item so-far)
              ...))
     (foldr deal-with-one empty-image content)))

(define (render-enum1 xe)
  (local ((define content (xexpr-content xe))
          ; XItem.v1 Image -> Image 
          (define (deal-with-one item so-far)
            (above/align 'left
                          (render-item1 item)
                          so-far)))
    (foldr deal-with-one empty-image content)))

; XItem.v1 -> Image 
; renders an item as a "word" prefixed by a bullet
(check-expect (render-item1 '(li (word ((text "one")))))
              (beside/align 'center BT (text "one" 12 'black)))
(check-expect (render-item1 '(li (word ((text "two")))))
              (beside/align 'center BT (text "two" 12 'black)))

#;
(define (render-item1 i)
  (... (xexpr-content i) ...))

#;
(define (render-item1 i)
  (local ((define content (xexpr-content i))
          (define element (first content))
          (define a-word (word-text element)))
    (... a-word ...)))

(define (render-item1 i)
  (local ((define content (xexpr-content i))
          (define element (first content))
          (define a-word (word-text element))
          (define item (text a-word 12 'black)))
    (beside/align 'center BT item)))

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

; XWord -> String
; extracts the value of the only attribute of xw
(check-expect (word-text XW0) "test1")
(check-expect (word-text XW1) "test2")

;(define (word-text xw) "") ;stub

(define (word-text xw)
  (find-attr (second xw) 'text))

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
