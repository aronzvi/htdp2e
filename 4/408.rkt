;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |408|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct db [schema content])
; A DB is a structure: (make-db Schema Content)
 
; A Schema is a [List-of Spec]
; A Spec is a [List Label Predicate]
; A Label is a String
; A Predicate is a [Any -> Boolean]
 
; A (piece of) Content is a [List-of Row]
; A Row is a [List-of Cell]
; A Cell is Any
; constraint cells do not contain functions 
 
; integrity constraint In (make-db sch con), 
; for every row in con,
; (I1) its length is the same as sch's, and
; (I2) its ith Cell satisfies the ith Predicate in sch

(define school-schema
  `(("Name"    ,string?)
    ("Age"     ,integer?)
    ("Present" ,boolean?)))

(define school-content
  `(("Alice" 35 #true)
    ("Bob"   25 #false)
    ("Carol" 30 #true)
    ("Dave"  32 #false)))

(define school-db
  (make-db school-schema
           school-content))

(define projected-content
  `(("Alice" #true)
    ("Bob"   #false)
    ("Carol" #true)
    ("Dave"  #false)))
 
(define projected-schema
  `(("Name" ,string?) ("Present" ,boolean?)))
 
(define projected-db
  (make-db projected-schema projected-content))

; DB [List-of Label] [Row -> Boolean] -> Content
; produces the list of rows that satisfy p, projected down to the given set of labels
; same as select name, price from t where ... gets rows that satisfy condition and then keep only given cols
(check-expect (select school-db '("Name") (lambda (r) (> (second r) 30)))  ; select name where age > 30
              '(("Alice")
                ("Dave")))
(check-expect (select school-db '("Name" "Age") (lambda (r) (third r)))  ; select name, age where Present=true
              '(("Alice" 35)
                ("Carol" 30)))
(check-expect (select school-db '("Name") (lambda (r) (> (second r) 30)))  ; select name where age > 30
              '(("Alice")
                ("Dave")))
(check-expect (select school-db '("Present") (lambda (r) (< (second r) 60)))  ; select present where age < 60
              '((#true)
                (#false)
                (#true)
                (#false)))

;(define (select db names p) empty) ;stub

#;
(define (select db names p)
  (... names
       p
       (db-schema db)    ;Schema
       (db-content db))) ;Content

(define (select db names p)
  (db-content (project (make-db (db-schema db)
                                (filter p (db-content db)))
                       names)))

; DB [List-of Label] -> DB
; retains a column from db if its label is in labels
(check-expect (db-content (project school-db '("Name" "Present")))
              projected-content)

;(define (project db labels) (make-db '() '()))

(define (project db labels)
  (local ((define schema  (db-schema db))
          (define content (db-content db))
 
          ; Spec -> Boolean
          ; does this column belong to the new schema
          (define (keep? c)
            (member? (first c) labels))
 
          ; Row -> Row 
          ; retains those columns whose name is in labels
          (define (row-project row)
            (foldr (lambda (cell m c) (if m (cons cell c) c))
                   '()
                   row
                   mask))
          (define mask (map keep? schema)))
    (make-db (filter keep? schema)
             (map row-project content))))
