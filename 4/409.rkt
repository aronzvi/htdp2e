;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |409|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; DB [List-of Label] -> DB
; produces a database like db but with its columns reordered according to lol
; assumes that lol consists exactly of the labels of db’s columns
#;
(check-expect (reorder school-db '("Age" "Name" "Present"))
              (make-db  `(("Age"     ,integer?)
                          ("Name"    ,string?)
                          ("Present" ,boolean?))
                        `((35 "Alice" #true)
                          (25 "Bob"   #false)
                          (30 "Carol" #true)
                          (32 "Dave"  #false))))

#;
(check-expect (reorder school-db '("Present" "Age" "Name"))
              (make-db  `(("Present" ,boolean?)
                          ("Age"     ,integer?)
                          ("Name"    ,string?))
                        `((#true 35 "Alice")
                          (#false 25 "Bob")
                          (#true 30 "Carol")
                          (#false 32 "Dave"))))

(check-expect (db-content (reorder school-db '("Present" "Age" "Name")))
                        `((#true 35 "Alice")
                          (#false 25 "Bob")
                          (#true 30 "Carol")
                          (#false 32 "Dave")))
#;
(check-expect (db-schema (reorder school-db '("Present" "Age" "Name")))
                        `(("Present" ,boolean?)
                          ("Age"     ,integer?)
                          ("Name"    ,string?)))

;(define (reorder db lol) db) ;stub

#;
(define (reorder db lol)
  (... (db-schema db)
       (db-content db)))
#;
(define (reorder db lol)
  (local (; Schema -> Schema
          ; reorders s according to lol
          (define (reorder-schema s) s)

          ; Content -> Content
          ; reorders c according to lol
          (define (reorder-content c) c))
    (make-db (reorder-schema (db-schema db))
             (reorder-content (db-content db)))))

(define (reorder db lol)
  (local ((define schema (db-schema db))
          (define content (db-content db))

          ; String -> Number
          ; produces index of s in schema
          (define (get-index-from-schema s)
            (local ((define (get-index schema i)
                      (cond [(empty? schema) (error "error")]
                            [else
                             (if (string=? (first (first schema)) s)
                                 i
                             (get-index (rest schema) (add1 i)))])))
              (get-index schema 0)))

          (define reorder-index-map (map get-index-from-schema lol))

          ; [List-of X] -> [List-of X]
          ; reorders l according to reorder-index-map
          (define (reorder-with-index-map l) (map (lambda (i) (list-ref l i)) reorder-index-map))
          (define reordered-schema (reorder-with-index-map schema)) 
          (define reordered-content (map reorder-with-index-map content)))
    (make-db reordered-schema reordered-content)))