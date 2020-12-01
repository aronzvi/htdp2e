;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname integrity-check) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
 
(define presence-schema
  `(("Present"     ,boolean?)
    ("Description" ,string?)))
 
(define school-content
  `(("Alice" 35 #true)
    ("Bob"   25 #false)
    ("Carol" 30 #true)
    ("Dave"  32 #false)))

(define presence-content
  `((#true  "presence")
    (#false "absence")))
 
(define school-db
  (make-db school-schema
           school-content))

(define presence-db
  (make-db presence-schema
           presence-content))


; Functions

; DB -> Boolean
; do all rows in db satisfy (I1) and (I2)
(check-expect (integrity-check school-db) #true)
(check-expect (integrity-check presence-db) #true)
(check-expect (integrity-check (make-db school-schema
                                        `(("Alice" 35 #true)
                                          ("Bob"   "25" #false)
                                          ("Carol" 30 #true)
                                          ("Dave"  32 #false))))
              #false)
(check-expect (integrity-check (make-db school-schema
                                        `(("Alice" 35 #true)
                                          ("Bob"   25 #false)
                                          ("Carol" 30 #true 34)
                                          ("Dave"  32 #false))))
              #false)
 
;(define (integrity-check db) #false)

#;
(define (integrity-check db)
  (local (; Row -> Boolean
          (define (row-integrity-check row)
            ...))
    (andmap row-integrity-check (db-content db))))

#;
(define (integrity-check db)
  (local (; Row -> Boolean 
          ; does row satisfy (I1) and (I2) 
          (define (row-integrity-check row)
            #false))
    (andmap row-integrity-check (db-content db))))

#;
(define (integrity-check db)
  (local (
          ;Row -> Boolean
          ; !!!
          (define (length-of-row-check row) false)

          ;Row -> Boolean
          ;!!!
          (define (check-every-cell row) false)

          ; Row -> Boolean 
          ; does row satisfy (I1) and (I2)
          (define (row-integrity-check row)
            (and (length-of-row-check row)
                 (check-every-cell row))))
    (andmap row-integrity-check (db-content db))))

#;
(define (integrity-check db)
  (local (
          ;Row -> Boolean
          (define (length-of-row-check row) (= (length row) (length (db-schema db))))

          ;Row -> Boolean
          ;!!!
          (define (check-every-cell row) false)

          ; Row -> Boolean 
          ; does row satisfy (I1) and (I2)
          (define (row-integrity-check row)
            (and (length-of-row-check row)
                 (check-every-cell row))))
    (andmap row-integrity-check (db-content db))))

#;
(define (integrity-check db)
  (local (; Row -> Boolean 
          ; does row satisfy (I1) and (I2) 
          (define (row-integrity-check row)
            (and (= (length row)
                    (length (db-schema db)))
                 (andmap (lambda (s c) [(second s) c])
                         (db-schema db)
                         row))))
    (andmap row-integrity-check (db-content db))))

#;
(define (integrity-check db)
  (local ((define schema (db-schema db))
          ; Row -> Boolean 
          ; does row satisfy (I1) and (I2) 
          (define (row-integrity-check row)
            (and (= (length row)
                    (length schema))
                 (andmap (lambda (s c) [(second s) c])
                         schema
                         row))))
    (andmap row-integrity-check (db-content db))))

(define (integrity-check db)
  (local ((define schema (db-schema db))
          (define width (length schema))
          (define content (db-content db))
          ; Row -> Boolean 
          ; does row satisfy (I1) and (I2) 
          (define (row-integrity-check row)
            (and (= (length row)
                    width)
                 (andmap (lambda (s c) [(second s) c])
                         schema
                         row))))
    (andmap row-integrity-check content)))

