;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |411|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define presence-schema2
  `(("Present"     ,boolean?)
    ("Description" ,string?)
    ("Black"   ,boolean?)
    ("Number" ,number?)))
 
(define school-content
  `(("Alice" 35 #true)
    ("Bob"   25 #false)
    ("Carol" 30 #true)
    ("Dave"  32 #false)))
 
(define presence-content
  `((#true  "presence")
    (#false "absence")))

(define presence-content2
  `((#true  "presence" #false 123)
    (#false "absence" #true 666)))

(define presence-content3
  `((#true  "presence")
    (#true "here")
    (#false "absence")
    (#false "there")))
 
(define school-db
  (make-db school-schema
           school-content))
 
(define presence-db
  (make-db presence-schema
           presence-content))

(define presence-db2
  (make-db presence-schema2
           presence-content2))

(define presence-db3
  (make-db presence-schema
           presence-content3))

 
; DB DB -> DB
;the schema of db-2 starts with the exact same Spec that the schema of db-1 ends in.
;creates a db from db-1 by replacing the last cell in each row with the translation of the cell in db-2
;assumes that a “translation” finds only one row per cell
(check-expect (db-content (join.v1 school-db presence-db)) ;“translate” a cell to a single value
              `(("Alice" 35 "presence")
                ("Bob"   25 "absence")
                ("Carol" 30 "presence")
                ("Dave"  32 "absence")))
(check-expect (db-content (join.v1 school-db presence-db2))  ;“translate” a cell to a row of values
              `(("Alice" 35 "presence" #false 123)
                ("Bob"   25 "absence" #true 666)
                ("Carol" 30 "presence" #false 123)
                ("Dave"  32 "absence" #true 666)))

;(define (join.v1 db-1 db-2) (make-db empty empty)) ;stub

; both dbs have schema and content. Do not need to check 
#;
(define (join.v1 db-1 db-2)
  (... (db-schema db-1)
       (db-schema db-2)
       (db-content db-1)
       (db-content db-2)))

#;
(define (join.v1 db-1 db-2)
  (local (;Schema Schema -> Schema
          ; replace the last spec in sch1 with (rest of sch2)
          (define (join-schema sch1 sch2) empty)

          ;Content Content -> Content
          ; replace the last cell in each row of c1 with the translation from the first cell of the row in c2
          (define (join-content c1 c2) empty))
    
    (make-db (join-schema (db-schema db-1) (db-schema db-2))
             (join-content (db-content db-1) (db-content db-2)))))

;removing passing around c2. I guess treating like atomic and will not change. not sure about this
#;
(define (join.v1 db-1 db-2)
  (local ((define content-1 (db-content db-1))
          (define content-2 (db-content db-2))
          
          ;Schema Schema -> Schema
          ; replace the last spec in sch1 with (rest of sch2)
          (define (join-schema sch1 sch2) empty)

          ;Row -> Row
          ; replace the last cell of row with the translation from the first cell of the row in content-2
          (define (join-row r) empty)
          
          ;Content -> Content
          ; replace the last cell in each row of c with the translation from the first cell of the row in content-2
          (define (join-content c) (map join-row c)))
    
    (make-db (join-schema (db-schema db-1) (db-schema db-2))
             (join-content content-1))))


; not really following a recipe with join-row
; all-but-last with the double reverseing is ugly
#;
(define (join.v1 db-1 db-2)
  (local ((define content-1 (db-content db-1))
          (define content-2 (db-content db-2))
          
          ;Schema Schema -> Schema
          ; replace the last spec in sch1 with (rest of sch2)
          (define (join-schema sch1 sch2) empty)

          ;Row -> Row
          ; replace the last cell of row with the translation from the first cell of the row in content-2
          (define (join-row r)
            (local ((define last-val (first (reverse r)))
                    (define all-but-last (reverse (rest (reverse r)))))
              
              (append all-but-last (get-translation last-val))))

          ; Cell -> [List-of Cell]
          ; gets the translation for cell from content-2
          (define (get-translation c) empty)
          
          ;Content -> Content
          ; replace the last cell in each row of c with the translation from the first cell of the row in content-2
          (define (join-content c) (map join-row c)))
    (make-db (join-schema (db-schema db-1) (db-schema db-2))
             (join-content content-1))))

; you do not pass around content-2 and suddenly pass it to get-translation since it needs to traverse it recursivley
; can we optimise get-translation so that it doesn't have to search all of content for each row? I don't think so
(define (join.v1 db-1 db-2)
  (local ((define content-1 (db-content db-1))
          (define content-2 (db-content db-2))
          
          ;Schema Schema -> Schema
          ; replace the last spec in sch1 with (rest of sch2)
          (define (join-schema sch1 sch2)
            (local ((define all-but-last-s1 (reverse (rest (reverse sch1))))
                    (define all-but-first-s2 (rest sch2)))
              (append all-but-last-s1 all-but-first-s2)))

          ;Row -> Row
          ; replace the last cell of row with the translation from the first cell of the row in content-2
          (define (join-row r)
            (local ((define last-val (first (reverse r)))
                    (define all-but-last (reverse (rest (reverse r)))))
              
              (append all-but-last (get-translation last-val content-2))))

          ; Cell Content -> Row
          ; gets the translation for cell from c2
          (define (get-translation c content)
            (cond [(empty? content) (error "error")]
                  [else
                   (local ((define row (first content)))
                     (if (equal? (first row) c)
                         (rest row)
                         (get-translation c (rest content))))])) 
          
          ;Content -> Content
          ; replace the last cell in each row of c with the translation from the first cell of the row in content-2
          (define (join-content c) (map join-row c)))
    
    (make-db (join-schema (db-schema db-1) (db-schema db-2))
             (join-content content-1))))

; DB DB -> DB
;the schema of db-2 starts with the exact same Spec that the schema of db-1 ends in.
;creates a db from db-1 by replacing the last cell in each row with the translation of the cell in db-2.
;a “translation” can find more than one row per cell
(check-expect (db-content (join.v2 school-db presence-db)) ;“translate” a cell to a single value
              `(("Alice" 35 "presence")
                ("Bob"   25 "absence")
                ("Carol" 30 "presence")
                ("Dave"  32 "absence")))
(check-expect (db-content (join.v2 school-db presence-db2))  ;“translate” a cell to a row of values
              `(("Alice" 35 "presence" #false 123)
                ("Bob"   25 "absence" #true 666)
                ("Carol" 30 "presence" #false 123)
                ("Dave"  32 "absence" #true 666)))
(check-expect (db-content (join.v2 school-db presence-db3)) ;“translate” a cell to several rows
              `(("Alice" 35 "presence")
                ("Alice" 35 "here")
                ("Bob"   25 "absence")
                ("Bob"   25 "there")
                ("Carol" 30 "presence")
                ("Carol" 30 "here")
                ("Dave"  32 "absence")
                ("Dave"  32 "there")))

;(define (join.v2 db-1 db-2) (make-db empty empty)) ;stub  

#;
(define (join.v2 db-1 db-2)
  (local ((define content-1 (db-content db-1))
          (define content-2 (db-content db-2))
          
          ;Schema Schema -> Schema
          ; replace the last spec in sch1 with (rest of sch2)
          (define (join-schema sch1 sch2)
            (local ((define all-but-last-s1 (reverse (rest (reverse sch1))))
                    (define all-but-first-s2 (rest sch2)))
              (append all-but-last-s1 all-but-first-s2)))

          ; Row -> [List-of Row]
          ; replace the last cell of row with the translation from the first cell of the row in content-2
          ; multiple traslations are possible
          (define (join-row r) empty)
          
          ; Content -> Content
          ; replace the last cell in each row of c with the translation from the first cell of the row in content-2
          ; a translation can find more than one row per cell
          (define (join-content c)
            (cond [(empty? c) empty]
                  [else
                   (... (join-row (first c))     ;Row
                        (join-content (rest c)))]))) ;Content 
    
    (make-db (join-schema (db-schema db-1) (db-schema db-2))
             (join-content content-1))))

#;
(define (join.v2 db-1 db-2)
  (local ((define content-1 (db-content db-1))
          (define content-2 (db-content db-2))
          
          ;Schema Schema -> Schema
          ; replace the last spec in sch1 with (rest of sch2)
          (define (join-schema sch1 sch2)
            (local ((define all-but-last-s1 (reverse (rest (reverse sch1))))
                    (define all-but-first-s2 (rest sch2)))
              (append all-but-last-s1 all-but-first-s2)))

          ; Row -> [List-of Row]
          ; replace the last cell of row with the translation from the first cell of the row in content-2
          ; multiple traslations are possible
          (define (join-row r) empty)
          
          ; Content -> Content
          ; replace the last cell in each row of c with the translation from the first cell of the row in content-2
          ; a translation can find more than one row per cell
          (define (join-content c)
            (cond [(empty? c) empty]
                  [else
                   (append (join-row (first c))     
                           (join-content (rest c)))])))
    
    (make-db (join-schema (db-schema db-1) (db-schema db-2))
             (join-content content-1))))
#;
(define (join.v2 db-1 db-2)
  (local ((define content-1 (db-content db-1))
          (define content-2 (db-content db-2))
          
          ;Schema Schema -> Schema
          ; replace the last spec in sch1 with (rest of sch2)
          (define (join-schema sch1 sch2)
            (local ((define all-but-last-s1 (reverse (rest (reverse sch1))))
                    (define all-but-first-s2 (rest sch2)))
              (append all-but-last-s1 all-but-first-s2)))

          ; Row -> [List-of Row]
          ; replace the last cell of row with the translation from the first cell of the row in content-2
          ; multiple traslations are possible
          (define (join-row r)
            (local ((define last-val (first (reverse r)))
                    (define c2-translation-rows (get-translation-rows last-val)))
              (map (lambda (r2) (traslate-row r r2)) c2-translation-rows)))

          ; Row -> Row
          ; translates the row     
          (define (traslate-row r1 r2)
            (local ((define all-but-last-r1 (reverse (rest (reverse r1)))))
              (append all-but-last-r1 (rest r2))))

          ; Cell -> Content
          ; gets all relevant rows for translation from content-2
          (define (get-translation-rows c) (filter (lambda (r) (equal? (first r) c)) content-2))
          
          ; Content -> Content
          ; replace the last cell in each row of c with the translation from the first cell of the row in content-2
          ; a translation can find more than one row per cell
          (define (join-content c)
            (cond [(empty? c) empty]
                  [else
                   (append (join-row (first c))     
                           (join-content (rest c)))])))
    
    (make-db (join-schema (db-schema db-1) (db-schema db-2))
             (join-content content-1))))

(define (join.v2 db-1 db-2)
  (local ((define content-1 (db-content db-1))
          (define content-2 (db-content db-2))
          
          ;Schema Schema -> Schema
          ; replace the last spec in sch1 with (rest of sch2)
          (define (join-schema sch1 sch2)
            (local ((define all-but-last-s1 (reverse (rest (reverse sch1))))
                    (define all-but-first-s2 (rest sch2)))
              (append all-but-last-s1 all-but-first-s2)))

          ; Row -> [List-of Row]
          ; replace the last cell of row with the translation from the first cell of the row in content-2
          ; multiple traslations are possible
          (define (join-row r)
            (local ((define last-val (first (reverse r)))
                    (define c2-translation-rows (get-translation-rows last-val)))
              (map (lambda (r2) (traslate-row r r2)) c2-translation-rows)))

          ; Row -> Row
          ; translates the row     
          (define (traslate-row r1 r2)
            (local ((define all-but-last-r1 (reverse (rest (reverse r1)))))
              (append all-but-last-r1 (rest r2))))

          ; Cell -> Content
          ; gets all relevant rows for translation from content-2
          (define (get-translation-rows c) (filter (lambda (r) (equal? (first r) c)) content-2))
          
          ; Content -> Content
          ; replace the last cell in each row of c with the translation from the first cell of the row in content-2
          ; a translation can find more than one row per cell
          (define (join-content c)
            (foldr (lambda (r rows) (append (join-row r) rows)) empty c)))
    
    (make-db (join-schema (db-schema db-1) (db-schema db-2))
             (join-content content-1))))