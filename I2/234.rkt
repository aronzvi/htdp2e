;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |234|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/web-io)

;; Data definitions:

;; ListOfString is one of:
;; - '()
;; - (cons String ListOfString)
;; interp. a list of strings
(define LOS1 '())
(define LOS2 (cons "sff" (cons "ddd" '())))
(define one-list
  '("Asia: Heat of the Moment"
    "U2: One"
    "The White Stripes: Seven Nation Army"))

(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (first los)
              (fn-for-los (rest los)))])) ;ListOfString

;; Template rules used:
;; - one of: 2 cases
;; - atomic disitinct: '()
;; - compound: (cons String ListOfString)
;; - self-reference: (rest los) is ListOfString

;; Rank is (cons Natural (cons String '())
(define R1 (list 1 "Asia: Heat of the Moment"))
(define R2 (list 2 "U2: One"))

(define(fn-for-rank r)
  (... (first r)
       (first (rest r))))

;; template rules used:
;; - compound: 2 fields

;; ListOfRank is one of
;; - '()
;; - (cons Rank ListOfRank)
(define LOR1 '())
(define LOR2 (list (list 1 "Asia: Heat of the Moment") (list 2 "U2: One") (list 3 "The White Stripes: Seven Nation Army")))

(define (fn-for-lor lor)
  (cond [(empty? lor) (...)]
        [else
         (... (fn-for-rank (first lor))   ;Rank
              (fn-for-lor (rest lor)))])) ;ListOfRank

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: '()
;; - compound: (cons Rank ListOfRank)
;; - reference: (first lor) is Rank
;; - self-reference: (rest lor) is ListOfRank

;; An HtmlAttribute is (cons Symbol (cons String empty))
(define HA1 (cons 'align (cons "center" empty)))
(define HA2 (cons 'width (cons "200" empty)))

;; An NonEmptyHtmlAttributeList is one of:
;; - (cons HtmlAttribute empty)
;; - (cons HtmlAttribute NonEmptyHtmlAttributeList)
(define NEHAL1 (cons HA1 empty))
(define NEHAL2 (cons HA1 (cons HA2 empty)))

;; An HtmlCell is one of:
;; - (cons 'td (cons String empty))
;; - (cons 'td (cons NonEmptyHtmlAttributeList (cons String empty)))
(define HCC1 (cons 'td (cons "1" empty)))
(define HCC2 (cons 'td (cons NEHAL1 (cons "blah blah" empty))))

;; A NonEmptyHtmlCellList is one of:
;; - (cons HtmlCell empty)
;; - (cons HtmlCell NonEmptyHtmlCellList)
(define NEHCL1 (cons HCC1 empty))
(define NEHCL2 (cons HCC2 (cons HCC1 empty)))

;;An HtmlRow is one of:
;; - (cons 'tr (cons NonEmptyHtmlCellList empty))
;; - (cons 'tr (cons NonEmptyHtmlAttributeList (cons NonEmptyHtmlCellList empty)))

(define ROW1 (cons 'tr (cons NEHCL1 empty)))
(define ROW2 (cons 'tr (cons NEHAL2 (cons NEHCL2 empty))))

;; An NonEmptyHtmlLRowList is one of:
;; - (cons HtmlRow empty)
;; - (cons HtmlRow HtmlLRowList)

(define HRL1 (cons ROW1 empty))
(define HRL2 (cons ROW1 (cons ROW2 empty)))

;; An HtmlTable is one of:
;; - (cons 'table (cons NonEmptyHtmlLRowList empty))
;; - (cons 'table (cons NonEmptyHtmlAttributeList (cons NonEmptyHtmlLRowList empty)))

(define T1 (cons 'table (cons HRL1 empty)))
(define T2 (cons 'table (cons NEHAL1 (cons HRL1 empty))))

;; Functions:

;; ListOfString -> ... nested list ...
;; produces a list representation of an HTML table wth given list of (ranked) song titles
(check-expect (make-ranking empty)
              '(table ((border "1"))))
(check-expect (make-ranking one-list)
              '(table ((border "1"))
                      (tr ((width "200")) (td "1") (td ((align "center")) "Asia: Heat of the Moment"))
                      (tr ((width "200")) (td "2") (td ((align "center")) "U2: One"))
                      (tr ((width "200")) (td "3") (td ((align "center")) "The White Stripes: Seven Nation Army"))))
       
;(define (make-ranking los) empty) ;stub

(define (make-ranking los)
  (make-table (ranking los)))

;; ListOfRank -> ... nested list ...
;; produces a list representation of an HTML table wth given list of ranked song titles
(check-expect (make-table empty) '(table ((border "1"))))
(check-expect (make-table (list (list 1 "Asia: Heat of the Moment")
                                (list 2 "U2: One")
                                (list 3 "The White Stripes: Seven Nation Army")))
              '(table ((border "1"))
                      (tr ((width "200")) (td "1") (td ((align "center")) "Asia: Heat of the Moment"))
                      (tr ((width "200")) (td "2") (td ((align "center")) "U2: One"))
                      (tr ((width "200")) (td "3") (td ((align "center")) "The White Stripes: Seven Nation Army"))))

#;
(define (make-ranking-table lor) ;stub
  '(table ((border "1"))))

(define (make-table lor)
  `(table ((border "1"))
          ,@(make-ranking-rows lor)))

;; ListOfRank -> ... nested list ...
;; produces list of HTML list representation rows for given lor
(check-expect (make-ranking-rows empty) empty)
(check-expect (make-ranking-rows (list (list 1 "Asia: Heat of the Moment")
                                       (list 2 "U2: One")
                                       (list 3 "The White Stripes: Seven Nation Army")))
              '((tr ((width "200")) (td "1") (td ((align "center")) "Asia: Heat of the Moment"))
                (tr ((width "200")) (td "2") (td ((align "center")) "U2: One"))
                (tr ((width "200")) (td "3") (td ((align "center")) "The White Stripes: Seven Nation Army"))))

;(define (make-ranking-rows lor) empty)

(define (make-ranking-rows lor)
  (cond [(empty? lor) empty]
        [else
         (cons (make-rank-row (first lor))  
               (make-ranking-rows (rest lor)))]))

;; Rank -> HtmlRow
;; creates a row for an HTML table from l
(check-expect (make-rank-row (list 1 "Asia: Heat of the Moment"))
              '(tr ((width "200")) (td "1") (td ((align "center")) "Asia: Heat of the Moment")))

;(define (make-rank-row rank) empty) ;stub

(define (make-rank-row r)
  `(tr ((width "200"))
       ,(make-cell (first r))
       ,(make-cell-align-center (first (rest r)))))

;; Natural -> HtmlCell
;; creates a cell for an HTML table from a string
(check-expect (make-cell 1) '(td "1")) ;stub

;(define (make-cell s) empty) ;stub

#;
(define (make-cell n) ;template
  (... n)) 

;; template rules used:
;; - atomic distinct

(define (make-cell n)
  `(td ,(number->string n)))

;; String -> HtmlCell
;; creates a cell with center alignment for an HTML table from a string
(check-expect (make-cell-align-center  "Asia: Heat of the Moment")
              '(td ((align "center")) "Asia: Heat of the Moment"))

;(define (make-cell-align-center s) empty) ;stub

#;
(define (make-cell-align-center s) ;template
  (... s)) 

;; template rules used:
;; - atomic distinct

(define (make-cell-align-center s)
  `(td ((align "center")) ,s))

;; ListOfString -> ListOfRank
;; produces a list of ranks with count in ascending order
(define (ranking los)
  (reverse (add-ranks (reverse los))))

;; ListOfString -> ListOfRank
;; produces a list of ranks with count in descending order
(define (add-ranks los)
  (cond
    [(empty? los) '()]
    [else (cons (list (length los) (first los))
                (add-ranks (rest los)))]))

(show-in-browser (make-ranking one-list))


