;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |173|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

;; =======================
;;Constants:

(define ARTICLES (cons "a" (cons "an" (cons "the" empty))))


;; =======================
;; Data definitions:

;; ListOfString is one of:
;; - empty
;; - (cons String ListOfString)
;; interp. a list of strings
(define LINE0 empty)
(define LINE1 (cons "BOOO" empty))
(define LINE2 (cons "hello" (cons "world" empty)))
(define LINE3 (cons "my" (cons "name" (cons "is" (cons "Aron" empty)))))
(define LINE-ARTICLES1 (cons "a" (cons "booby" (cons "boob" empty))))
(define LINE-ARTICLES2 (cons "the" (cons "blob" (cons "is" (cons "a" (cons "booby" (cons "bob" (cons "and" (cons "an" (cons "eloglob" empty))))))))))
(define LINE-ARTICLES3 (cons "an" (cons "the" (cons "a" empty))))
(define LINE-ARTICLES4 (cons "the" (cons "hoe" (cons "and" (cons "an" (cons "are" (cons "a" empty )))))))

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons String ListOfString)
;; - self-reference: (rest los) is ListOfString

(define (fn-for-los los)
  (cond
    [(empty? los) (...)]
    [else
     (... (first los)                 ;String
          (fn-for-los (rest los)))])) ;ListOfString

;; LLS is one of:
;; - empty
;; (cons ListOfString LLS)
;; interp. a list of ListOfString
(define LLS0 empty)
(define LLS1 (cons LINE1 (cons LINE0 (cons LINE2 (cons LINE3 empty)))))
(define LLS-ARTICLES1 (cons LINE-ARTICLES1 (cons empty (cons LINE-ARTICLES2 (cons LINE-ARTICLES3 (cons LINE-ARTICLES4 empty))))))

(define (fn-for-lls lls)
  (cond
    [(empty? lls) (...)]
    [else
     (... (fn-for-los (first lls))    ;ListOfString
          (fn-for-lls (rest lls)))])) ;LLS

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons ListOfString ListOfListOfString)
;; - reference:  (first lolos) is ListOfString
;; - self-reference: (rest lolos) is LLS

;; =======================
;; Functions:

;; String -> String??? ;The output here is the new filename written. Not sure if this is proper
;; removes all articles from file n and writes results to file no-articles-n. Produces no-articles-n if successful
;; How to test this properly??? need to init test file?
(check-expect (remove-articles-from-file "test-articles.txt") "no-articles-test-articles.txt")

;(define (remove-articles-from-file n) "") ;stub

(define (remove-articles-from-file n)
  (write-file (string-append "no-articles-" n)
              (collapse (remove-articles (read-words/line n)))))

;; LLS -> LLS
;; removes "a", "an", and "the" words from given lines
(check-expect (remove-articles empty) empty)
(check-expect (remove-articles LLS-ARTICLES1) (cons (cons "booby" (cons "boob" empty))
                                                    (cons empty
                                                          (cons (cons "blob" (cons "is" (cons "booby" (cons "bob" (cons "and" (cons "eloglob" empty))))))
                                                                (cons empty
                                                                      (cons (cons "hoe" (cons "and" (cons "are" empty)))
                                                                            empty))))))

;(define (remove-articles lines) empty) ;stub

(define (remove-articles lls)
  (cond
    [(empty? lls) empty]
    [else
     (cons (remove-articles-from-words (first lls))    
           (remove-articles (rest lls)))]))

;; ListOfString -> ListOfString
;; removes "a", "an", and "the" words from given list of words
(check-expect (remove-articles-from-words empty) empty)
(check-expect (remove-articles-from-words (cons "a" empty)) empty)
(check-expect (remove-articles-from-words (cons "an" empty)) empty)
(check-expect (remove-articles-from-words (cons "the" empty)) empty)
(check-expect (remove-articles-from-words (cons "cooco" (cons "for" (cons "cookies" empty)))) (cons "cooco" (cons "for" (cons "cookies" empty))))
(check-expect (remove-articles-from-words LINE-ARTICLES1) (cons "booby" (cons "boob" empty)))
(check-expect (remove-articles-from-words LINE-ARTICLES2)
              (cons "blob" (cons "is" (cons "booby" (cons "bob" (cons "and" (cons "eloglob" empty)))))))
(check-expect (remove-articles-from-words LINE-ARTICLES4) (cons "hoe" (cons "and" (cons "are" empty))))
(check-expect (remove-articles-from-words LINE-ARTICLES3) empty)

;(define (remove-articles-from-words words) empty) ;stub

(define (remove-articles-from-words words)
  (cond
    [(empty? words) empty]
    [else
     (if (member? (first words) ARTICLES)
         (remove-articles-from-words (rest words))
         (cons (first words)
               (remove-articles-from-words (rest words))))])) 


;; LLS -> String
;; converts a list of lines into a string. The strings are separated " ". The lines are separated with "\n"
(check-expect (collapse empty) "")
(check-expect (collapse LLS1) "BOOO\n\nhello world\nmy name is Aron")

;(define (collapse lines) "") ;stub

(define (collapse lls)
  (cond
    [(empty? lls) ""]
    [else
     (if (= (length lls) 1)
         (string-append (collapse-line (first lls))     
                        (collapse (rest lls)))    
         (string-append (collapse-line (first lls)) "\n"    
                        (collapse (rest lls))))]))

;; ListOfString -> String
;; converts a list of words into a string. The strings are separated " "
(check-expect (collapse-line empty) "")
(check-expect (collapse-line LINE1) "BOOO")
(check-expect (collapse-line LINE2) "hello world")
(check-expect (collapse-line LINE3) "my name is Aron")

;(define (collapse-line words) "") ;stub

(define (collapse-line los)
  (cond
    [(empty? los) ""]
    [else
     (if (= (length los) 1)
         (string-append (first los)          
                        (collapse-line (rest los)))
         (string-append (first los) " "                 
                        (collapse-line (rest los))))]))

(remove-articles-from-file "ttt.txt")
