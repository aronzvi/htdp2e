;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |171|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

;; ListOfString is one of:
;; - empty
;; - (cons String ListOfString)
;; interp. a list of strings
(define LOS-POEM-LINES (cons "TTT"
                             (cons ""
                                   (cons "Put up in a place"
                                         (cons "where it's easy to see"
                                               (cons "the cryptic admonishment"
                                                     (cons "T.T.T."
                                                           (cons ""
                                                                 (cons "When you feel how depressingly"
                                                                       (cons "slowly you climb,"
                                                                             (cons "it's well to remember that"
                                                                                   (cons "Things Take Time."
                                                                                         (cons ""
                                                                                               (cons "Piet Hein"
                                                                                                     empty))))))))))))))

(define LOS-POEM-WORDS (cons "TTT"
                             (cons "Put"
                                   (cons "up"
                                         (cons "in"
                                               (cons "a"
                                                     (cons "place"
                                                           (cons "where"
                                                                 (cons "it's"
                                                                       (cons "easy"
                                                                             (cons "to"
                                                                                   (cons "see"
                                                                                         (cons "the"
                                                                                               (cons "cryptic"
                                                                                                     (cons "admonishment"
                                                                                                           (cons "T.T.T."
                                                                                                                 (cons "When"
                                                                                                                       (cons "you"
                                                                                                                             (cons "feel"
                                                                                                                                   (cons "how"
                                                                                                                                         (cons "depressingly"
                                                                                                                                               (cons "slowly"
                                                                                                                                                     (cons "you"
                                                                                                                                                           (cons "climb,"
                                                                                                                                                                 (cons "it's"
                                                                                                                                                                       (cons "well"
                                                                                                                                                                             (cons "to"
                                                                                                                                                                                   (cons "remember"
                                                                                                                                                                                         (cons "that"
                                                                                                                                                                                               (cons "Things"
                                                                                                                                                                                                     (cons "Take"
                                                                                                                                                                                                           (cons "Time."
                                                                                                                                                                                                                 (cons "Piet"
                                                                                                                                                                                                                       (cons "Hein"
                                                                                                                                                                                                                             empty))))))))))))))))))))))))))))))))))
(define (fn-for-los los)
  (cond
    [(empty? los) (...)]
    [else
     (... (first los)                 ;String
          (fn-for-los (rest los)))])) ;ListOfString

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons String ListOfString)
;; - self-reference: (rest los) is ListOfString

(check-expect (read-lines "ttt.txt") LOS-POEM-LINES)
(check-expect (read-words "ttt.txt") LOS-POEM-WORDS)

;; ListOfListOfString is one of:
;; - empty
;; (cons ListOfString ListOfListOfString)
;; interp. a list of ListOfString
(define LOLOS-POEM (cons (cons "TTT" empty)
                         (cons empty
                               (cons (cons "Put"
                                           (cons "up"
                                                 (cons "in"
                                                       (cons "a"
                                                             (cons "place"
                                                                   empty)))))
                                     (cons (cons "where"
                                                 (cons "it's"
                                                       (cons "easy"
                                                             (cons "to"
                                                                   (cons "see"
                                                                         empty)))))
                                           (cons (cons "the"
                                                       (cons "cryptic"
                                                             (cons "admonishment"
                                                                   empty)))
                                                 (cons (cons "T.T.T." empty)
                                                       (cons empty
                                                             (cons (cons "When"
                                                                         (cons "you"
                                                                               (cons "feel"
                                                                                     (cons "how"
                                                                                           (cons "depressingly"
                                                                                                 empty)))))
                                                                   (cons (cons "slowly"
                                                                               (cons "you"
                                                                                     (cons "climb,"
                                                                                           empty)))
                                                                         (cons  (cons "it's"
                                                                                      (cons "well"
                                                                                            (cons "to"
                                                                                                  (cons "remember"
                                                                                                        (cons "that"
                                                                                                              empty)))))
                                                                                (cons (cons "Things"
                                                                                            (cons "Take"
                                                                                                  (cons "Time."
                                                                                                        empty)))
                                                                                      (cons empty
                                                                                            (cons (cons "Piet"
                                                                                                        (cons "Hein"
                                                                                                              empty))
                                                                                                  empty))))))))))))))

(define (fn-for-lolos lolos)
  (cond
    [(empty? lolos) (...)]
    [else
     (... (fn-for-los (first lolos))      ;ListOfString
          (fn-for-lolos (rest lolos)))])) ;ListOfListOfString

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons ListOfString ListOfListOfString)
;; - reference:  (first lolos) is ListOfString
;; - self-reference: (rest lolos) is ListOfListOfString

(check-expect (read-words/line "ttt.txt") LOLOS-POEM)