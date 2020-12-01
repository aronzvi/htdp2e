;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |343|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require htdp/dir)

#;
(define (fn-for-dir d)
  (local ((define (fn-for-dir d)
            (... (dir-name d)                    ;String
                 (fn-for-lod (dir-dirs d))       ;[List-of Dir] 
                 (fn-for-lof (dir-files d))))    ;[List-of File]

          (define (fn-for-lod lod)
            (cond [(empty? lod) (...)]
                  [else
                   (... (fn-for-dir (first lod))     ;Dir
                        (fn-for-lod (rest lod)))]))) ;[List-of Dir]
    (fn-for-dir d)))

#;
(define (fn-for-file f)
  (... (file-name f)      ;String
       (file-size f)      ;Integer
       (file-content f))) ;String

(define TEST-DIR (create-dir "testdir"))

; A Path is [List-of String].
; interpretation directions into a directory tree

(define P0 (list "TS" "Text" "part1"))
(define ALL-FILES-TEST-DIR (list (list "testdir" ".DS_Store")
                                 (list "testdir" "one" ".DS_Store")
                                 (list "testdir" "one" "1" ".DS_Store")
                                 (list "testdir" "one" "1" "f-1-1-1.txt")
                                 (list "testdir" "one" "1" "f-1-1-2.txt")
                                 (list "testdir" "one" "2" ".DS_Store")
                                 (list "testdir" "one" "2" "f-1-2-1.txt")
                                 (list "testdir" "one" "2" "f-1-2-2.txt")))

(check-expect (all-files-test-dir? ALL-FILES-TEST-DIR) true)
(check-expect (all-files-test-dir? (rest ALL-FILES-TEST-DIR)) false)
(check-expect (all-files-test-dir? (cons (list "blah blah") ALL-FILES-TEST-DIR)) false)
(check-expect (all-files-test-dir? (list
                                    (list "testdir" "one" "2" "f-1-2-1.txt")
                                    (list "testdir" ".DS_Store")
                                    (list "testdir" "one" ".DS_Store")
                                    (list "testdir" "one" "1" ".DS_Store")
                                    (list "testdir" "one" "1" "f-1-1-1.txt")
                                    (list "testdir" "one" "1" "f-1-1-2.txt")
                                    (list "testdir" "one" "2" ".DS_Store")
                                    (list "testdir" "one" "2" "f-1-2-2.txt")))
              true)
(define (all-files-test-dir? lop)
  (set=? ALL-FILES-TEST-DIR lop))

; Dir -> [List-of Path]
; produces list of paths to all files contained in a given directory tree (list of full paths for every file in the tree)
(check-satisfied (ls-R TEST-DIR) all-files-test-dir?)

;(define (ls-R d) empty) ;stub

(define (ls-R d)
  (local (; Dir -> [List-of Path]
          ; ???
          (define (fn-for-dir d)
            (local ((define files-with-dir (map (lambda (f) (list (dir-name d) (file-name f))) (dir-files d)))
                    (define subdir-files-with-dir (map (lambda (path) (cons (dir-name d) path)) (fn-for-lod (dir-dirs d)))))
              
              (append files-with-dir subdir-files-with-dir)))

          ; [List-of Dir] -> [List-of Path]
          ; ???
          (define (fn-for-lod lod)
            (cond [(empty? lod) empty]
                  [else
                   (append (fn-for-dir (first lod))     ;Dir
                           (fn-for-lod (rest lod)))]))) ;[List-of Dir]
    (fn-for-dir d)))

; [List-of X] [List-of X] -> Boolean
; determines lox1 and lox2 contain the same items—regardless of order
(check-expect (set=? empty empty) #true)
(check-expect (set=? '(1) empty) #false)
(check-expect (set=? empty '(1)) #false)
(check-expect (set=? '(1) '(1)) #true)
(check-expect (set=? '(1) '(2)) #false)
(check-expect (set=? '(1 2) '(2 1)) #true)
(check-expect (set=? '(1 2) '(2 3)) #false)
(check-expect (set=? '(1 2) '(1 2 4)) #false)
(check-expect (set=? '(1 2 5) '(1 2)) #false)
(check-expect (set=? '(1 2) '(1 2 2)) #false)

;(define (set=? lox1 lox2) false) ;stub

(define (set=? lox1 lox2)
  (local ((define (every-in? lox1 lox2)
            (andmap (lambda (x) (member? x lox2)) lox1)))
    (and (= (length lox1) (length lox2))
         (every-in? lox1 lox2)
         (every-in? lox2 lox1))))