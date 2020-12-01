;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |340|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require htdp/dir)

#;
(define (fn-for-dir d)
  (local ((define (fn-for-dir d)
            (... (dir-name d)                    ;Symbol
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

(define D0 (create-dir "/var/log"))
(define D1 (create-dir "/Users/aronzvi/projects"))
(define D2 (create-dir "/Users/aronzvi/Documents"))
(define D3 (create-dir "/Users/aronzvi/Documents/Carlitos_figures_2"))

; String Dir -> Boolean
; produces true if directory has a subdirectory directly beneath it with given name
(check-expect (find-dir? "fff" D3) false)
(check-expect (find-dir? "Carlitos_videos_Gri" D3) true)

;(define (find-dir? s d) false) ;stub

(define (find-dir?.v1 n d)
  (local (;[List-of Dir] -> Boolean
          (define (fn-for-lod lod)
            (cond [(empty? lod) false]
                  [else
                   (if  (dir-name (first lod) n)
                       true
                       (fn-for-lod (rest lod)))])))
    (fn-for-lod (dir-dirs d))))

(define (find-dir?.v2 n d)
  (local (;[List-of Dir] -> Boolean
          (define (fn-for-lod lod)
            (ormap (lambda (d) (dir-name d)) lod)))
    (fn-for-lod (dir-dirs d))))

(define (find-dir? s d)
  (ormap (lambda (d) (string=? (dir-name d) s)) (dir-dirs d)))

; String [List-of File] -> Boolean
; produces true if directory has a file directly beneath it with given name
(check-expect (find-file? "dddd" D3) false)
(check-expect (find-file? " Can close legs (collect) when leading her boleo from side step and back ocho.mov" D3) true)

;(define (find-file? s d) false) ;stub

(define (find-file? s d)
  (ormap (lambda (f) (string=? (file-name f) s)) (dir-files d)))

; Dir -> [List-of String]
; produces list of all names and files in d (just the first level. just like ls command)
(check-satisfied (ls D3)
                 (lambda (l)
                   (and
                    (andmap (lambda (s)               ; all items in the list are either a file or folder within the folder - There are no extra items in the list
                              (or (find-dir? s D3)
                                  (find-file? s D3)))
                            l)
                    (and (andmap (lambda (d) (member? (dir-name d) l)) (dir-dirs D3))  ; all (names of) files and folders of the folder are in the list - No missing items in the list
                         (andmap (lambda (f) (member? (file-name f) l)) (dir-files D3))))))                           

;(define (ls d) empty) ;stub

(define (ls d)
  (local (; Dir -> [List-of String]
          (define (fn-for-dir d)
            (append
             (fn-for-lod (dir-dirs d))       
             (all-file-names (dir-files d))))    

          ;[List-of File] -> [List-of String]
          (define (all-file-names lof)
            (map file-name lof))
          
          ; [List-of Dir] -> [List-of String]
          (define (fn-for-lod lod)
            (cond [(empty? lod) empty]
                  [else
                   (cons (dir-name (first lod))     
                   (fn-for-lod (rest lod)))])))
(fn-for-dir d)))

; Symbol -> String
; produces the last element in a file path of form /xxx/yyy/zzz
(check-expect (name-from-path '/xxx/yyy/zzz) "zzz")
(check-expect (name-from-path 'xxx) "xxx")

;(define (name-from-path s) s) ;stub

(define (name-from-path s)
  (local ((define char-list (string->list (symbol->string s)))

          ; [List-of Char] -> [List-of Char]
          ; produces chars in list after the last backslash
          (define (chars-from-last-backslash loc)
            (cond [(empty? loc) empty]
                  [else
                   (if (or (member? #\/ (rest loc)) (char=? (first loc)  #\/))
                       (chars-from-last-backslash (rest loc))
                       (cons (first loc) (chars-from-last-backslash (rest loc))))])))
    (list->string (chars-from-last-backslash char-list))))




