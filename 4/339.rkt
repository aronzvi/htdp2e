;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |339|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define (fn-for-file f)
  (... (file-name f)      ;String
       (file-size f)      ;Integer
       (file-content f))) ;String

(define D0 (create-dir "/var/log"))
(define D1 (create-dir "/Users/aronzvi/projects"))
(define D2 (create-dir "/Users/aronzvi/Documents"))

; Dir -> Boolean
; determines whether or not a file with name occurs in the directory tree
(check-expect (find? "dddx" D2) false)
(check-expect (find? "e39408f0cea97f358fe1ec95120d2c10.appinfo" D2) true)
(check-expect (find? "zxzx" D1) false)
(check-expect (find? "objc2_xrefs_helper.py" D1) true)

;(define (find? n d) false) ;stub

(define (find?.v1 n d)
  (local (; Dir -> Boolean
          (define (fn-for-dir d)
            (or 
             (fn-for-lod (dir-dirs d))       
             (find-in-files? (dir-files d)))) 

          ; [List-of File] -> Boolean
          (define (find-in-files? lof)
            (ormap (lambda (f) (string=? (file-name f) n)) lof))

          ; [List-of Dir] -> Boolean
          (define (fn-for-lod lod)
            (cond [(empty? lod) false]
                  [else
                   (or (fn-for-dir (first lod))     
                       (fn-for-lod (rest lod)))]))) 
    (fn-for-dir d)))

(define (find?.v2 n d)
  (local (; Dir -> Boolean
          (define (fn-for-dir d)
            (or 
             (fn-for-lod (dir-dirs d))       
             (find-in-files? (dir-files d)))) 

          ; [List-of File] -> Boolean
          (define (find-in-files? lof)
            (ormap (lambda (f) (string=? (file-name f) n)) lof))

          ; [List-of Dir] -> Boolean
          (define (fn-for-lod lod)
            (ormap fn-for-dir lod))) 
    (fn-for-dir d)))

(define (find?.v3 n d)
  (local (; Dir -> Boolean
          (define (fn-for-dir d)
            (or 
             (fn-for-lod (dir-dirs d))       
             (ormap (lambda (f) (string=? (file-name f) n)) (dir-files d)))) 

          ; [List-of Dir] -> Boolean
          (define (fn-for-lod lod)
            (ormap fn-for-dir lod))) 
    (fn-for-dir d)))

(define (find?.v4 n d)
  (local (; Dir -> Boolean
          (define (fn-for-dir d)
            (or 
             (ormap fn-for-dir (dir-dirs d))       
             (ormap (lambda (f) (string=? (file-name f) n)) (dir-files d))))) 
    (fn-for-dir d)))


(define (find? n d)
  (or 
   (ormap (lambda (d) (find? n d)) (dir-dirs d))       
   (ormap (lambda (f) (string=? (file-name f) n)) (dir-files d)))) 
