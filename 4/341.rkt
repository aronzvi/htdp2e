;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |341|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; Dir -> Natural
; computes the total size of all the files in the entire directory tree
; storing a directory in a Dir structure costs 1 file storage unit
(check-expect (du D3) (+ 1
                         1
                         190482700 222243072 191260502
                         1
                         6148 96987322 161389592 34776139 20593711 40026882 38826603 14375859
                         37948548 20261865 178724411 48717468 260644341 30778326 47983700 67112791 98332468 53202212 284955733
                         50799930 44216495 50567357 29427689 422839443 40383430 269115826 128875770 74396614 84170962 9217912 43824050 96433811
                         51984305 10244 80860580 71815492 156027410 43027465))

;(define (du d) 0) ;stub

(define (du.v1 d)
  (local ((define (fn-for-dir d)
            (+ 1
               (fn-for-lod (dir-dirs d))       
               (fn-for-lof (dir-files d))))    
          (define (fn-for-lof lof)
            (foldr (lambda (f n) (+ (file-size f) n)) 0 lof))
          
          (define (fn-for-lod lod)
            (cond [(empty? lod) 0]
                  [else
                   (+ (fn-for-dir (first lod))     
                      (fn-for-lod (rest lod)))]))) 
    (fn-for-dir d)))

(define (du.v2 d)
  (local ((define (fn-for-dir d)
            (+ 1
               (fn-for-lod (dir-dirs d))       
               (foldr (lambda (f n) (+ (file-size f) n)) 0 (dir-files d))))
          
          (define (fn-for-lod lod)
            (cond [(empty? lod) 0]
                  [else
                   (+ (fn-for-dir (first lod))     
                      (fn-for-lod (rest lod)))]))) 
    (fn-for-dir d)))

(define (du.v3 d)
  (local ((define (fn-for-dir d)
            (+ 1
               (fn-for-lod (dir-dirs d))       
               (foldr (lambda (f n) (+ (file-size f) n)) 0 (dir-files d))))
          (define (fn-for-lod lod)
            (foldr (lambda (d n) (+ (fn-for-dir d) n)) 0 lod))) 
    (fn-for-dir d)))

(define (du.v4 d)
  (local ((define (fn-for-dir d)
            (+ 1
               (foldr (lambda (d n) (+ (fn-for-dir d) n)) 0 (dir-dirs d))
               (foldr (lambda (f n) (+ (file-size f) n)) 0 (dir-files d)))))
    (fn-for-dir d)))

(define (du d)
  (+ 1
     (foldr (lambda (d n) (+ (du d) n)) 0 (dir-dirs d))
     (foldr (lambda (f n) (+ (file-size f) n)) 0 (dir-files d))))
