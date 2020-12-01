;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |338|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require htdp/dir)

#;
(define (fn-for-dir d)
  (... (dir-name d)                    ;String
       (fn-for-lod (dir-dirs d))       ;[List-of Dir] 
       (fn-for-lof (dir-files d))))    ;[List-of File]

(define D0 (create-dir "/var/log"))
(define D1 (create-dir "/Users/aronzvi/projects"))
(define D2 (create-dir "/Users/aronzvi/Documents"))

(define (how-many d)
  (local (; Dir.v3 -> Natural
          ; produces the number of files in d
          (define (fn-for-dir-v3 d)
            (+ 
             (fn-for-dir* (dir-dirs d))   
             (length (dir-files d))))
          ; Dir* -> Natual
          ; prodices the number of files in d*
          (define (fn-for-dir* d*)
            (cond [(empty? d*) 0]
                  [else
                   (+ (fn-for-dir-v3 (first d*))    
                      (fn-for-dir* (rest d*)))])))  
    (fn-for-dir-v3 d)))

(how-many D0)
(how-many D1)
(how-many D2)

;Why are you confident that how-many produces correct results for these directories? 
