;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |343|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; A Path is [List-of String].
; interpretation directions into a directory tree

(define P0 (list "TS" "Text" "part1"))

; Dir String -> [List-of Path]
; produces the list of all paths that lead to f in d
(check-expect (find-all D2 "xxxxz") empty)
(check-expect (find-all D2 "19_21_20.mov") '(("/Users/aronzvi/Documents"
                                              "/Users/aronzvi/Documents/Carlitos_figures_2"
                                              "/Users/aronzvi/Documents/Carlitos_figures_2/Carlitos_videos_Gri"
                                              "19_21_20.mov")))
(check-expect (find-all D2 "htdptest.txt")
              '(("/Users/aronzvi/Documents" "htdptest.txt")
                ("/Users/aronzvi/Documents" "/Users/aronzvi/Documents/WebEx" "htdptest.txt")
                ("/Users/aronzvi/Documents" "/Users/aronzvi/Documents/finance" "/Users/aronzvi/Documents/finance/fbars_signed" "htdptest.txt")))

;(define (find-all d f) empty) ;stub

(define (find-all d f)
  (filter (lambda (p) (string=? (first (reverse p)) f)) (ls-R d)))

; Dir -> [List-of Path]
; produces list of paths to all files contained in a given directory tree (list of full paths for every file in the tree)
(check-expect (length (ls-R D3))  (length (list (list "/Users/aronzvi/Documents/Carlitos_figures_2" " Can close legs (collect) when leading her boleo from side step and back ocho.mov")
                                                (list "/Users/aronzvi/Documents/Carlitos_figures_2" "Crossed right for both into boleo. Collect with left and sacada with enrosque.mov")
                                                (list "/Users/aronzvi/Documents/Carlitos_figures_2" "Repeating back ocho sequence step in with right and back and forward boleo.mov")
                                                (list  "/Users/aronzvi/Documents/Carlitos_figures_2" "diagonal back sacada into sewing and back gancho.mov")
                                                (list "/Users/aronzvi/Documents/Carlitos_figures_2" "repeating cambio del peso.mov")
                                                (list "/Users/aronzvi/Documents/Carlitos_figures_2" ".DS_Store")
                                                (list "/Users/aronzvi/Documents/Carlitos_figures_2" "/Users/aronzvi/Documents/Carlitos_figures_2/Carlitos_videos_Gri" "19_21_20.mov")
                                                (list "/Users/aronzvi/Documents/Carlitos_figures_2" "/Users/aronzvi/Documents/Carlitos_figures_2/Carlitos_videos_Gri" "1_2_3_4.mov")
                                                (list "/Users/aronzvi/Documents/Carlitos_figures_2" "/Users/aronzvi/Documents/Carlitos_figures_2/Carlitos_videos_Gri" "7_8_9_10.mov")
                                                (list "/Users/aronzvi/Documents/Carlitos_figures_2" "/Users/aronzvi/Documents/Carlitos_figures_2/Files_airmore_20180822_131701" "VID_20170830_124159.3gp")
                                                (list "/Users/aronzvi/Documents/Carlitos_figures_2" "/Users/aronzvi/Documents/Carlitos_figures_2/Files_airmore_20180822_131701" "VID_20170905_161200.3gp")
                                                (list "/Users/aronzvi/Documents/Carlitos_figures_2" "/Users/aronzvi/Documents/Carlitos_figures_2/Files_airmore_20180822_131701" "VID_20180202_145912.3gp")
                                                (list "/Users/aronzvi/Documents/Carlitos_figures_2" "/Users/aronzvi/Documents/Carlitos_figures_2/Files_airmore_20180822_131701" "VID_20170830_124502.3gp")
                                                (list "/Users/aronzvi/Documents/Carlitos_figures_2" "/Users/aronzvi/Documents/Carlitos_figures_2/Files_airmore_20180822_131701" "VID_20170905_161421.3gp")
                                                (list "/Users/aronzvi/Documents/Carlitos_figures_2" "/Users/aronzvi/Documents/Carlitos_figures_2/Files_airmore_20180822_131701" "VID_20180411_215645.3gp")
                                                (list "/Users/aronzvi/Documents/Carlitos_figures_2" "/Users/aronzvi/Documents/Carlitos_figures_2/Files_airmore_20180822_131701" "VID_20170830_125121.3gp")
                                                (list "/Users/aronzvi/Documents/Carlitos_figures_2" "/Users/aronzvi/Documents/Carlitos_figures_2/Files_airmore_20180822_131701" "VID_20180112_144506.3gp")
                                                (list "/Users/aronzvi/Documents/Carlitos_figures_2" "/Users/aronzvi/Documents/Carlitos_figures_2/Files_airmore_20180822_131701" "VID_20180504_001753.3gp")
                                                (list "/Users/aronzvi/Documents/Carlitos_figures_2" "/Users/aronzvi/Documents/Carlitos_figures_2/Files_airmore_20180822_131701" "VID_20170830_125510.3gp")
                                                (list "/Users/aronzvi/Documents/Carlitos_figures_2" "/Users/aronzvi/Documents/Carlitos_figures_2/Files_airmore_20180822_131701" "VID_20180112_150831.3gp")
                                                (list "/Users/aronzvi/Documents/Carlitos_figures_2" "/Users/aronzvi/Documents/Carlitos_figures_2/Files_airmore_20180822_131701" "VID_20180615_191742.3gp")
                                                (list "/Users/aronzvi/Documents/Carlitos_figures_2" "/Users/aronzvi/Documents/Carlitos_figures_2/Files_airmore_20180822_131701" "VID_20170830_125527.3gp")
                                                (list "/Users/aronzvi/Documents/Carlitos_figures_2" "/Users/aronzvi/Documents/Carlitos_figures_2/Files_airmore_20180822_131701" "VID_20180112_152857.3gp")
                                                (list "/Users/aronzvi/Documents/Carlitos_figures_2" "/Users/aronzvi/Documents/Carlitos_figures_2/Files_airmore_20180822_131701" "VID_20180803_163306.3gp")
                                                (list "/Users/aronzvi/Documents/Carlitos_figures_2" "/Users/aronzvi/Documents/Carlitos_figures_2/Files_airmore_20180822_131701" "VID_20170830_130904.3gp")
                                                (list "/Users/aronzvi/Documents/Carlitos_figures_2" "/Users/aronzvi/Documents/Carlitos_figures_2/Files_airmore_20180822_131701" "VID_20180112_153418.3gp")
                                                (list "/Users/aronzvi/Documents/Carlitos_figures_2" "/Users/aronzvi/Documents/Carlitos_figures_2/Files_airmore_20180822_131701" "VID_20180807_135638.3gp")
                                                (list "/Users/aronzvi/Documents/Carlitos_figures_2" "/Users/aronzvi/Documents/Carlitos_figures_2/Files_airmore_20180822_131701" "VID_20170830_130926.3gp")
                                                (list "/Users/aronzvi/Documents/Carlitos_figures_2" "/Users/aronzvi/Documents/Carlitos_figures_2/Files_airmore_20180822_131701" "VID_20180123_151128.3gp")
                                                (list "/Users/aronzvi/Documents/Carlitos_figures_2" "/Users/aronzvi/Documents/Carlitos_figures_2/Files_airmore_20180822_131701" "VID_20180123_151815.3gp")
                                                (list "/Users/aronzvi/Documents/Carlitos_figures_2" "/Users/aronzvi/Documents/Carlitos_figures_2/Files_airmore_20180822_131701" "VID_20180807_135723.3gp")
                                                (list "/Users/aronzvi/Documents/Carlitos_figures_2" "/Users/aronzvi/Documents/Carlitos_figures_2/Files_airmore_20180822_131701" "VID_20180810_151250.3gp")
                                                (list "/Users/aronzvi/Documents/Carlitos_figures_2" "/Users/aronzvi/Documents/Carlitos_figures_2/Files_airmore_20180822_131701" "VID_20170830_131223.3gp")
                                                (list "/Users/aronzvi/Documents/Carlitos_figures_2" "/Users/aronzvi/Documents/Carlitos_figures_2/Files_airmore_20180822_131701" "VID_20170830_132915.3gp")
                                                (list "/Users/aronzvi/Documents/Carlitos_figures_2" "/Users/aronzvi/Documents/Carlitos_figures_2/Files_airmore_20180822_131701" "VID_20180123_152003.3gp")
                                                (list "/Users/aronzvi/Documents/Carlitos_figures_2" "/Users/aronzvi/Documents/Carlitos_figures_2/Files_airmore_20180822_131701" "VID_20180817_182250.3gp")
                                                (list "/Users/aronzvi/Documents/Carlitos_figures_2" "/Users/aronzvi/Documents/Carlitos_figures_2/Files_airmore_20180822_131701" "VID_20170831_165844.3gp")
                                                (list "/Users/aronzvi/Documents/Carlitos_figures_2" "/Users/aronzvi/Documents/Carlitos_figures_2/Files_airmore_20180822_131701" "VID_20180123_152705.3gp")
                                                (list "/Users/aronzvi/Documents/Carlitos_figures_2" "/Users/aronzvi/Documents/Carlitos_figures_2/Files_airmore_20180822_131701" "VID_20170831_170057.3gp")
                                                (list "/Users/aronzvi/Documents/Carlitos_figures_2" "/Users/aronzvi/Documents/Carlitos_figures_2/Files_airmore_20180822_131701" "VID_20180127_155056.3gp")
                                                (list "/Users/aronzvi/Documents/Carlitos_figures_2" "/Users/aronzvi/Documents/Carlitos_figures_2/Files_airmore_20180822_131701" ".DS_Store"))))

;(define (ls-R d) empty) ;stub

(define (ls-R d)
  (local (; Dir Path -> [List-of Path]
          (define (fn-for-dir d p)
            (local ((define full-dir-path (add-to-end-of-list (symbol->string (dir-name d)) p)))
              (append (create-file-paths (dir-files d) full-dir-path)
                      (fn-for-lod (dir-dirs d) full-dir-path))))    

          ; [List-of File] Path -> [List-of Path]
          ; produces list of all file paths with given folder path to files
          (define (create-file-paths lof p)
            (map (lambda (f) (add-to-end-of-list (file-name f) p)) lof))

          ; [List-of Dir] Path -> [List-of Path]
          (define (fn-for-lod lod p)
            (cond [(empty? lod) empty]
                  [else
                   (append (fn-for-dir (first lod) p)     
                           (fn-for-lod (rest lod) p))]))) 
    (fn-for-dir d empty)))

; Y [List-of X] -> [List-of X and Y]  - don't like this signature. 
; adds x to end of lox
(check-expect (add-to-end-of-list 1 empty) (list 1))
(check-expect (add-to-end-of-list 1 (list "dd" 3 2)) (list "dd" 3 2 1))

;(define (add-to-end-of-list x lox) lox) ;stub

(define (add-to-end-of-list x lox)
  (reverse (cons x (reverse lox)))) 