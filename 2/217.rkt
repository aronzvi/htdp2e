;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |217|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Constants:

(define HEIGHT 300)
(define WIDTH 500)
(define MTSCN (empty-scene WIDTH HEIGHT))
(define SEGMENT-R 5)
(define SEGMENT-COLOR "red")
(define SEGMENT (circle SEGMENT-R "solid" SEGMENT-COLOR))
(define DIAMETER (* SEGMENT-R 2))
(define WORM-POS-START-X (/ WIDTH 2))
(define WORM-POS-START-Y (/ HEIGHT 2))
(define WORM-DIRECTION-START "right")
(define WORM-SPEED DIAMETER)
(define TICK-RATE 0.5)
(define WORM-START (make-worm "right" (list (make-posn 40 10))))


;; Data definitions:

;; A direction is one of:
;; - "up"
;; - "down"
;; - "left"
;; - "right"

(define (fn-for-direction d)
  (cond [(string=? d "up") (...)]
        [(string=? d "down") (...)]
        [(string=? d "left") (...)]
        [(string=? d "right") (...)]))

;; Template rules used:
;; - one of: 4 cases
;; - atomic distinct: "up"
;; - atomic distinct: "down"
;; - atomic distinct: "left"
;; - atomic distinct: "right"

;; NELoP is one of:
;; - (cons Posn empty)
;; - (cons Posn NELoP)
;; interp. A list of Posns with at least one posn
(define LOP1 (make-posn 40 10))
(define LOP2 (list (make-posn 40 10) (make-posn 80 100)))

(define (fn-for-nelop nelop)
  (cond [(empty? (rest nelop)) (... (fn-for-posn (first nelop)))]
        [else
         (... (fn-for-posn (first nelop))     ;Posn
              (fn-for-nelop (rest nelop)))])) ;NELoP

;; Template rules used:
;; - one of: 2 cases
;; - compound: (cons Posn empty)
;; - reference: (first lop) is Posn
;; - compound: (cons Posn NELoP)
;; - reference: (first lop) is Posn
;; - self-reference: (rest lop) is NELoP

(define-struct worm [direction segments])
;; a Worm is (make-worm Direction NELoP)
;; interp. a worm with its direction and segments
;; A worm will alway have at least one segment (it's head).
;; The head is the first of the list. and the tail grows towards the end of the list 
(define W1 (make-worm "right" (list (make-posn 40 10))))                                     ;just a head
(define W2 (make-worm "right" (list (make-posn 40 10) (make-posn (+ 40 WORM-SPEED) 10))))    ;two segments, straight line
(define W3 (make-worm "right"                                                                ;4 segments, straight line
                      (list (make-posn 40 10)
                            (make-posn (+ 40 WORM-SPEED) 10)
                            (make-posn (+ (+ 40 WORM-SPEED) WORM-SPEED) 10)
                            (make-posn (+(+ (+ 40 WORM-SPEED) WORM-SPEED) WORM-SPEED) 10))))
(define W4 (make-worm "up"                                                                    ;5 segments, straight line and up at head
                      (list (make-posn 40 10)
                            (make-posn (+ 40 WORM-SPEED) 10)
                            (make-posn (+ (+ 40 WORM-SPEED) WORM-SPEED) 10)
                            (make-posn (+(+ (+ 40 WORM-SPEED) WORM-SPEED) WORM-SPEED) 10)
                            (make-posn (+ (+ (+ 40 WORM-SPEED) WORM-SPEED) (+ 10 WORM-SPEED)) 10))))
(define WORM-START (make-worm WORM-DIRECTION-START                                   
                              (list (make-posn (+ (+ (+ (+ (+ (+ WORM-POS-START-X WORM-SPEED) WORM-SPEED) WORM-SPEED) WORM-SPEED) WORM-SPEED) WORM-SPEED) (- (- (- WORM-POS-START-Y WORM-SPEED) WORM-SPEED) WORM-SPEED))
                                    (make-posn (+ (+ (+ (+ (+ WORM-POS-START-X WORM-SPEED) WORM-SPEED) WORM-SPEED) WORM-SPEED) WORM-SPEED) (- (- (- WORM-POS-START-Y WORM-SPEED) WORM-SPEED) WORM-SPEED))
                                    (make-posn (+ (+ (+ (+ WORM-POS-START-X WORM-SPEED) WORM-SPEED) WORM-SPEED) WORM-SPEED) (- (- (- WORM-POS-START-Y WORM-SPEED) WORM-SPEED) WORM-SPEED))
                                    (make-posn (+ (+ (+ WORM-POS-START-X WORM-SPEED) WORM-SPEED) WORM-SPEED) (- (- (- WORM-POS-START-Y WORM-SPEED) WORM-SPEED) WORM-SPEED))
                                    (make-posn (+ (+ (+ WORM-POS-START-X WORM-SPEED) WORM-SPEED) WORM-SPEED) (- (- WORM-POS-START-Y WORM-SPEED) WORM-SPEED))
                                    (make-posn (+ (+ (+ WORM-POS-START-X WORM-SPEED) WORM-SPEED) WORM-SPEED) (- WORM-POS-START-Y WORM-SPEED))
                                    (make-posn (+ (+ (+ WORM-POS-START-X WORM-SPEED) WORM-SPEED) WORM-SPEED) WORM-POS-START-Y)
                                    (make-posn (+ (+ WORM-POS-START-X WORM-SPEED) WORM-SPEED) WORM-POS-START-Y)
                                    (make-posn (+ WORM-POS-START-X WORM-SPEED) WORM-POS-START-Y)
                                    (make-posn WORM-POS-START-X WORM-POS-START-Y))))

(define (fn-for-worm w)
  (... (fn-for-direction (worm-direction w)) ;Direction
       (fn-for-lop (worm-segments w))))        ;NELoP

;; Template rules used:
;; - compound: 2 fields
;; - reference: (first w) is Direction
;; - reference: (rest w) is ListOfPosn

;; Functions:

;; Number -> Worm
;; start the world with (worm-main (make-worm ...) TICK-RATE)
;; 
(define (worm-main w rate)
  (big-bang w                  ; Worm
    (on-tick   move-worm rate) ; Worm -> Worm
    (to-draw   render-worm)    ; Worm -> Image
    (on-key    handle-key)))   ; Worm KeyEvent -> Worm

;; Worm -> Worm
;; Moves the worm in the direction it is moving in by WORM-SPEED
(check-expect (move-worm (make-worm "up" (list (make-posn 50 50))))                                    
              (make-worm "up" (list (make-posn 50 (- 50 WORM-SPEED)))))
(check-expect (move-worm (make-worm "down" (list (make-posn 50 50))))                                    
              (make-worm "down" (list (make-posn 50 (+ 50 WORM-SPEED)))))
(check-expect (move-worm (make-worm "right" (list (make-posn 50 50))))                                    
              (make-worm "right" (list (make-posn (+ 50 WORM-SPEED) 50))))
(check-expect (move-worm (make-worm "left" (list (make-posn 50 50))))                                    
              (make-worm "left" (list (make-posn (- 50 WORM-SPEED) 50))))

(check-expect (move-worm (make-worm "left" (list (make-posn 40 10)
                                                 (make-posn (+ 40 WORM-SPEED) 10))))
              (make-worm "left" (list (make-posn (- 40 WORM-SPEED) 10)
                                      (make-posn 40 10))))
(check-expect (move-worm (make-worm "right"
                                    (list (make-posn (+ 40 WORM-SPEED) 10)
                                          (make-posn 40 10))))
              (make-worm "right" (list (make-posn (+ (+ 40 WORM-SPEED) WORM-SPEED) 10)
                                       (make-posn (+ 40 WORM-SPEED) 10))))
(check-expect (move-worm (make-worm "up" (list (make-posn 30 50)
                                               (make-posn 30 (+ 50 WORM-SPEED)))))                                    
              (make-worm "up" (list (make-posn 30 (- 50 WORM-SPEED))
                                    (make-posn 30 50))))
(check-expect (move-worm (make-worm "down" (list (make-posn 30 (+ 50 WORM-SPEED))
                                                 (make-posn 3 50))))                                    
              (make-worm "down" (list (make-posn 30 (+ (+ 50 WORM-SPEED )WORM-SPEED))
                                      (make-posn 30 (+ 50 WORM-SPEED)))))
(check-expect (move-worm (make-worm "up" (list (make-posn 40 (- (- (- (- 100 WORM-SPEED) WORM-SPEED) WORM-SPEED) WORM-SPEED))
                                               (make-posn 40 (- (- (- 100 WORM-SPEED) WORM-SPEED) WORM-SPEED))
                                               (make-posn 40 (- (- 100 WORM-SPEED) WORM-SPEED))
                                               (make-posn 40 (- 100 WORM-SPEED))
                                               (make-posn 40 100))))
              (make-worm "up" (list (make-posn 40 (- (- (- (- (- 100 WORM-SPEED) WORM-SPEED) WORM-SPEED) WORM-SPEED) WORM-SPEED))
                                    (make-posn 40 (- (- (- (- 100 WORM-SPEED) WORM-SPEED) WORM-SPEED) WORM-SPEED))
                                    (make-posn 40 (- (- (- 100 WORM-SPEED) WORM-SPEED) WORM-SPEED))
                                    (make-posn 40 (- (- 100 WORM-SPEED) WORM-SPEED))
                                    (make-posn 40 (- 100 WORM-SPEED)))))

;(define (move-worm w) w) ;stub

(define (move-worm w)
  (make-worm (worm-direction w)
             (move-segments-in-direction (worm-segments w) (worm-direction w))))

;; NELoP Direction -> NELoP
;; moves the segments in given direction.
(check-expect (move-segments-in-direction (list (make-posn 50 50)) "up")
              (list (make-posn 50 (- 50 WORM-SPEED))))
(check-expect (move-segments-in-direction (list (make-posn 50 50)) "down")
              (list (make-posn 50 (+ 50 WORM-SPEED))))
(check-expect (move-segments-in-direction (list (make-posn 50 50)) "right")                                    
              (list (make-posn (+ 50 WORM-SPEED) 50)))
(check-expect (move-segments-in-direction (list (make-posn 50 50)) "left")                                    
              (list (make-posn (- 50 WORM-SPEED) 50)))
(check-expect (move-segments-in-direction (list (make-posn 40 10)
                                                (make-posn (+ 40 WORM-SPEED) 10))
                                          "left")
              (list (make-posn (- 40 WORM-SPEED) 10)
                    (make-posn 40 10)))
(check-expect (move-segments-in-direction (list (make-posn (+ 40 WORM-SPEED) 10)
                                                (make-posn 40 10))
                                          "right")
              (list (make-posn (+ (+ 40 WORM-SPEED) WORM-SPEED) 10)
                    (make-posn (+ 40 WORM-SPEED) 10)))

(check-expect (move-segments-in-direction (list (make-posn 30 50)
                                                (make-posn 30 (+ 50 WORM-SPEED)))
                                          "up")
              (list (make-posn 30 (- 50 WORM-SPEED))
                    (make-posn 30 50)))
(check-expect (move-segments-in-direction (list (make-posn 30 (+ 50 WORM-SPEED))
                                                (make-posn 3 50))
                                          "down")
              (list (make-posn 30 (+ (+ 50 WORM-SPEED )WORM-SPEED))
                    (make-posn 30 (+ 50 WORM-SPEED))))
(check-expect (move-segments-in-direction (list (make-posn 40 (- (- (- (- 100 WORM-SPEED) WORM-SPEED) WORM-SPEED) WORM-SPEED))
                                                (make-posn 40 (- (- (- 100 WORM-SPEED) WORM-SPEED) WORM-SPEED))
                                                (make-posn 40 (- (- 100 WORM-SPEED) WORM-SPEED))
                                                (make-posn 40 (- 100 WORM-SPEED))
                                                (make-posn 40 100)) "up")
              (list (make-posn 40 (- (- (- (- (- 100 WORM-SPEED) WORM-SPEED) WORM-SPEED) WORM-SPEED) WORM-SPEED))
                    (make-posn 40 (- (- (- (- 100 WORM-SPEED) WORM-SPEED) WORM-SPEED) WORM-SPEED))
                    (make-posn 40 (- (- (- 100 WORM-SPEED) WORM-SPEED) WORM-SPEED))
                    (make-posn 40 (- (- 100 WORM-SPEED) WORM-SPEED))
                    (make-posn 40 (- 100 WORM-SPEED))))

;(define (move-segments-in-direction nelop d) nelop) ;stub

(define (move-segments-in-direction nelop d)
  (cond [(string=? d "up") (move-segments-up nelop)]
        [(string=? d "down") (move-segments-down nelop)]
        [(string=? d "left") (move-segments-left nelop)]
        [(string=? d "right") (move-segments-right nelop)]))

;; NELoP -> NELoP
;; moves segments up
(check-expect (move-segments-up (list (make-posn 50 50)))
              (list (make-posn 50 (- 50 WORM-SPEED))))
(check-expect (move-segments-up (list (make-posn 30 50)
                                      (make-posn 30 (+ 50 WORM-SPEED))))
              (list (make-posn 30 (- 50 WORM-SPEED))
                    (make-posn 30 50)))
(check-expect (move-segments-up (list (make-posn 30 (- 50 WORM-SPEED))
                                      (make-posn 30 50)
                                      (make-posn 30 (+ 50 WORM-SPEED))))
              (list (make-posn 30 (- (- 50 WORM-SPEED) WORM-SPEED))
                    (make-posn 30 (- 50 WORM-SPEED))
                    (make-posn 30 50)))

;(define (move-segments-up nelop) nelop) ;stub

(define (move-segments-up nelop)
  (cond [(empty? (rest nelop)) (list (move-segment-up (first nelop) WORM-SPEED))]
        [else
         (cons (move-segment-up (first nelop) WORM-SPEED)     
               (remove-last-segment nelop))])) 

;; Posn n -> Posn
;; moves posn up by n
(check-expect (move-segment-up (make-posn 50 50) WORM-SPEED)
              (make-posn 50 (- 50 WORM-SPEED)))

;(define (move-segment-up p n) p) ;stub

;; Template from Posn

(define (move-segment-up p n)
  (make-posn (posn-x p)
             (- (posn-y p) n)))

;; NELoP -> ListOfPosn
;; removes last posn from list
(check-expect (remove-last-segment (list (make-posn 80 20))) empty)
(check-expect (remove-last-segment (list  (make-posn 60 70)
                                          (make-posn 80 20)))
              (list (make-posn 60 70)))
(check-expect (remove-last-segment (list (make-posn 50 30)
                                         (make-posn 60 70)
                                         (make-posn 80 20)))
              (list (make-posn 50 30)
                    (make-posn 60 70)))

;(define (remove-last-segment lop) lop) ;stub

(define (remove-last-segment nelop)
  (cond [(empty? (rest nelop)) empty]
        [else
         (cons (first nelop)    
               (remove-last-segment (rest nelop)))])) 

;; NELop -> NELop
;; moves segments down
(check-expect (move-segments-down (list (make-posn 50 50)))
              (list (make-posn 50 (+ 50 WORM-SPEED))))
(check-expect (move-segments-down (list (make-posn 30 (+ 50 WORM-SPEED))
                                        (make-posn 3 50)))
              (list (make-posn 30 (+ (+ 50 WORM-SPEED )WORM-SPEED))
                    (make-posn 30 (+ 50 WORM-SPEED))))

;(define (move-segments-down nelop) nelop) ;stub

(define (move-segments-down nelop)
  (cond [(empty? (rest nelop)) (list (move-segment-down (first nelop) WORM-SPEED))]
        [else
         (cons (move-segment-down (first nelop) WORM-SPEED)     
               (remove-last-segment nelop))]))

;; Posn n -> Posn
;; moves posn down by n
(check-expect (move-segment-down (make-posn 50 50) WORM-SPEED)
              (make-posn 50 (+ 50 WORM-SPEED)))

;(define (move-segment-down p n) p) ;stub

;; Template from Posn

(define (move-segment-down p n)
  (make-posn (posn-x p)
             (+ (posn-y p) n)))

;; NELop -> NELop
;; moves segments left
(check-expect (move-segments-left (list (make-posn 50 50)))
              (list (make-posn (- 50 WORM-SPEED) 50)))
(check-expect (move-segments-left (list (make-posn 40 10)
                                        (make-posn (+ 40 WORM-SPEED) 10)))
              (list (make-posn (- 40 WORM-SPEED) 10)
                    (make-posn 40 10)))

;(define (move-segments-left nelop) nelop) ;stub

(define (move-segments-left nelop)
  (cond [(empty? (rest nelop)) (list (move-segment-left (first nelop) WORM-SPEED))]
        [else
         (cons (move-segment-left (first nelop) WORM-SPEED)     
               (remove-last-segment nelop))]))

;; Posn n -> Posn
;; moves posn left by n
(check-expect (move-segment-left (make-posn 50 50) WORM-SPEED)
              (make-posn (- 50 WORM-SPEED) 50))

;(define (move-segment-left p n) p) ;stub

;; Template from Posn

(define (move-segment-left p n)
  (make-posn (- (posn-x p) n)
             (posn-y p)))

;; ListOfPosn -> ListOfPosn
;; moves segments right
(check-expect (move-segments-right (list (make-posn 50 50)))                                  
              (list (make-posn (+ 50 WORM-SPEED) 50)))
(check-expect (move-segments-right (list (make-posn (+ 40 WORM-SPEED) 10)
                                         (make-posn 40 10)))
              (list (make-posn (+ (+ 40 WORM-SPEED) WORM-SPEED) 10)
                    (make-posn (+ 40 WORM-SPEED) 10)))

;(define (move-segments-right nelop) nelop) ;stub

(define (move-segments-right nelop)
  (cond [(empty? (rest nelop)) (list (move-segment-right (first nelop) WORM-SPEED))]
        [else
         (cons (move-segment-right (first nelop) WORM-SPEED)     
               (remove-last-segment nelop))]))

;; Posn n -> Posn
;; moves posn right by n
(check-expect (move-segment-right (make-posn 50 50) WORM-SPEED)
              (make-posn (+ 50 WORM-SPEED) 50))

;(define (move-segment-right p n) p) ;stub

;; Template from Posn

(define (move-segment-right p n)
  (make-posn (+ (posn-x p) n)
             (posn-y p)))

;; Worm -> Image
;; render the Worm on MTSCN
(check-expect (render-worm (make-worm WORM-DIRECTION-START                                   
                                      (list (make-posn WORM-POS-START-X WORM-POS-START-Y))))
              (place-image SEGMENT WORM-POS-START-X WORM-POS-START-Y MTSCN))
(check-expect (render-worm (make-worm "right" (list (make-posn 40 10) (make-posn (+ 40 WORM-SPEED) 10))))
              (place-image SEGMENT 40 10 (place-image SEGMENT (+ 40 WORM-SPEED) 10 MTSCN)))

;(define (render-worm w) empty-image) ;stub

(define (render-worm w)
  (render-segments (worm-segments w) MTSCN))

;; ListOfPosn Image -> Image
;; renders segments onto img
(check-expect (render-segments empty MTSCN) MTSCN)
(check-expect (render-segments (list (make-posn WORM-POS-START-X WORM-POS-START-Y)) MTSCN)
              (place-image SEGMENT WORM-POS-START-X WORM-POS-START-Y MTSCN))
(check-expect (render-segments (list (make-posn 40 10) (make-posn (+ 40 WORM-SPEED) 10)) MTSCN)
              (place-image SEGMENT 40 10 (place-image SEGMENT (+ 40 WORM-SPEED) 10 MTSCN)))

;(define (render-segments lop img) img) ;stub

(define (render-segments lop img)
  (cond [(empty? lop) img]
        [else
         (render-segment (first lop)
                         (render-segments (rest lop) img))]))

;; Posn Image -> Image
;; renders SEGMENT on img at pos
(check-expect (render-segment (make-posn 40 10) MTSCN)
              (place-image SEGMENT 40 10 MTSCN))
(check-expect (render-segment (make-posn 70 90) MTSCN)
              (place-image SEGMENT 70 90 MTSCN))

;(define (render-segment p img) img) ;stub

;; Template from Posn

(define (render-segment p img)
  (place-image SEGMENT (posn-x p)
               (posn-y p)
               img))

;; Worm KeyEvent -> Worm
;; handle the arrow keys
(check-expect (handle-key (make-worm "right" (list (make-posn 40 50))) "up")
              (make-worm "up" (list (make-posn 40 50))))
(check-expect (handle-key (make-worm "right" (list (make-posn 40 50))) "down")
              (make-worm "down" (list (make-posn 40 50))))
(check-expect (handle-key (make-worm "right" (list (make-posn 40 50))) "left")
              (make-worm "left" (list (make-posn 40 50))))
(check-expect (handle-key (make-worm "right" (list (make-posn 40 50))) "right")
              (make-worm "right" (list( make-posn 40 50))))
(check-expect (handle-key (make-worm "right" (list (make-posn 40 50))) "x")
              (make-worm "right" (list( make-posn 40 50))))


;(define (handle-key w ke) w) ;stub

#;
(define (handle-key w ke)
  (cond [(key=? "up" ke) (... w)]
        [(key=? "down" ke) (... w)]
        [(key=? "left" ke) (... w)]
        [(key=? "right" ke) (... w)]
        [else
         (... w)]))
;; Template formed using the large enumeration special case

(define (handle-key w ke)
  (cond [(key=? "up" ke) (set-worm-direction-up w)]
        [(key=? "down" ke) (set-worm-direction-down w)]
        [(key=? "left" ke) (set-worm-direction-left w)]
        [(key=? "right" ke) (set-worm-direction-right w)]
        [else w]))

;; Worm -> Worm
;; sets direction of worm to up
(check-expect (set-worm-direction-up (make-worm "right" (list (make-posn 40 50))))
              (make-worm "up" (list (make-posn 40 50))))

;(define (set-worm-direction-up w) w) ;stub

(define (set-worm-direction-up w)
  (set-worm-direction w "up"))

;; Worm -> Worm
;; sets direction of worm to down

;; Worm Direction -> Worm
;; sets direction of worm to d
(check-expect (set-worm-direction (make-worm "left" (list (make-posn 40 50))) "up")
              (make-worm "up" (list (make-posn 40 50))))
(check-expect (set-worm-direction (make-worm "left" (list (make-posn 40 50))) "down")
              (make-worm "down" (list (make-posn 40 50))))
(check-expect (set-worm-direction (make-worm "down" (list (make-posn 40 50))) "left")
              (make-worm "left" (list (make-posn 40 50))))
(check-expect (set-worm-direction (make-worm "down" (list (make-posn 40 50))) "right")
              (make-worm "right" (list (make-posn 40 50))))

;(define (set-worm-direction w d) d) ;stub

(define (set-worm-direction w d)
  (make-worm d (worm-segments w)))    

;; Worm -> Worm
;; sets direction of worm to down
(check-expect (set-worm-direction-down (make-worm "right" (list (make-posn 40 50))))
              (make-worm "down" (list (make-posn 40 50))))

;(define (set-worm-direction-down w) w) ;stub

(define (set-worm-direction-down w)
  (set-worm-direction w "down"))

;; Worm -> Worm
;; sets direction of worm to left
(check-expect (set-worm-direction-left (make-worm "right" (list (make-posn 40 50))))
              (make-worm "left" (list (make-posn 40 50))))

;(define (set-worm-direction-left w) w) ;stub

(define (set-worm-direction-left w)
  (set-worm-direction w "left"))

;; Worm -> Worm
;; sets directoin of worm to right
(check-expect (set-worm-direction-right (make-worm "up" (list (make-posn 40 50))))
              (make-worm "right" (list (make-posn 40 50))))

;(define (set-worm-direction-right w) w) ;stub

(define (set-worm-direction-right w)
   (set-worm-direction w "right"))


