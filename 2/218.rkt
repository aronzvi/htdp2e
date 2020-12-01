;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |218|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
(define AT-TOP-WALL SEGMENT-R)
(define AT-BOTTOM-WALL (- HEIGHT SEGMENT-R))
(define AT-LEFT-WALL SEGMENT-R)
(define AT-RIGHT-WALL (- WIDTH SEGMENT-R))
(define HIT-WALL-MSG "worm hit wall")
(define RAN-INTO-SELF-MSG "worm ran into self")
(define DISPLAY-STATE #true)

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
;; start the world with (worm-main WORM-START TICK-RATE DISPLAY-STATE)
;; 
(define (worm-main w rate display-state)
  (big-bang w                                 ; Worm
    (on-tick   move-worm rate)                ; Worm -> Worm
    (to-draw   render-worm)                   ; Worm -> Image
    (stop-when game-over? render-game-over)   ; Worm -> ???
    (on-key    handle-key)                    ; Worm KeyEvent -> Worm
    (state display-state)))                  

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
(check-expect (handle-key (make-worm "up" (list (make-posn 40 50))) "right")
              (make-worm "right" (list( make-posn 40 50))))
(check-expect (handle-key (make-worm "up" (list (make-posn 40 50))) "left")
              (make-worm "left" (list( make-posn 40 50))))
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
;; sets direction of worm to up. can go up only if not going down
(check-expect (set-worm-direction-up (make-worm "right" (list (make-posn 40 50))))
              (make-worm "up" (list (make-posn 40 50))))
(check-expect (set-worm-direction-up (make-worm "left" (list (make-posn 40 50))))
              (make-worm "up" (list (make-posn 40 50))))
(check-expect (set-worm-direction-up (make-worm "up" (list (make-posn 40 50))))
              (make-worm "up" (list (make-posn 40 50))))
(check-expect (set-worm-direction-up (make-worm "down" (list (make-posn 40 50))))
              (make-worm "down" (list (make-posn 40 50))))

;(define (set-worm-direction-up w) w) ;stub

(define (set-worm-direction-up w)
  (if (not (direction-down? (worm-direction w))) 
      (set-worm-direction w "up")
      w))        

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
;; sets direction of worm to down. can only go down if not going up
(check-expect (set-worm-direction-down (make-worm "right" (list (make-posn 40 50))))
              (make-worm "down" (list (make-posn 40 50))))
(check-expect (set-worm-direction-down (make-worm "left" (list (make-posn 40 50))))
              (make-worm "down" (list (make-posn 40 50))))
(check-expect (set-worm-direction-down (make-worm "down" (list (make-posn 40 50))))
              (make-worm "down" (list (make-posn 40 50))))
(check-expect (set-worm-direction-down (make-worm "up" (list (make-posn 40 50))))
              (make-worm "up" (list (make-posn 40 50))))

;(define (set-worm-direction-down w) w) ;stub

(define (set-worm-direction-down w)
  (if (not (direction-up? (worm-direction w)))
      (set-worm-direction w "down")
      w))

;; Worm -> Worm
;; sets direction of worm to left. can only go left if not going right
(check-expect (set-worm-direction-left (make-worm "up" (list (make-posn 40 50))))
              (make-worm "left" (list (make-posn 40 50))))
(check-expect (set-worm-direction-left (make-worm "down" (list (make-posn 40 50))))
              (make-worm "left" (list (make-posn 40 50))))
(check-expect (set-worm-direction-left (make-worm "left" (list (make-posn 40 50))))
              (make-worm "left" (list (make-posn 40 50))))
(check-expect (set-worm-direction-left (make-worm "right" (list (make-posn 40 50))))
              (make-worm "right" (list (make-posn 40 50))))

;(define (set-worm-direction-left w) w) ;stub

(define (set-worm-direction-left w)
  (if (not (direction-right? (worm-direction w)))
      (set-worm-direction w "left")
      w))

;; Worm -> Worm
;; sets directoin of worm to right. can only go right if not going left
(check-expect (set-worm-direction-right (make-worm "up" (list (make-posn 40 50))))
              (make-worm "right" (list (make-posn 40 50))))
(check-expect (set-worm-direction-right (make-worm "down" (list (make-posn 40 50))))
              (make-worm "right" (list (make-posn 40 50))))
(check-expect (set-worm-direction-right (make-worm "right" (list (make-posn 40 50))))
              (make-worm "right" (list (make-posn 40 50))))
(check-expect (set-worm-direction-right (make-worm "left" (list (make-posn 40 50))))
              (make-worm "left" (list (make-posn 40 50))))

;(define (set-worm-direction-right w) w) ;stub

(define (set-worm-direction-right w)
  (if (not (direction-left? (worm-direction w)))
      (set-worm-direction w "right")
      w))

;; Worm -> Boolean
;; produces true if the worm has run into the walls of the world or into itself
(check-expect (game-over? (make-worm "down" (list (make-posn (+ 50 WORM-SPEED) (- 40 WORM-SPEED))
                                                  (make-posn (+ (+ 50 WORM-SPEED) WORM-SPEED) (- 40 WORM-SPEED))
                                                  (make-posn (+ (+ 50 WORM-SPEED) WORM-SPEED) 40)
                                                  (make-posn (+ 50 WORM-SPEED) 40)
                                                  (make-posn 50 40))))
              #true)
(check-expect (game-over? (make-worm "right" (list (make-posn AT-RIGHT-WALL 40))))
              #true)

;(define (game-over? w) #false) ;stub

(define (game-over? w)
  (or (ran-into-walls? w)
      (ran-into-self? (move-worm w))))

;; Worm -> Boolean
;; produces true if worm ran into wall
(check-expect (ran-into-walls? (make-worm "right" (list (make-posn AT-RIGHT-WALL 40))))
              #true)
(check-expect (ran-into-walls? (make-worm "left" (list (make-posn AT-LEFT-WALL 40))))
              #true)
(check-expect (ran-into-walls? (make-worm "up" (list (make-posn 40 AT-TOP-WALL))))
              #true)
(check-expect (ran-into-walls? (make-worm "down" (list (make-posn 40 AT-BOTTOM-WALL))))
              #true)

;(define (ran-into-walls? w) #false) ;stub

(define (ran-into-walls? w)
  (or (ran-into-right-wall? w)
      (ran-into-left-wall? w)
      (ran-into-top-wall? w)
      (ran-into-bottom-wall? w)))

;; Worm -> Boolean
;; produces true if worm ran into right wall
(check-expect (ran-into-right-wall? (make-worm "right" (list (make-posn AT-RIGHT-WALL 40))))
              #true)
(check-expect (ran-into-right-wall? (make-worm "right" (list (make-posn (+ AT-RIGHT-WALL 1) 40))))
              #true)
(check-expect (ran-into-right-wall? (make-worm "right" (list (make-posn (- AT-RIGHT-WALL 1) 40))))
              #false)
(check-expect (ran-into-right-wall? (make-worm "right" (list (make-posn 50 40))))
              #false)
(check-expect (ran-into-right-wall? (make-worm "up" (list (make-posn AT-RIGHT-WALL 40))))
              #false)

;(define (ran-into-right-wall? w) #false) ;stub

(define (ran-into-right-wall? w)
  (and (direction-right? (worm-direction w))
       (at-or-passed-right-wall? (first (worm-segments w)))))

;; Direction -> Boolean
;; produces true if direction is right direction
(check-expect (direction-right? "right") #true)
(check-expect (direction-right? "left") #false)
(check-expect (direction-right? "up") #false)
(check-expect (direction-right? "down") #false)

;(define (direction-right? d) #false) ;stub

(define (direction-right? d)
  (string=? d "right"))

;; Posn -> Boolean
;; Produces true if posn is at or past right wall
(check-expect (at-or-passed-right-wall? (make-posn WIDTH 40))
              #true)
(check-expect (at-or-passed-right-wall? (make-posn (+ AT-RIGHT-WALL 1) 40))
              #true)
(check-expect (at-or-passed-right-wall? (make-posn (- AT-RIGHT-WALL 1) 40))
              #false)

;(define (at-or-passed-right-wall? p) #false) ;stub

;; Template from Posn

(define (at-or-passed-right-wall? p)
  (>= (posn-x p) AT-RIGHT-WALL))

;; Worm -> Boolean
;; produces true if worm ran into left wall
(check-expect (ran-into-left-wall? (make-worm "left" (list (make-posn AT-LEFT-WALL 40))))
              #true)
(check-expect (ran-into-left-wall? (make-worm "down" (list (make-posn AT-LEFT-WALL 40))))
              #false)
(check-expect (ran-into-left-wall? (make-worm "left" (list (make-posn (+ AT-LEFT-WALL 1) 40))))
              #false)

;(define (ran-into-left-wall? w) #false) ;stub

(define (ran-into-left-wall? w)
  (and (direction-left? (worm-direction w))
       (at-or-passed-left-wall? (first (worm-segments w)))))

;; Direction -> Boolean
;; produces true if direction is left direction
(check-expect (direction-left? "left") #true)
(check-expect (direction-left? "right") #false)
(check-expect (direction-left? "up") #false)
(check-expect (direction-left? "down") #false)

;(define (direction-left? d) #false) ;stub

(define (direction-left? d)
  (string=? d "left"))

;; Posn -> Boolean
;; Produces true if posn is at or past left wall
(check-expect (at-or-passed-left-wall? (make-posn AT-LEFT-WALL 40))
              #true)
(check-expect (at-or-passed-left-wall? (make-posn (- AT-LEFT-WALL 1) 40))
              #true)
(check-expect (at-or-passed-left-wall? (make-posn (+ AT-LEFT-WALL 1) 40))
              #false)

;(define (at-or-passed-left-wall? p) #false) ;stub

;; Template from Posn

(define (at-or-passed-left-wall? p)
  (<= (posn-x p) AT-LEFT-WALL))

;; Worm -> Boolean
;; produces true if worm ran into top wall
(check-expect (ran-into-top-wall? (make-worm "up" (list (make-posn 40 AT-TOP-WALL))))
              #true)
(check-expect (ran-into-top-wall? (make-worm "up" (list (make-posn 40 (+ AT-TOP-WALL 1)))))
              #false)

;(define (ran-into-top-wall? w) #false) ;stub

(define (ran-into-top-wall? w)
  (and (direction-up? (worm-direction w))
       (at-or-passed-top-wall? (first (worm-segments w)))))

;; Direction -> Boolean
;; produces true if direction is up direction
(check-expect (direction-up? "up") #true)
(check-expect (direction-up? "right") #false)
(check-expect (direction-up? "down") #false)
(check-expect (direction-up? "left") #false)

;(define (direction-up? d) #false) ;stub

(define (direction-up? d)
  (string=? d "up"))

;; Posn -> Boolean
;; Produces true if posn is at or past top wall
(check-expect (at-or-passed-top-wall? (make-posn 40 AT-TOP-WALL))
              #true)
(check-expect (at-or-passed-top-wall? (make-posn 40 (- AT-TOP-WALL 1)))
              #true)
(check-expect (at-or-passed-top-wall? (make-posn 40 (+ AT-TOP-WALL 1)))
              #false)

;(define (at-or-passed-top-wall? p) #false) ;stub

;; Template from Posn

(define (at-or-passed-top-wall? p)
  (<= (posn-y p) AT-TOP-WALL))

;; Worm -> Boolean
;; produces true if worm ran into bottom wall
(check-expect (ran-into-bottom-wall? (make-worm "down" (list (make-posn 40 AT-BOTTOM-WALL))))
              #true)
(check-expect (ran-into-bottom-wall? (make-worm "down" (list (make-posn 40 (- AT-BOTTOM-WALL 1)))))
              #false)
(check-expect (ran-into-bottom-wall? (make-worm "up" (list (make-posn 40 AT-BOTTOM-WALL))))
              #false)

;(define (ran-into-bottom-wall? w) #false) ;stub

(define (ran-into-bottom-wall? w)
  (and (direction-down? (worm-direction w))
       (at-or-passed-bottom-wall? (first (worm-segments w)))))

;; Direction -> Boolean
;; produces true if direction is down direction
(check-expect (direction-down? "down") #true)
(check-expect (direction-down? "up") #false)
(check-expect (direction-down? "right") #false)
(check-expect (direction-down? "left") #false)

;(define (direction-down? d) #false) ;stub

(define (direction-down? d)
  (string=? d "down"))

;; Posn -> Boolean
;; Produces true if posn is at or past bottom wall
(check-expect (at-or-passed-bottom-wall? (make-posn 40 AT-BOTTOM-WALL))
              #true)
(check-expect (at-or-passed-bottom-wall? (make-posn 40 (+ AT-BOTTOM-WALL 1)))
              #true)
(check-expect (at-or-passed-bottom-wall? (make-posn 40 (- AT-BOTTOM-WALL 1)))
              #false)

;(define (at-or-passed-bottom-wall? p) #false) ;stub

(define (at-or-passed-bottom-wall? p)
  (>= (posn-y p) AT-BOTTOM-WALL))

;; Worm -> Boolean
;; produces true if worm ran into self
(check-expect (ran-into-self? (make-worm "down" (list (make-posn 50 40)
                                                      (make-posn (+ 50 WORM-SPEED) (- 40 WORM-SPEED))
                                                      (make-posn (+ (+ 50 WORM-SPEED) WORM-SPEED) (- 40 WORM-SPEED))
                                                      (make-posn (+ (+ 50 WORM-SPEED) WORM-SPEED) 40)
                                                      (make-posn (+ 50 WORM-SPEED) 40)
                                                      (make-posn 50 40))))
              #true)
(check-expect (ran-into-self? (make-worm "right" (list (make-posn 50 50)
                                                       (make-posn (- 50 WORM-SPEED) (- 50 WORM-SPEED))
                                                       (make-posn (- 50 WORM-SPEED) (- (- 50 WORM-SPEED) WORM-SPEED))
                                                       (make-posn 50 (- (- 50 WORM-SPEED) WORM-SPEED))
                                                       (make-posn 50 (- 50 WORM-SPEED))
                                                       (make-posn 50 50))))
              #true)
(check-expect (ran-into-self? (make-worm "down" (list (make-posn (- 50 WORM-SPEED) (- (- 50 WORM-SPEED) WORM-SPEED))
                                                      (make-posn 50 (- (- 50 WORM-SPEED) WORM-SPEED))
                                                      (make-posn 50 (- 50 WORM-SPEED))
                                                      (make-posn 50 50))))
              #false)

;(define (ran-into-self? w) #false) ;stub

(define (ran-into-self? w)
  (first-in-rest? (worm-segments w)))

;; NELoP -> Boolean
;; produces true if first posn exists in rest of nelop
(check-expect (first-in-rest? (list (make-posn 50 50)))
              #false)
(check-expect (first-in-rest? (list (make-posn 50 50) (make-posn 50 50)))
              #true)
(check-expect (first-in-rest? (list (make-posn 50 50) (make-posn 30 90) (make-posn 50 50)))
              #true)

;(define (first-in-rest? nelop) #false) ;stub

(define (first-in-rest? nelop)
  (member? (first nelop) (rest nelop)))

;; Worm -> Image
;; Displays the scene and a message to explain whether the program stopped because the worm hit the wall or because it ran into itself
(check-expect (render-game-over (make-worm "down" (list (make-posn (+ 50 WORM-SPEED) (- 40 WORM-SPEED))
                                                        (make-posn (+ (+ 50 WORM-SPEED) WORM-SPEED) (- 40 WORM-SPEED))
                                                        (make-posn (+ (+ 50 WORM-SPEED) WORM-SPEED) 40)
                                                        (make-posn (+ 50 WORM-SPEED) 40)
                                                        (make-posn 50 40))))
              (overlay/align "left" "bottom" (text RAN-INTO-SELF-MSG 20 "red") (render-worm (make-worm "down" (list (make-posn (+ 50 WORM-SPEED) (- 40 WORM-SPEED))
                                                                                                                    (make-posn (+ (+ 50 WORM-SPEED) WORM-SPEED) (- 40 WORM-SPEED))
                                                                                                                    (make-posn (+ (+ 50 WORM-SPEED) WORM-SPEED) 40)
                                                                                                                    (make-posn (+ 50 WORM-SPEED) 40)
                                                                                                                    (make-posn 50 40))))))
(check-expect (render-game-over (make-worm "right" (list (make-posn AT-RIGHT-WALL 40))))
              (overlay/align "left" "bottom" (text HIT-WALL-MSG 20 "red ") (render-worm (make-worm "right" (list (make-posn AT-RIGHT-WALL 40))))))

;(define (render-game-over w) (render-worm w)) ;stub

(define (render-game-over w)
  (if (ran-into-walls? w)
      (render-game-over-with-msg w HIT-WALL-MSG)
      (render-game-over-with-msg w RAN-INTO-SELF-MSG)))

;; Worm String -> Image
;; renders the game over scene with msg
(check-expect (render-game-over-with-msg (make-worm "down" (list (make-posn (+ 50 WORM-SPEED) (- 40 WORM-SPEED))
                                                                 (make-posn (+ (+ 50 WORM-SPEED) WORM-SPEED) (- 40 WORM-SPEED))
                                                                 (make-posn (+ (+ 50 WORM-SPEED) WORM-SPEED) 40)
                                                                 (make-posn (+ 50 WORM-SPEED) 40)
                                                                 (make-posn 50 40)))
                                         RAN-INTO-SELF-MSG)
              (overlay/align "left" "bottom" (text RAN-INTO-SELF-MSG 20 "red") (render-worm (make-worm "down" (list (make-posn (+ 50 WORM-SPEED) (- 40 WORM-SPEED))
                                                                                                                    (make-posn (+ (+ 50 WORM-SPEED) WORM-SPEED) (- 40 WORM-SPEED))
                                                                                                                    (make-posn (+ (+ 50 WORM-SPEED) WORM-SPEED) 40)
                                                                                                                    (make-posn (+ 50 WORM-SPEED) 40)
                                                                                                                    (make-posn 50 40))))))
(check-expect (render-game-over-with-msg (make-worm "right" (list (make-posn AT-RIGHT-WALL 40)))
                                         HIT-WALL-MSG)
              (overlay/align "left" "bottom" (text HIT-WALL-MSG 20 "red ") (render-worm (make-worm "right" (list (make-posn AT-RIGHT-WALL 40))))))

;(define (render-game-over-with-msg w s) MTSCN) ;stub

(define (render-game-over-with-msg w s)
  (overlay/align "left" "bottom" (text s 20 "red") (render-worm w)))



