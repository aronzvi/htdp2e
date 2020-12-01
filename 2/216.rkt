;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |216|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
(define WORM-AXIS-START "x")
(define WORM-SPEED DIAMETER)
(define TICK-RATE 0.5)
(define AT-TOP-WALL SEGMENT-R)
(define AT-BOTTOM-WALL (- HEIGHT SEGMENT-R))
(define AT-LEFT-WALL SEGMENT-R)
(define AT-RIGHT-WALL  (- WIDTH SEGMENT-R))
(define HIT-BORDER-TXT-IMG (text "worm hit border" 20 "red"))

;; Data definitions:

;; Axis is one of:
;; - "x"
;; - "y"
;; interp. The current axis
;; <examples are redundant for enumerations>

(define (fn-for-axis a)
  (cond [(string=? a "x") (...)]
        [(string=? a "y") (...)]))

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: "x"
;; - atomic disitinct: "y"

(define-struct worm [position direction axis])
;; A Worm is (make-worm Posn Number Axis)
;; interp. A worm. It's physical position on the canvas and its direction\velocity and axis
(define W1 (make-worm (make-posn 0 0) 3 "x"))      ;Worm at (0,0) travelling to the right at speed 3
(define W2 (make-worm (make-posn 0 0) -5 "x"))     ;Worm at (0,0) travelling to the left at speed 5
(define W3 (make-worm (make-posn 100 200) 4 "y")) ;Worm at (100,200)  travelling downwards at 4
(define W4 (make-worm (make-posn 100 200) -4 "y")) ;Worm at (100,200)  travelling upwards at 4

(define (fn-for-worm w)
  (... (fn-for-posn (worm-position w))   ;Posn
       (worm-direction w)
       (fn-for-axis (worm-axis w))))     ;Axis

;; Template rule used:
;; - compound: 3 fields
;; - reference: (worm-position w) is Posn
;; - reference: (worm-axis w) is Axis

;; =================
;; Functions:

;; Number -> Worm
;; start the world with (worm-main TICK-RATE)
;; 
(define (worm-main rate)
  (big-bang (make-worm (make-posn WORM-POS-START-X WORM-POS-START-Y)  ; Worm
                       WORM-SPEED
                       WORM-AXIS-START)              
    (on-tick   tock rate)                                     ; Worm -> Worm
    (to-draw   render)                                        ; Worm -> Image
    (stop-when reached-walls? render-reached-walls)           ; Worm -> ???
    (on-key    handle-key)))                                  ; Worm KeyEvent -> Worm

;; Worm -> Worm
;; Moves the by WORM-SPEED
(check-expect (tock (make-worm (make-posn 0 0)
                               WORM-SPEED
                               "x"))
              (make-worm (make-posn (+ 0 WORM-SPEED) 0)
                         WORM-SPEED
                         "x"))
(check-expect (tock (make-worm (make-posn 0 0)
                               WORM-SPEED
                               "y"))
              (make-worm (make-posn 0 (+ 0 WORM-SPEED))
                         WORM-SPEED
                         "y"))
(check-expect (tock (make-worm (make-posn 50 70)
                               (- WORM-SPEED)
                               "x"))
              (make-worm (make-posn (+ 50 (- WORM-SPEED)) 70)
                         (- WORM-SPEED)
                         "x"))
(check-expect (tock (make-worm (make-posn 50 70)
                               (- WORM-SPEED)
                               "y"))
              (make-worm (make-posn 50 (+ 70 (- WORM-SPEED)))
                         (- WORM-SPEED)
                         "y"))
;(define (tock w) w) ;stub

(define (tock w)
  (make-worm
   (posn+ (worm-position w) (worm-axis w) (worm-direction w))
   (worm-direction w)
   (worm-axis w)))

;; Posn Axis Number -> Posn
;; adds n to given axis of given posn
(check-expect (posn+ (make-posn 0 0) "x" 10)
              (make-posn 10 0))
(check-expect (posn+ (make-posn 0 0) "y" 10)
              (make-posn 0 10))
(check-expect (posn+ (make-posn 0 0) "x" -10)
              (make-posn -10 0))
(check-expect (posn+ (make-posn 0 0) "y" -10)
              (make-posn 0 -10))

;(define (posn+ p a n) p) ;stub

(define (posn+ p a n)
  (cond [(string=? a "x") (posn-x+ n p)]
        [(string=? a "y") (posn-y+ n p)]))

;; Posn Number -> Posn
;; adds n to x of given posn
(check-expect (posn-x+ 10 (make-posn 0 0))
              (make-posn 10 0))
(check-expect (posn-x+ -10 (make-posn 0 0))
              (make-posn -10 0))

;(define (posn-x+ n p) p) ;stub

;; template from Posn

(define (posn-x+ n p)
  (make-posn (+ n (posn-x p))
             (posn-y p)))

;; Posn Number -> Posn
;; adds n to y of given posn
(check-expect (posn-y+ 10 (make-posn 0 0))
              (make-posn 0 10))
(check-expect (posn-y+ -10 (make-posn 0 0))
              (make-posn 0 -10))

;(define (posn-y+ n p) p) ;stub

;; template from Posn

(define (posn-y+ n p)
  (make-posn (posn-x p)
             (+ n (posn-y p))))

;; Worm -> Image
;; render the Worm on MTSCN 
(check-expect (render (make-worm (make-posn (/ WIDTH 2) (/ HEIGHT 2))
                                 WORM-SPEED
                                 "y"))
              (place-image SEGMENT (/ WIDTH 2) (/ HEIGHT 2) MTSCN))

;(define (render w) empty-image) ;stub

(define (render w)
  (place-image SEGMENT
               (posn-x (worm-position w))
               (posn-y (worm-position w))
               MTSCN))

;; Worm KeyEvent -> Worm
;; handle the arrow keys
(check-expect (handle-key (make-worm (make-posn 0 0)
                                     WORM-SPEED
                                     "x")
                          "up")
              (make-worm (make-posn 0 0)
                         (- WORM-SPEED)
                         "y"))
(check-expect (handle-key (make-worm (make-posn 0 0)
                                     (- WORM-SPEED)
                                     "x")
                          "up")
              (make-worm (make-posn 0 0)
                         (- WORM-SPEED)
                         "y"))
(check-expect (handle-key (make-worm (make-posn 0 0)
                                     WORM-SPEED
                                     "y")
                          "up")
              (make-worm (make-posn 0 0)
                         (- WORM-SPEED)
                         "y"))
(check-expect (handle-key (make-worm (make-posn 0 0)
                                     WORM-SPEED
                                     "x")
                          "down")
              (make-worm (make-posn 0 0)
                         WORM-SPEED
                         "y"))
(check-expect (handle-key (make-worm (make-posn 0 0)
                                     (- WORM-SPEED)
                                     "x")
                          "down")
              (make-worm (make-posn 0 0)
                         WORM-SPEED
                         "y"))
(check-expect (handle-key (make-worm (make-posn 0 0)
                                     (- WORM-SPEED)
                                     "y")
                          "left")
              (make-worm (make-posn 0 0)
                         (- WORM-SPEED)
                         "x"))
(check-expect (handle-key (make-worm (make-posn 0 0)
                                     WORM-SPEED
                                     "y")
                          "left")
              (make-worm (make-posn 0 0)
                         (- WORM-SPEED)
                         "x"))
(check-expect (handle-key (make-worm (make-posn 0 0)
                                     WORM-SPEED
                                     "y")
                          "right")
              (make-worm (make-posn 0 0)
                         WORM-SPEED
                         "x"))
(check-expect (handle-key (make-worm (make-posn 0 0)
                                     (- WORM-SPEED)
                                     "y")
                          "right")
              (make-worm (make-posn 0 0)
                         WORM-SPEED
                         "x"))
(check-expect (handle-key (make-worm (make-posn 0 0)
                                     WORM-SPEED
                                     "y")
                          "x")
              (make-worm (make-posn 0 0)
                         WORM-SPEED
                         "y"))

;(define (handle-key w ke) w) ;stub

#;
(define (handle-key w ke)
  (cond [(key=? "up" ke) (... w)]
        [(key=? "down" ke) (... w)]
        [(key=? "left" ke) (... w)]
        [(key=? "right" ke) (... w)]
        [else
         (...)]))
;; Template formed using the large enumeration special case

(define (handle-key w ke)
  (cond [(key=? "up" ke) (set-worm-direction-up w)]
        [(key=? "down" ke) (set-worm-direction-down w)]
        [(key=? "left" ke) (set-worm-direction-left w)]
        [(key=? "right" ke) (set-worm-direction-right w)]
        [else w]))

;; Worm -> Worm
;; sets the worm's direction to up
(check-expect (set-worm-direction-up (make-worm (make-posn 0 0)
                                                WORM-SPEED
                                                "x"))
              (make-worm (make-posn 0 0)
                         (- WORM-SPEED)
                         "y"))
(check-expect (set-worm-direction-up (make-worm (make-posn 0 0)
                                                (- WORM-SPEED)
                                                "x"))
              (make-worm (make-posn 0 0)
                         (- WORM-SPEED)
                         "y"))
(check-expect (set-worm-direction-up (make-worm (make-posn 0 0)
                                                WORM-SPEED
                                                "y"))
              (make-worm (make-posn 0 0)
                         (- WORM-SPEED)
                         "y"))
(check-expect (set-worm-direction-up (make-worm (make-posn 0 0)
                                                (- WORM-SPEED)
                                                "y"))
              (make-worm (make-posn 0 0)
                         (- WORM-SPEED)
                         "y"))

;(define (set-worm-direction-upw) w) ;stub

(define (set-worm-direction-up w)
  (make-worm (worm-position w)  
             (set-negative (worm-direction w))
             "y"))

;; Number -> Number
;; given +-n produces -n. n!=0
(check-expect (set-negative 5) -5)
(check-expect (set-negative -5) -5)

;(define (set-negative n) n) ;stub

#;
(define (set-negative n) ;Template
  (... n))

;; Template rules used:
;; atomic non-distinct: Number

(define (set-negative n) 
  (- (abs n)))

;; Worm -> Worm
;; sets the worm's direction to down
(check-expect (set-worm-direction-down (make-worm (make-posn 0 0)
                                                  WORM-SPEED
                                                  "x"))
              (make-worm (make-posn 0 0)
                         WORM-SPEED
                         "y"))
(check-expect (set-worm-direction-down (make-worm (make-posn 0 0)
                                                  (- WORM-SPEED)
                                                  "x"))
              (make-worm (make-posn 0 0)
                         WORM-SPEED
                         "y"))
(check-expect (set-worm-direction-down (make-worm (make-posn 0 0)
                                                  WORM-SPEED
                                                  "y"))
              (make-worm (make-posn 0 0)
                         WORM-SPEED
                         "y"))
(check-expect (set-worm-direction-down (make-worm (make-posn 0 0)
                                                  (- WORM-SPEED)
                                                  "y"))
              (make-worm (make-posn 0 0)
                         WORM-SPEED
                         "y"))

; (define (set-worm-direction-down w) w) ;stub

(define (set-worm-direction-down w)
  (make-worm (worm-position w)  
             (set-positive (worm-direction w))
             "y"))

;; Number -> Number
;; given +-n produces n.
(check-expect (set-positive 5) 5)
(check-expect (set-positive -5) 5)

;(define (set-positive n) n) ;stub

#;
(define (set-positive n) ;Template
  (... n))

;; Template rules used:
;; atomic non-distinct: Number

(define (set-positive n) 
  (abs n))

;; Worm -> Worm
;; sets the worm's direction to left
(check-expect (set-worm-direction-left (make-worm (make-posn 0 0)
                                                  WORM-SPEED
                                                  "x"))
              (make-worm (make-posn 0 0)
                         (- WORM-SPEED)
                         "x"))
(check-expect (set-worm-direction-left (make-worm (make-posn 0 0)
                                                  (- WORM-SPEED)
                                                  "x"))
              (make-worm (make-posn 0 0)
                         (- WORM-SPEED)
                         "x"))
(check-expect (set-worm-direction-left (make-worm (make-posn 0 0)
                                                  WORM-SPEED
                                                  "y"))
              (make-worm (make-posn 0 0)
                         (- WORM-SPEED)
                         "x"))
(check-expect (set-worm-direction-left (make-worm (make-posn 0 0)
                                                  (- WORM-SPEED)
                                                  "y"))
              (make-worm (make-posn 0 0)
                         (- WORM-SPEED)
                         "x"))

;(define (set-worm-direction-left w) w) ;stub

(define (set-worm-direction-left w)
  (make-worm (worm-position w)  
             (set-negative (worm-direction w))
             "x"))

;; Worm -> Worm
;; sets the worm's direction to right
(check-expect (set-worm-direction-right (make-worm (make-posn 0 0)
                                                   WORM-SPEED
                                                   "x"))
              (make-worm (make-posn 0 0)
                         WORM-SPEED
                         "x"))
(check-expect (set-worm-direction-right (make-worm (make-posn 0 0)
                                                   (- WORM-SPEED)
                                                   "x"))
              (make-worm (make-posn 0 0)
                         WORM-SPEED
                         "x"))
(check-expect (set-worm-direction-right (make-worm (make-posn 0 0)
                                                   WORM-SPEED
                                                   "y"))
              (make-worm (make-posn 0 0)
                         WORM-SPEED
                         "x"))
(check-expect (set-worm-direction-right (make-worm (make-posn 0 0)
                                                   (- WORM-SPEED)
                                                   "y"))
              (make-worm (make-posn 0 0)
                         WORM-SPEED
                         "x"))

;(define (set-worm-direction-right w) w) ;stub

(define (set-worm-direction-right w)
  (make-worm (worm-position w)  
             (set-positive (worm-direction w))
             "x"))

;; Worm -> Boolean
;; produces true if worm has passed the walls of the world
(check-expect (reached-walls? (make-worm (make-posn AT-RIGHT-WALL 100) ; travelling to the right and touching right wall
                                         WORM-SPEED
                                         "x"))
              #false)
(check-expect (reached-walls? (make-worm (make-posn (+ AT-RIGHT-WALL 1) 100) ;travelling to the right and just passed right wall
                                         WORM-SPEED
                                         "x"))
              #true)
(check-expect (reached-walls? (make-worm (make-posn (/ WIDTH 2) 100) ; middle width
                                         WORM-SPEED
                                         "x"))
              #false)
(check-expect (reached-walls? (make-worm (make-posn AT-LEFT-WALL 100) ; travelling to the left touching left wall
                                         (- WORM-SPEED)
                                         "x"))
              #false)
(check-expect (reached-walls? (make-worm (make-posn (- AT-LEFT-WALL 1) 100) ; travelling to the left just passed left wall
                                         (- WORM-SPEED)
                                         "x"))
              #true)
(check-expect (reached-walls? (make-worm (make-posn 100 AT-TOP-WALL) ; touching top
                                         (- WORM-SPEED)
                                         "x"))
              #false)
(check-expect (reached-walls? (make-worm (make-posn 100 (- AT-TOP-WALL 1)) ; just passed top
                                         (- WORM-SPEED)
                                         "x"))
              #true)
(check-expect (reached-walls? (make-worm (make-posn 100 (/ HEIGHT 2)) ; middle height
                                         (- WORM-SPEED)
                                         "x"))
              #false)
(check-expect (reached-walls? (make-worm (make-posn 100 AT-BOTTOM-WALL) ; touching bottom
                                         (- WORM-SPEED)
                                         "x"))
              #false)
(check-expect (reached-walls? (make-worm (make-posn 100 (+ AT-BOTTOM-WALL 1)) ; just passed bottom
                                         (- WORM-SPEED)
                                         "x"))
              #true)

;(define (reached-walls? w) #false) ;stub

(define (reached-walls? w)
  (or (passed-top-wall? w)
      (passed-bottom-wall? w)
      (passed-left-wall? w)
      (passed-right-wall? w)))

;; Worm -> Boolean
;; produces true if worm passed top wall
(check-expect (passed-top-wall? (make-worm (make-posn 100 AT-TOP-WALL) ; touching top
                                           (- WORM-SPEED)
                                           "x"))
              #false)
(check-expect (passed-top-wall? (make-worm (make-posn 100 (- AT-TOP-WALL 1)) ; just passed top
                                           (- WORM-SPEED)
                                           "x"))
              #true)
(check-expect (passed-top-wall? (make-worm (make-posn 100 (/ HEIGHT 2)) ; middle height
                                           (- WORM-SPEED)
                                           "x"))
              #false)

;(define (passed-top-wall? w) #false) ;stub

(define (passed-top-wall? w)
  (posn-y<? (worm-position w) AT-TOP-WALL))

;; Posn Number -> Boolean
;; produces true if y of posn is smaller than n
(check-expect (posn-y<? (make-posn 3 5) 10) #true)
(check-expect (posn-y<? (make-posn 3 5) 4) #false)
(check-expect (posn-y<? (make-posn 3 5) 5) #false)

;(define (posn-y<? p n) #false)

;; Template from Posn

(define (posn-y<? p n)
  (< (posn-y p) n))
  
;; Worm -> Boolean
;; produces true if worm passed bottom wall
(check-expect (passed-bottom-wall? (make-worm (make-posn 100 AT-BOTTOM-WALL) ; touching bottom
                                              (- WORM-SPEED)
                                              "x"))
              #false)
(check-expect (passed-bottom-wall? (make-worm (make-posn 100 (+ AT-BOTTOM-WALL 1)) ; just passed bottom
                                              (- WORM-SPEED)
                                              "x"))
              #true)
(check-expect (passed-bottom-wall? (make-worm (make-posn 100 (/ HEIGHT 2)) ; middle height
                                              (- WORM-SPEED)
                                              "x"))
              #false)

;(define (passed-bottom-wall? w) #false) ;stub

(define (passed-bottom-wall? w)
  (posn-y>? (worm-position w) AT-BOTTOM-WALL))

;; Posn Number -> Boolean
;; produces true if y of posn is greater than n
(check-expect (posn-y>? (make-posn 3 5) 4) #true)
(check-expect (posn-y>? (make-posn 3 5) 6) #false)
(check-expect (posn-y>? (make-posn 3 5) 5) #false)

;(define (posn-y>? p n) #false)

;; Template from Posn

(define (posn-y>? p n)
  (> (posn-y p) n))

;; Worm -> Boolean
;; produces true if worm passed left wall
(check-expect (passed-left-wall? (make-worm (make-posn (/ WIDTH 2) 100) ; middle width
                                            WORM-SPEED
                                            "x"))
              #false)
(check-expect (passed-left-wall? (make-worm (make-posn AT-LEFT-WALL 100) ;touching left wall
                                            (- WORM-SPEED)
                                            "x"))
              #false)
(check-expect (passed-left-wall? (make-worm (make-posn (- AT-LEFT-WALL 1) 100) ;just passed left wall
                                            (- WORM-SPEED)
                                            "x"))
              #true)

;(define (passed-left-wall? w) #false) ;stub

(define (passed-left-wall? w)
  (posn-x<? (worm-position w) AT-LEFT-WALL))

;; Posn Number -> Boolean
;; produces true if x of posn is smaller than n
(check-expect (posn-x<? (make-posn 3 5) 4) #true)
(check-expect (posn-x<? (make-posn 3 5) 2) #false)
(check-expect (posn-x<? (make-posn 3 5) 3) #false)

;(define (posn-x<? p n) #false)

;; Template from Posn

(define (posn-x<? p n)
  (< (posn-x p) n))

;; Worm -> Boolean
;; produces true if worm passed right wall
(check-expect (passed-right-wall? (make-worm (make-posn (/ WIDTH 2) 100) ; middle width
                                             WORM-SPEED
                                             "x"))
              #false)
(check-expect (passed-right-wall? (make-worm (make-posn AT-RIGHT-WALL 100) ;touching right wall
                                             (- WORM-SPEED)
                                             "x"))
              #false)
(check-expect (passed-right-wall? (make-worm (make-posn (+ AT-RIGHT-WALL 1) 100) ;just passed right wall
                                             (- WORM-SPEED)
                                             "x"))
              #true)

;(define (passed-right-wall? w) #false) ;stub

(define (passed-right-wall? w)
  (posn-x>? (worm-position w) AT-RIGHT-WALL))

;; Posn Number -> Boolean
;; produces true if x of posn is smaller than n
(check-expect (posn-x>? (make-posn 5 5) 4) #true)
(check-expect (posn-x>? (make-posn 3 5) 4) #false)
(check-expect (posn-x>? (make-posn 3 5) 3) #false)

;(define (posn-x>? p n) #false)

;; Template from Posn

(define (posn-x>? p n)
  (> (posn-x p) n))

;; Worm -> Image
;; render the final scene with the text "worm hit border" in the lower left of the world scene
(check-expect (render-reached-walls (make-worm (make-posn (+ AT-RIGHT-WALL 1) 100) ;just passed right wall
                                               (- WORM-SPEED)
                                               "x"))
              (overlay/align "left" "bottom" HIT-BORDER-TXT-IMG (render (make-worm (make-posn (+ AT-RIGHT-WALL 1) 100) ;just passed right wall
                                                                                   (- WORM-SPEED)
                                                                                   "x"))))
              
;(define (render-reached-walls w) empty-image) ;stub

(define (render-reached-walls w)
  (overlay/align "left" "bottom" HIT-BORDER-TXT-IMG (render w)))