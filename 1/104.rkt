;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |104|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct automobile (passengers license-plate mpg))
;; Autombile is (make-automobile Natural String Number)
;; Interp. an automobile with
;;   passengers - the number of passengers that it can carry
;;   license-plate - the license plate
;;   mpg - fuel consumption (miles per gallon)

(define A1 (make-automobile 4 "Y0-M4m4a" 60))

#;
(define (fn-for-automobile a)
  (... (automobile-passengers a)
       (automobile-license-plate a)
       (automobile-mpg a)))

;; Template rules used:
;; - compound: 3 fields

(define-struct van (passengers license-plate mpg))
;; Van is (make-automobile Natural String Number)
;; Interp. a van with
;;   passengers - the number of passengers that it can carry
;;   license-plate - the license plate
;;   mpg - fuel consumption (miles per gallon)

(define V1 (make-van 8 "Y0-Y0-Yo" 80))

#;
(define (fn-for-van v)
  (... (van-passengers v)
       (van-license-plate v)
       (van-mpg v)))

;; Template rules used:
;; - compound: 3 fields

(define-struct bus (passengers license-plate mpg))
;; Bus is (make-bus Natural String Number)
;; Interp. a bus with
;;   passengers - the number of passengers that it can carry
;;   license-plate - the license plate
;;   mpg - fuel consumption (miles per gallon)

(define B1 (make-bus 30 "Hola-M4m4y" 80))

#;
(define (fn-for-bus b)
  (... (bus-passengers b)
       (bus-license-plate b)
       (bus-mpg b)))

;; Template rules used:
;; - compound: 3 fields

(define-struct suv (passengers license-plate mpg))
;; Suv is (make-automobile Natural String Number)
;; Interp. an Suv with
;;   passengers - the number of passengers that it can carry
;;   license-plate - the license plate
;;   mpg - fuel consumption (miles per gallon)

(define S1 (make-suv 6 "Coc0-moko" 50))

#;
(define (fn-for-automobile s)
  (... (aotomobile-passengers s)
       (automobile-license-plate s)
       (automobile-mpg s)))

;; Template rules used:
;; - compound: 3 fields

;; Vehicle is one of:
;; - Automobile
;; - Van
;; - Bus
;; - Suv
;; interp. A vehicle

(define VH1 (make-automobile 2 "RRRR-SDS4" 50))
(define VH2 (make-van 10 "FuCC-U-2" 70))
(define VH3 (make-bus 40 "OOF-DD-4" 80))
(define VH4 (make-van 8 "CKAA-D-2" 40))

(define (fn-for-vehicle v)
  (cond [(automobile? v) (... (fn-for-automobile v))]  ;Automobile
        [(van? v) (... (fn-for-van v))]                ;Van
        [(bus? v) (... (fn-for-bus v))]                ;Bus
        [(suv? v) (... (fn-for-suv v))]))              ;Suv

;; Template rules used:
;; one of: 4 cases
;; - reference: v is Automobile
;; - reference: v is Van
;; - reference: v is Bus
;; - reference: v is Suv


;; VehicleType is one of:
;; - "Automobile"
;; - "Van"
;; - "Bus"
;; - "Suv"
;; interp. A vehicle type

(define VT-A "Automobile")
(define VT-V "Van")
(define VT-B "Bus")
(define VT-S "Suv")

(define (fn-for-vehicle-type vt)
  (cond [(string=? vt "Automobile") ...]
        [(string=? vt "Van") ...]
        [(string=? vt "Bus") ...]
        [(string=? vt "Suv") ...]))

(define-struct vehicle.v2 (type passengers license-plate mpg))
; A Vehicle.v2 is (make-vehicle.v2 VehicleType Natural String Number)
; Interp. a vehicle with:
;  - type: the type of vehicle
;  - license-plate - the license plate number
;  - mpg - fuel consumption (miles per gallon)

(define V-A.v2 (make-vehicle.v2 "Automobile" 2 "RRRR-SDS4" 50))
(define V-B.v2 (make-vehicle.v2 "Bus" 40 "OOF-DD-4" 80))

(define (fn-for-vehicle.v2 v)
  (...
   (vehicle.v2-type v)
   (vehicle.v2-passengers v)
   (vehicle.v2-license-plate v)
   (vehicle.v2-mpg v)))

