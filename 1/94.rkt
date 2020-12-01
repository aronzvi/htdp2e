;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |94|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define SCENE-WIDTH 400)
(define SCENE-HEIGHT 200)
(define SCENE (empty-scene SCENE-WIDTH SCENE-HEIGHT))
(define SCENE-MID-X (/ SCENE-WIDTH 2))

(define TANK-COLOR "black")
(define TANK-WIDTH 25)
(define TANK-HEIGHT (/ TANK-WIDTH 2))
(define TANK (rectangle TANK-WIDTH TANK-HEIGHT "solid" TANK-COLOR))
(define TANK-Y (- SCENE-HEIGHT (/ (image-height TANK) 2)))
(define TANK-SPEED 1)

(define UFO-COLOR "green")
(define UFO-WIDTH 40)
(define UFO-HEIGHT (/ UFO-WIDTH 2))
(define UFO-R (/ UFO-HEIGHT 2))
(define UFO (overlay (circle UFO-R "solid" UFO-COLOR) (rectangle UFO-WIDTH UFO-R "solid" UFO-COLOR)))
(define UFO-SPEED 1)
(define UFO-JUMP-LEN 1)

(define MISSLE-COLOR "red")
(define MISSLE-HEIGHT (/ TANK-HEIGHT 1.5))
(define MISSLE (triangle MISSLE-HEIGHT "solid" MISSLE-COLOR))
(define MISSLE-SPEED (* 2 UFO-SPEED))

(define INITIAL-SCENE (place-image UFO SCENE-MID-X 0 (place-image TANK SCENE-MID-X TANK-Y SCENE)))