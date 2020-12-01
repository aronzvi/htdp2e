;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |5|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define TOP_HEIGHT 100)
(define TRUNK_HEIGHT TOP_HEIGHT)
(define TRUNK_WIDTH (/ TRUNK_HEIGHT 4))

(above
                 (triangle TOP_HEIGHT "solid" "green")
                 (rectangle TRUNK_WIDTH TRUNK_HEIGHT "solid" "brown"))
