;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |350|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; the parse-sl definition is placing restrictions on the exact type of SL that it can process
; - only list of length 3
; - Only Atom of type number for second and third items
; - Only symbol of type '+ or '* for first item

; We would usually define the data that we wish the function to work on with the restrictions included