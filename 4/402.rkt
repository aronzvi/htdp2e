;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |402|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; probably fits #1
;We need to traverse through the BSL-var-expr and apply subst for relevent expression. we need to use the template for BSL-var-expr to traverse and therefore ex has a dominant role
; Does not fit #2
; ad may not have the proper variables (error in this case) abd it may be in differernt order so it cannot be processed in a syncronized manner with ex
; why not #3?? "if there is no obvious connection between the two parameters..." I guess there isa connection???