;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |117|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
1. (3 + 4)
is (value primitive value) which is
   (expr primitive expr) not a valid expression
   probably an invalidly formed primitive application expression

2. number?
is a primitive and is missing enclosing parentheses and at least one following expression to be a valid primitive application expression

3. (x)
is (variable)
is invalid as variable because of enclosing parentheses 
is missing at least one following expression to be a function application
