#lang racket
(define a 3)

(define b (+ a 2))

; using keyword lambda and * function with only 2 parameters at a time
(define cube1 
  (lambda (x)
    (* x (* x x))))

; using lambda and * function with multiple parameters
(define cube2
  (lambda (x)
    (* x x x)))

; syntactic sugar, without lambda keyword
(define (cube3 x)
  (* x x x))

(define pow
  (lambda (x y)
         (if (= y 0)
         1
         (* x (pow x (- y 1))))))
(