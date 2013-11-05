#lang racket
(provide (all-defined-out))
; null cons car cdr null?
; functions
; (list e1 e2 ... en)

; sum all the numbers in the list

(define (sum xs)
  (if (null? xs)
      0
      (+ (car xs) (sum (cdr xs)))))

; append

(define (my-append xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (my-append (cdr xs) ys))))

; map
(define (my-map f xs)
  (if (null? xs)
      null
      (cons (f (car xs))
            (my-map f (cdr xs)))))