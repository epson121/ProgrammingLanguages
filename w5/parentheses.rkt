#lang racket
; parentheses workout

(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))

; error with 1 being in parentheses
(define (fact1 n) (if (= n 0) (1) (* n (fact1 (- n 1)))))

; calling fact recursiveliy, fails for n = 0
(define (fact2 n) (if (= n 0) (1) (* n (fact (- n 1)))))

; parentheses missing in last call to fact4
(define (fact4 n) (if (= n 0) 1 (* n fact4 (- n 1))))

(define (fact5 n) (if (= n 0) 1 (* n (fact5 (- n 1)))))