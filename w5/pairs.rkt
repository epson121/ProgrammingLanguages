#lang racket
(provide (all-defined-out))

(define pr (cons 1 (cons #t "hi"))) ; (1, (true, "hi"))
(define lst (cons 1 (cons #t (cons "hi" null))))
(define hi (cdr (cdr pr)))
(define hi-again (car (cdr (cdr lst))))
(define hi-again-shorter (caddr lst))