
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define sequence
  (lambda (low high stride)
   (if (> low high)
       null
       (cons low (sequence (+ low stride) high stride)))))

(define string-append-map
  (lambda (xs suffix)
    (map (lambda (s) (string-append s suffix)) xs)))

(define list-nth-mod
  (lambda  (xs n)
    (cond [(< n 0) (error "list-nth-mod: negative number")]
          [(null? xs)  (error "list-nth-mod: empty list")]
          [#t (car (list-tail xs (remainder n (length xs))))])))

(define (stream-for-n-steps s n)
  ; define helper lambda 
  ; c - counter (n -> 0)
  ; rec -> list of results (in reverse order)
  (letrec ([f (lambda (stream c res)
               (let ([pr (stream)])
                 (if (= c 0)
                     ; return normal list
                     (reverse res)
                     (f (cdr pr) (- c 1) (cons (car pr) res)))))])
      (f s n (list))))

(define funny-number-stream
  (letrec ([f (lambda (x) (if (= 0 (remainder x 5))
                               (cons (* -1 x) (lambda() (f (+ x 1))))
                               (cons x (lambda() (f (+ x 1))))
                               ))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (x) (if (= 0 (remainder x 2))
                               (cons "dog.jpg" (lambda() (f (+ x 1))))
                               (cons "dan.jpg" (lambda() (f (+ x 1))))
                               ))])
    (lambda () (f 1))))

(define (stream-add-zero s)
  (letrec ([f (lambda (stream)
               (let ([pr (stream)])
                 (lambda() (cons (cons 0 (car pr)) (stream-add-zero (cdr pr))))))])
      (f s)))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (lambda() (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (f (+ n 1)))))])
      (f 0)))
   
(define (vector-assoc v vec)
  (cond
    [(equal? 0 (vector-length vec)) #f]
    [(pair? (vector-ref vec 0)) (cond [(equal? v (car(vector-ref vec 0))) (vector-ref vec 0)]
                                      [#t (vector-assoc v (vector-drop vec 1))])]
    [#t (vector-assoc v (vector-drop vec 1))]))

(define (cached-assoc xs n)
  (letrec([cache (make-vector n #f)]
          [next 0]
          [f (lambda (x)
               ;(write cache)
               ;(write next)
               (let ([ans (vector-assoc x cache)])
                 (if ans
                     (cdr ans)
                     (let ([new-ans (assoc x xs)])
                       (begin
                         (if new-ans (vector-set! cache (remainder next n) new-ans) new-ans)
                         (if new-ans (set! next (+ 1 next)) new-ans)
                         new-ans)))))])
    f))









