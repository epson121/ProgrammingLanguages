;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

(define (racketlist->mupllist rlist)
  (cond [(null? rlist) (aunit)] 
        [(null? (cdr rlist)) (apair (car rlist) (aunit))]
        [#t (apair (car rlist) (racketlist->mupllist (cdr rlist)))]))

(define (mupllist->racketlist mlist)  
  (cond [(equal? (aunit) mlist) '()]
        [(equal? (apair-e2 mlist) (aunit)) (cons (apair-e1 mlist) null)]
        [#t (append (list (apair-e1 mlist)) (mupllist->racketlist (apair-e2 mlist)))]))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
   ;(displayln (format "e: ~v" e))
   ;(displayln (format "env: ~v~%" env))
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number" v1 )))]
        ; if int return expression
        [(int? e) e]
        [(aunit? e) e]
        ; fun  (nameopt formal body)
        ; (list (cons (s1, (f s1 s2 e))), (cons s2, e)) 
        [(fun? e) (closure env 
                           (fun (fun-nameopt e) 
                                 (fun-formal e) (fun-body e)))]
        ; if greater      check which is greater by calling eval-under-env for e1 and e2
        [(ifgreater? e) (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
                              [v2 (eval-under-env (ifgreater-e2 e) env)])
                              (if (> (int-num v1) (int-num v2))
                                  (eval-under-env (ifgreater-e3 e) env)
                                  (eval-under-env (ifgreater-e4 e) env)))]
        ;An mlet expression evaluates its first expression to a value v. Then it evaluates the second
        ;expression to a value, in an environment extended to map the name in the mlet expression to v.
        ; mlet (var e body)
        [(mlet? e) (eval-under-env (mlet-body e) (append env (list (cons (mlet-var e) (eval-under-env(mlet-e e) env)))))]
        [(closure? e) e]
        ;A call evaluates its first and second subexpressions to values. If the first is not a closure, it is an
        ;error. Else, it evaluates the closure’s function’s body in the closure’s environment extended to map
        ;the function’s name to the closure (unless the name field is #f) and the function’s argument-name8
        ;(i.e., the parameter name) to the result of the second subexpression.
        ; call (funexp actual)
        ; closure (env fun)
        [(call? e) 
          (let ([e1 (eval-under-env (call-funexp e) env)]
               [e2 (eval-under-env (call-actual e) env)])
           (if (closure? e1)
               ; evaluate closures functions body
               (eval-under-env (fun-body (closure-fun e1))
                               (if (fun-nameopt(closure-fun e1))
                                           ; extended to map the function’s name to the closure
                                   (append (list (cons (fun-nameopt(closure-fun e1)) e1)
                                                 ; and the function’s argument-name to the result of the second subexpression.
                                                 (cons (fun-formal(closure-fun e1)) e2)) 
                                           ;in the closure’s environment
                                           (closure-env e1))
                                   (append (list (cons (fun-formal(closure-fun e1)) e2)) 
                                           ;in the closure’s environment
                                           (closure-env e1))))
               (error "Not a closure" e1 )))]
        [(apair? e) (apair (eval-under-env(apair-e1 e) env)  (if (aunit? (apair-e2 e))
                                                                 (aunit)
                                                                 (eval-under-env(apair-e2 e) env)))]
         ;(apair-e1 e)]
        ; TODO optimize with let
        [(fst? e) (if (apair? (eval-under-env (fst-e e) env)) (apair-e1 (eval-under-env(fst-e e) env)) (error "Not a pair"))]
        [(snd? e) (if (apair? (eval-under-env (snd-e e) env)) (apair-e2 (eval-under-env(snd-e e) env)) (error "Not a pair"))]
        ;An isaunit expression evaluates its subexpression. If the result is an aunit expression, then the
        ;result for the isaunit expression is the mupl value (int 1), else the result is the mupl value
        [(isaunit? e) (if (aunit? (eval-under-env(isaunit-e e) env)) (int 1) (int 0))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;(> (int-num (eval-under-env (ifgreater-e1 e) env)) (int-num (eval-under-env(ifgreater-e2 e) env)))
                         ;return e3 if true 
                         ;(ifgreater-e3 e)
                         ;else return e4
                         ;(ifgreater-e4 e)
;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3) 
  (ifgreater (isaunit e1) (int 0) e2 e3))

; mlet (var e body)
(define (mlet* lstlst e2) 
  (if (null? (cdr lstlst))
      (mlet (car (car lstlst)) (cdr (car lstlst)) e2)
      (mlet (car (car lstlst)) (cdr (car lstlst)) (mlet* (cdr lstlst) e2))))


(define (ifeq e1 e2 e3 e4) 
  (ifgreater e1 e2 (ifgreater e2 e1 e3 e4) (ifgreater e2 e1 e4 e3)))

;; Problem 4

; Bind to the Racket variable mupl-map a mupl function that acts like map (as we used extensively
; in ML). Your function should be curried: it should take a mupl function and return a mupl
; function that takes a mupl list and applies the function to every element of the list returning a
; new mupl list. Recall a mupl list is aunit or a pair where the second component is a mupl list.

;fun map (f,xs) =
;  case xs of
;    [] => []
;    | x::xs’ => (f x)::(map(f,xs’))

(define mupl-map
  ; b -> holds mapping function
  ; c -> holds recursive function (takes in list)
  ; d -> represents list
  (fun #f "b" 
       (fun "c" "d"
            ;if list is (aunit)
           (ifaunit (var "d")
                    ; return (aunit)
                    (aunit)
                    ;else call map (b) 
                    (apair (call (var "b")
                                 ;on first element of pair
                                 (fst(var "d"))) 
                           ; and recurively call c 
                           (call (var "c") 
                                 ; with smaller list
                                 (snd (var "d"))))))))
            
                
; Bind to the Racket variable mupl-mapAddN a mupl function that takes an mupl integer i and
; returns a mupl function that takes a mupl list of mupl integers and returns a new mupl list of
; mupl integers that adds i to every element of the list. Use mupl-map (a use of mlet is given to
; you to make this easier).

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "b"
             (fun "c" "d"
                  (call (call (var "map") (fun #f "x" (add (var "x") (var "b")))) (var "d"))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
