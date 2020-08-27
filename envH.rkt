#lang racket

(require "parseH.rkt")

(provide environment? empty-env? extended-env? empty-env extended-env syms vals old-env lookup init-env new-prim-proc prim-proc-symbol? prim-proc? primitive-operators)


; datatype definition

(define environment? (lambda (e) (or (empty-env? e) (extended-env? e))))

(define empty-env? (lambda (e)
                     (cond
                       [(list? e) (eq? (car e) 'empty-env)]
                       [else #f])))

(define extended-env? (lambda (e)
                        (cond
                          [(list? e) (eq? (car e) 'extended-env)]
                          [else #f])))

(define empty-env (lambda ()
                    (list 'empty-env)))

(define extended-env (lambda (syms vals old-env)
                       (list 'extended-env syms (map box vals) old-env)))

(define syms (lambda (env)
               (cond
                 [(extended-env? env) (cadr env)]
                 [else (error 'syms "bad environment")])))

(define vals (lambda (env)
               (cond
                 [(extended-env? env) (caddr env)]
                 [else (error 'vals "bad environment")])))

(define old-env (lambda (env)
               (cond
                 [(extended-env? env) (cadddr env)]
                 [else (error 'old-env "bad environment")])))

(define the-empty-env (empty-env))

(define new-prim-proc
  (lambda (input)
    (list 'prim-proc input)))

(define prim-proc-symbol?
  (lambda (input)
    (cond
      [(prim-proc? input) (cadr input)])))

(define prim-proc?
  (lambda (input)
    (cond
      [(equal? (car input) 'prim-proc) #t]
      [else #f])))

(define primitive-operators '(+ - * / add1 sub1 minus list build first rest empty? build equals? lt? gt? leq? geq?))


(define EnvA (extended-env '(x y) '(1 2) the-empty-env))
(define EnvB (extended-env '(x z) '(5 7) EnvA))

(define index
  (lambda (x i l)
    (cond
      [(null? l) -1]
      [(eq? x (car l)) i]
      [else (index x (+ i 1) (cdr l))])))

(define find
  (lambda (i l)
    (cond
      [(= i -1) 'none]
      [(null? l) 'none]
      [(eq? i 0) (car l)]
      [else (find (- i 1) (cdr l))])))

(define lookup
  (lambda (env x)
    (cond
     [(extended-env? env) (let ([e (find (index x 0 (syms env)) (vals env))])
                            (if (eq? e 'none)
                                (lookup (old-env env) x) e))]
     [else (error 'lookup "No binding for ~s" x)])))

(define init-env (extended-env primitive-operators

                 (map new-prim-proc primitive-operators)

                              (extended-env '(x y True False nil) '(10 23 999 0 ()) (empty-env))))



