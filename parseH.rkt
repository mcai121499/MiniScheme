#lang racket

(provide new-lit-exp lit-exp? lit-exp-num new-var-ref var-ref? var-ref-symbol parse new-app-exp app-exp? app-exp-proc app-exp-args if-exp if-exp? if-exp-cond if-exp-t if-exp-f new-let-exp let-exp? let-exp-syms let-exp-vals let-exp-body
         new-lambda-exp lambda-exp? get-lambda-params get-lambda-exp new-closure closure? closure-params closure-body closure-env assign-exp assign-exp? get-assign-symbol
         get-assign-value new-begin-exp begin-exp? get-begin-exps)

(define new-lit-exp
  (lambda (input)
    (list 'lit-exp input)))

(define lit-exp?
  (lambda (input)
    (cond
      [(equal? (car input) 'lit-exp)  #t]
      {else #f})))

(define lit-exp-num
  (lambda (lit-exp)
    (cond
      [(lit-exp? lit-exp) (cadr lit-exp)]
      [else (error 'lit-exp-num "Invalid lit-exp: ~s" lit-exp)])))

(define new-var-ref
  (lambda (input)
    (list 'var-ref input)))

(define var-ref?
  (lambda (input)
    (cond
      [(equal? (car input) 'var-ref) #t]
      [else #f])))

(define var-ref-symbol
  (lambda (input)
    (cond
      [(var-ref? input) (cadr input)]
      [else (error 'var-ref-symbol "Invalid var-ref: ~s" input)])))

(define new-app-exp
  (lambda (proc args)
    (list 'app-exp proc args)))

(define app-exp?
  (lambda (input)
    (cond
      [(list? input)(equal? (car input) 'app-exp)]
      [else (error 'app-exp? "Bad input: ~s" input)])))

(define T
  (new-app-exp + '(1 1)))

(define app-exp-proc
  (lambda (exp)
    (cond
      [(app-exp? exp) (cadr exp)]
      [else (error 'get "Bad input: ~s" exp)])))

(define app-exp-args
  (lambda (exp)
    (cond
      [(app-exp? exp) (caddr exp) ]
      [else (error 'get "Bad input: ~s" exp)])))

(define if-exp
  (lambda (cond tru fal)
    (list 'if cond tru fal)))

(define if-exp?
  (lambda (input)
    (cond
      [(list input)(equal? 'if (car input))]
      [else (error 'if-exp "Bad input: ~s" input)])))

(define if-exp-cond
  (lambda (input)
    (cond
      [(if-exp? input) (cadr input)]
      [else (error 'get "Bad input: ~s" input)])))

(define if-exp-t
  (lambda (input)
    (cond
      [(if-exp? input) (caddr input)]
      [else (error 'get "Bad input: ~s" input)])))

(define if-exp-f
  (lambda (input)
    (cond
      [(if-exp? input) (cadddr input)]
      [else (error 'get "Bad input: ~s" input)])))

(define new-let-exp
  (lambda (let-syms let-binds body)
    (list 'let let-syms let-binds body)))

(define let-exp-syms
  (lambda (input)
    (cond
      [(let-exp? input)   (cadr input)]
      [else (error 'get "Bad input: ~s" input)])))

(define let-exp-vals
  (lambda (input)
    (cond
      [(let-exp? input)  (caddr input)]
      [else (error 'get "Bad input: ~s" input)])))

(define let-exp-body
  (lambda (input)
    (cond
      [(let-exp? input) (cadddr input)]
      [else (error 'get "Bad input: ~s" input)])))

(define let-exp?
  (lambda (input)
    (cond
      [(list? input) (eq? 'let (car input))]
      [else (error 'get "Bad input: ~s" input)])))

(define new-lambda-exp
  (lambda (params exp)
    (list 'lambda params exp)))

(define lambda-exp?
  (lambda (input)
    (cond
      [(list? input) (eq? 'lambda (car input))]
      [else (error 'identifier "Bad input: ~s" input)])))

(define get-lambda-params
  (lambda (input)
    (cond
      [(lambda-exp? input) (cadr input)]
      [else (error 'get "Bad input: ~s" input)])))

(define get-lambda-exp
  (lambda (input)
    (cond
      [(lambda-exp? input) (caddr input)]
      [else (error 'get "Bad input: ~s" input)])))

(define new-closure
  (lambda (params body env)
    (list 'closure params body env )))

(define closure?
  (lambda (input)
    (cond
      [(list? input) (eq? 'closure (car input))]
      [else (error 'get "Bad input: ~s" input)])))

(define closure-params
  (lambda (input)
    (cond
      [(closure? input) (cadr input)]
      [else (error 'get "Bad input: ~s" input)])))

(define closure-body
  (lambda (input)
    (cond
      [(closure? input) (caddr input)]
      [else (error 'get "Bad input: ~s" input)])))

(define closure-env
  (lambda (input)
    (cond
      [(closure? input) (cadddr input)]
      [else (error 'get "Bad input: ~s" input)])))

(define assign-exp
  (lambda (symbol exp)
    (list 'set! symbol exp)))

(define assign-exp?
  (lambda (input)
    (cond
      [(list? input) (eq? 'set! (car input))]
      [else (error 'identifier "Bad input: ~s" input)])))

(define get-assign-symbol
  (lambda (input)
    (cond
      [(assign-exp? input) (cadr input)]
      [else (error 'getter "Bad input: ~s" input)])))

(define get-assign-value
  (lambda (input)
    (cond
      [(assign-exp? input) (caddr input)]
      [else (error 'getter "Bad input: ~s" input)])))

(define new-begin-exp
  (lambda (listofexps)
    (list 'begin-exp listofexps)))

(define begin-exp?
  (lambda (input)
    (cond
      [(list? input) (eq? 'begin-exp (car input))]
      [else (error 'identifier "Bad input: ~s" input)])))

(define get-begin-exps
  (lambda (input)
    (cond
      [(begin-exp? input) (cadr input)]
      [else (error 'identifier "Bad input: ~s" input)])))


(define parse
  (lambda (input)
    (cond
      [(number? input) (new-lit-exp input)]
      [(symbol? input) (new-var-ref input)]
      [(not (pair? input)) (error 'parse "Invalid syntax ~s" input)]
      [(equal? 'if (car input)) (if-exp (parse (cadr input)) (parse (caddr input)) (parse (cadddr input)))]
      [(equal? 'lambda (car input)) (new-lambda-exp (cadr input) (parse (caddr input)))]
      [(equal? 'let (car input)) (new-let-exp  (map car (cadr input)) (map parse (map cadr (cadr input))) (parse (caddr input))) ]
      [(equal? 'set! (car input)) (assign-exp (cadr input) (parse (caddr input)))]
      [(equal? 'begin (car input)) (new-begin-exp (map parse (cdr input)))]
      [(equal? 'letrec (car input)) (let ([new-syms (map (lambda (x) (gensym)) (map car (cadr input)))])(new-let-exp (map car (cadr input)) (map parse (map (lambda (x) 0) (map car (cadr input))))
                 (new-let-exp new-syms (map parse (map cadr (cadr input)))
                              (new-begin-exp (append (map (lambda (x y) (assign-exp x y)) (map car (cadr input)) (map parse new-syms)) (list (parse (caddr input))))))))]
      [else (new-app-exp (parse (car input)) (map parse (cdr input)))])))


