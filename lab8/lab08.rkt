#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301, Winter 2018
;;
;; Lab 8 - Building A Scheme Interpreter
;;
;; Jesse Ericksen
;; W01173602
;; 
;; The purpose of this program is to build a basic Scheme interpreter which
;; takes in a list expression and evaluates procedures on elements by looking up
;; respective elements from a provided environment.
;; *  Lab5 Adds functionality for IF and COND statements
;; ** Lab6 Adds functionality for let statements
;; *** Lab7 Adds functionality for lambda statements
;; **** Lab8 Adds functionality for letrec statements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide lookup)
(provide evaluate)
(provide special-form?)
(provide evaluate-special-form)
(provide apply-function)
(provide apply-closure)


(define (lookup sym env)
  (cond [(null? env) (error "Symbol Not Found in Environment")]
        [(closure? sym) (toList sym)]
        [(not (symbol? sym)) (error "Not A Symbol")]
        [(eq? sym (caar env)) (cadar env)]
        [else (lookup sym (cdr env))]))

(define (evaluate exp env)
  (define (mapEval exp) (evaluate exp env))
  (cond
    [(number? exp) exp]
    [(symbol? exp) (lookup exp env)]
    [(closure? exp) (toList exp)]
    [(list? exp)
      (cond [(special-form? exp) (evaluate-special-form exp env)]
            [ else (define newE (map mapEval exp))
                   (apply-function (car newE) (cdr newE) env)])]))

(define (special-form? list)
  (cond
    [(eq? (car list) 'if) #t]
    [(eq? (car list) 'cond) #t]
    [(eq? (car list) 'let) #t]
    [(eq? (car list) 'lambda) #t]
    [(eq? (car list) 'letrec) #t]
    [else #f]))

(define (evaluate-special-form exp env)
  (cond
    [(eq? (car exp) 'if)   (if (evaluate (second exp) env) (evaluate (third exp) env) (evaluate (fourth exp) env))]
    [(eq? (car exp) 'cond) (if (evaluate (car(car(cdr exp))) env)
                               (evaluate (car(cdr(car(cdr exp)))) env)
                               (evaluate-special-form (cons (car exp) (cdr(cdr exp))) env))]
   [(eq? (car exp) 'let)    (evaluate (third exp) (append (letF (second exp) env '()) env))]
   [(eq? (car exp) 'lambda) (append(cons 'closure (cdr exp)) (list env))]
   [(eq? (car exp) 'letrec) (evaluate (third exp) (append (changeEnv (helpletrec (second exp) '() ) '() (append (helpletrec (second exp) '() )env)) env))]
   [else (error "First Item Of List Is Not A Valid Special Form")]))

(define (helpletrec exp newEnv)
  (if (empty? exp) newEnv
      (helpletrec (cdr exp)
               (cons (cons (first(first exp)) (list(closure (second(second(first exp))) (third(second(first exp))) '()))) newEnv))))

(define (changeEnv exp newExp NewEnv)
  (cond [(empty? exp) newExp]
        [else
         (set-closure-env! (second(first NewEnv)) (append newExp NewEnv))
         (changeEnv (cdr exp) (cons (first NewEnv) newExp) (cdr NewEnv))]))

(define (toList exp)
  (list 'closure (closure-vars exp) (closure-expr exp) (closure-env exp)))

(define (apply-closure exp val)
  (evaluate (third exp)
            (append (map list (second exp) val) (fourth exp))))

(define (apply-function cexp dexp env)
  (cond
    [(procedure? cexp) (apply cexp dexp)]
    [(closure? cexp) (apply-closure (toList cexp) dexp)]
    [(eq? (car cexp) 'closure) (apply-closure cexp dexp)]
    [else (error "Not A Procedure")]))

(define (letF exp env newEnv)
     (if (null? exp) newEnv
            (letF (cdr exp) env (cons(cons (first(first exp)) (list (evaluate (second(first exp)) env))) newEnv))))

(define print-closure
  (lambda (c1)
    (display (list 'closure (closure-vars c1) (closure-expr c1) (closure-env c1)))))

(struct closure (vars expr (env #:mutable)))

;;Test Environments Below ======>

(define add
  (lambda (a b)
    (cond ((and (number? a) (number? b)) (+ a b))
          ((and (list? a) (list? b)) (append a b))
          (else (error "unable to add" a b)))))

(define e1 (map (lambda (x y) (list x y))
                 '(x y z + - * cons car cdr nil = equal? < else  add list)
             (list 2 4 6 + - * cons car cdr '() = equal? < #t    add list)))









