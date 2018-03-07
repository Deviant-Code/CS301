#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301, Winter 2018
;;
;; Lab 7 - Building A Scheme Interpreter
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide lookup)
(provide evaluate)
(provide special-form?)
(provide evaluate-special-form)
(provide apply-function)
(provide apply-closure)

(define (lookup sym env)
  (cond [(null? env) (error "Symbol Not Found in Environment")]
        [(not (symbol? sym)) (error "Not A Symbol")]
        [(closure? sym) (toList sym)]
        [(eq? sym (caar env)) (cadar env)]
        [else (lookup sym (cdr env))]))

(define (evaluate exp env)
  (define (mapEval exp) (evaluate exp env))
  (cond
    [(number? exp) exp]
    [(symbol? exp) (lookup exp env)]
    [(list? exp)
      (if (special-form? exp) (evaluate-special-form exp env)
           (apply-function (car (map mapEval exp)) (cdr (map mapEval exp)) env))]))

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
    [(eq? (car exp) 'if) (if (evaluate (second exp) env) (evaluate (third exp) env) (evaluate (fourth exp) env))]
    [(eq? (car exp) 'cond) (if (evaluate (car(car(cdr exp))) env)
                               (evaluate (car(cdr(car(cdr exp)))) env)
                               (evaluate-special-form (cons (car exp) (cdr(cdr exp))) env))]
   [(eq? (car exp) 'let) (evaluate (third exp) (append (letF (second exp) env '()) env))]
   [(eq? (car exp) 'lambda) (append(cons 'closure (cdr exp)) (list env))]
   [(eq? (car exp) 'letrec) (evaluate (third exp) (append (changeEnv (helpletrec (second exp) '() (third exp) ) '() (helpletrec (second exp) '() (third exp) )) e1))]
   [else (error "First Item Of List Is Not A Valid Special Form")]))


(define (changeEnv exp newExp newEnv)
  (cond [(empty? exp) newExp]
        [else  (set-closure-env! (second(first exp)) newEnv)
               (changeEnv (cdr exp) (cons (first exp) newExp) newEnv)]))

(define (toList exp)
  (list 'closure (closure-vars exp) (closure-expr exp) (closure-env exp)))
              
(define (helpletrec exp newEnv lexp)
  (if (empty? exp) newEnv
     (helpletrec (cdr exp)
                 (cons (cons (first(first exp)) (list(closure (first l) lexp '()))) newEnv)
                 lexp)))

(struct closure (vars expr (env #:mutable)))

(define print-closure
  (lambda (c1)
    (display (list 'closure (closure-vars c1) (closure-expr c1) (closure-env c1)))))


  

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

(define add
  (lambda (a b)
    (cond ((and (number? a) (number? b)) (+ a b))
          ((and (list? a) (list? b)) (append a b))
          (else (error "unable to add" a b)))))

;;test environment
(define e1  (map (lambda (x y) (list x y))
                 '(     x  y  z + - * cons car cdr nil list add = equal? else)
                 (list 10 20 30 + - * cons car cdr '() list add = equal? #t)))

(define lt '(letrec ((even? (lambda (n) (if (= n 0) (= 1 1) (odd? (- n 1)))))
                           (odd? (lambda (n) (if (= n 0) (= 1 2) (even? (- n 1)))))
                           (plus (lambda (a b) (if (= a 0) b (+ 1 (plus (- a 1) b))))))))

(define l (second lt))


(define r '(letrec ((even? (lambda (n) (if (= n 0) (= 1 1) (odd? (- n 1)))))
                           (odd? (lambda (n) (if (= n 0) (= 1 2) (even? (- n 1)))))
                           (plus (lambda (a b) (if (= a 0) b (+ 1 (plus (- a 1) b))))))
                    (even? (plus 4 5))))

(define z (helpletrec l '() (third r)))

(define p1 '(letrec ((f (lambda (n) (if (< n 1) 1 (* n (f (- n 1))))))) (f 5)))

(define ep1 (append (changeEnv (helpletrec (second p1) '() (third p1) ) '() (helpletrec (second p1) '() (third p1) )) e1))



