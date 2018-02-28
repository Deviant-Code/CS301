#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301, Winter 2018
;;
;; Lab 6 - Building A Scheme Interpreter
;;
;; Jesse Ericksen
;; W01173602
;; 
;; The purpose of this program is to build a basic Scheme interpreter which
;; takes in a list expression and evaluates procedures on elements by looking up
;; respective elements from a provided environment.
;; *  Lab5 Adds functionality for IF and COND statements
;; ** Lab6 Adds functionality for let statements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide lookup)
(provide evaluate)
(provide special-form?)
(provide evaluate-special-form)

(define (lookup sym env)
  (cond [(null? env) (error "Symbol Not Found in Environment")]
        [(not (symbol? sym)) (error "Not A Symbol")]
        [(eq? sym (caar env)) (cadar env)]
        [else (lookup sym (cdr env))]))

(define (evaluate exp env)
  (define (mapEval exp) (evaluate exp env))
  (cond [(number? exp) exp]
        [(symbol? exp) (lookup exp env)]
        [(list? exp)
         (cond
           [(special-form? exp) (evaluate-special-form exp env)]
           [else 
            (if (procedure? (lookup (car exp) env))
                (apply (lookup(car exp) env) (cdr (map mapEval exp)))
                (error "Not A Procedure"))])]))

(define (special-form? list)
  (cond
    [(eq? (car list) 'if) #t]
    [(eq? (car list) 'cond) #t]
    [(eq? (car list) 'let) #t]
    [else #f]))

(define (evaluate-special-form exp env)
  (cond
    [(eq? (car exp) 'if) (if (evaluate (second exp) env) (evaluate (third exp) env) (evaluate (fourth exp) env))]
    [(eq? (car exp) 'cond) (if (evaluate (car(car(cdr exp))) env)
                               (evaluate (car(cdr(car(cdr exp)))) env)
                               (evaluate-special-form (cons (car exp) (cdr(cdr exp))) env))]
   [(eq? (car exp) 'let) (evaluate (third exp) (append (letF (second exp) env '()) env))]
   [else (error "First Item Of List Is Not A Valid Special Form")]))
                         

                           
(define (letF exp env newEnv)
     (if (null? exp) newEnv
            (letF (cdr exp) env (cons(cons (first(first exp)) (list (evaluate (second(first exp)) env))) newEnv))))
                                  