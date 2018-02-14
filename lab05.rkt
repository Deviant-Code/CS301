#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301, Winter 2018
;;
;; Lab 4 - Building A Scheme Interpreter
;;
;; Jesse Ericksen
;; W01173602
;; 
;; The purpose of this program is to build a basic Scheme interpreter which
;; takes in a list expression and evaluates procedures on elements by looking up
;; respective elements from a provided environment.
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
    [else #f]))

(define (evaluate-special-form exp env)
  (define list (cdr exp))
  (cond
    [(eq? (car exp) 'if) (if (evaluate (second exp) env) (evaluate (third exp) env) (evaluate (fourth exp)))]
    [(eq? (car exp) 'cond) (cond
                             [(evaluate (car(car(cdr exp))) env) (evaluate (second(car(cdr list))) env)]
                             [else (evaluate-special-form (cons (car exp) (cdr(cdr exp))) env)])]
    [else (error "First Item Of List Is Not A Valid Special Form")]))



;; Testing functions below here.

(define list1 '(if (= 1 1) (+ 1 2) (+ 2 3)))

(define list2 '(cond
                 ((= 1 1) (+ 1 2))
                 (else (+ 2 3))))
(define list3 '(cond ((nil? ls) 1)
                        ((nil? (cdr ls)) 2)
                        ((nil? (cdr (cdr ls))) 3)
                        (else 4)))

(define envi (list
              (list '+ +)
              (list '= =)))

(define add
  (lambda (a b)
    (cond ((number? a) (+ a b))
          ((list? a) (append a b))
          (else (error "unable to add" a b)))))

(define e1  (map (lambda (x y) (list x y))
                 '(     x  y  z ls + - * cons car cdr nil list add = nil? else)
                 (list 10 20 30 (list 1 2) + - * cons car cdr '() list add = empty? #t)))