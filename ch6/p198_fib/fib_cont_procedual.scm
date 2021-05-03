#lang eopl

(define fib
  (lambda (n)
    (fib/k n (end-cont))))

(define fib/k
  (lambda (n cont)
    (if (< n 2)
        (apply-cont cont 1)
        (fib/k (- n 1) (fib1-cont n cont)))))

(define (end-cont)
  (lambda (val) 
    val))

(define (fib1-cont saved_n saved_cont)
  (lambda (val) 
    (fib/k (- saved_n 2) (fib2-cont val saved_cont))))

(define (fib2-cont saved_val1 saved_cont)
  (lambda (val) 
    (apply-cont saved_cont (+ saved_val1 val))))

(define (apply-cont cont val)
  (cont val))

(eopl:pretty-print (fib 1))
(eopl:pretty-print (fib 2))
(eopl:pretty-print (fib 3))
(eopl:pretty-print (fib 4))
(eopl:pretty-print (fib 5))
(eopl:pretty-print (fib 6))
(eopl:pretty-print (fib 7))