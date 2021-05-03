#lang eopl

(define fib
  (lambda (n)
    (fib/k n (end-cont))))

(define fib/k
  (lambda (n cont)
    (if (< n 2)
        (apply-cont cont 1)
        (fib/k (- n 1) (fib1-cont n cont)))))

(define-datatype continuation continuation?
  (end-cont)
  (fib1-cont
    (n integer?)
    (cont continuation?))
  (fib2-cont
    (val1 integer?)
    (cont continuation?))
)

(define apply-cont
  (lambda (cont val) 
    (cases continuation cont 
      (end-cont () 
        val) 
      (fib1-cont (saved_n saved_cont) 
        (fib/k (- saved_n 2) (fib2-cont val saved_cont)))
      (fib2-cont (saved_val1 saved_cont)
        (apply-cont saved_cont (+ saved_val1 val)))
    )))

(eopl:pretty-print (fib 1))
(eopl:pretty-print (fib 2))
(eopl:pretty-print (fib 3))
(eopl:pretty-print (fib 4))
(eopl:pretty-print (fib 5))
(eopl:pretty-print (fib 6))
(eopl:pretty-print (fib 7))