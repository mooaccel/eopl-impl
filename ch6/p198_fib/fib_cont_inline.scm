#lang eopl

; 这个inline确实有点厉害...

(define fib
  (lambda (n) 
    (fib/k n (lambda (val) 
                val))))
(define fib/k
  (lambda (n cont)
    (if (< n 2)
        (cont 1)
        (fib/k (- n 1)
               (lambda (val1)  ; n cont
                  (fib/k (- n 2)
                         (lambda (val2)
                            (cont (+ val1 val2)))))))))

(eopl:pretty-print (fib 1))
(eopl:pretty-print (fib 2))
(eopl:pretty-print (fib 3))
(eopl:pretty-print (fib 4))
(eopl:pretty-print (fib 5))
(eopl:pretty-print (fib 6))
(eopl:pretty-print (fib 7))