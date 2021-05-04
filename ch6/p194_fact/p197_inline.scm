#lang eopl

(define fact
  (lambda (n)
    (fact/k n (lambda (val) 
                val))))

(define fact/k
  (lambda (n cont) 
    (if (zero? n)
        (cont 1)
        (fact/k (- n 1) (lambda (val) 
                          (cont (* n val)))))))

(eopl:pretty-print (fact 6))