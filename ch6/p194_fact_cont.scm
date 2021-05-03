#lang eopl

(define fact
  (lambda (n) (fact/k n (end-cont))))

(define fact/k
  (lambda (n cont) 
    (if (zero? n) 
        (apply-cont cont 1) 
        (fact/k (- n 1) (fact1-cont n cont)))))

(define-datatype continuation continuation?
  (end-cont) 
  (fact1-cont 
    (n integer?) 
    (cont continuation?)))

(define apply-cont
  (lambda (cont val) 
    (cases continuation cont 
      (end-cont () 
        val) 
      (fact1-cont (saved_n saved_cont) 
        (eopl:printf "In apply-cont fact1-cont, saved_n = ~s, val = ~s ~%" saved_n val)
        (apply-cont saved_cont (* saved_n val))))))


(eopl:pretty-print (fact 5))