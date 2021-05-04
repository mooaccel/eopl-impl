#lang eopl

(define cur_n 'uninitialized)
(define cont 'uninitialized)
(define val 'uninitialized)

(define pc 'uninitialized)

(define fact
  (lambda (arg_n) 
    (set! cont (end-cont))
    (set! cur_n arg_n)
    (set! pc fact/k)
    (trampoline!)))

(define trampoline!
  (lambda () 
    (if pc
        (begin
          (pc)
          (trampoline!))
        val)))

(define fact/k
  (lambda ()
    (if (zero? cur_n) 
        (begin
          (set! val 1)
          (set! pc apply-cont))
        (begin
          (set! cont (fact1-cont cur_n cont))
          (set! cur_n (- cur_n 1))
          (set! pc fact/k)))))

(define-datatype continuation continuation?
  (end-cont) 
  (fact1-cont 
    (n integer?) 
    (cont continuation?)))

(define apply-cont
  (lambda () 
    (cases continuation cont 
      (end-cont ()
        (set! pc #f))
      (fact1-cont (saved_n saved_cont) 
        (eopl:printf "In apply-cont fact1-cont, saved_n = ~s, val = ~s ~%" saved_n val)
        (set! cont saved_cont)
        (set! val (* saved_n val))
        (set! pc apply-cont)))))


(eopl:pretty-print (fact 5))