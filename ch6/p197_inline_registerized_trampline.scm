; 尝试将inline融入ch6/p195_registerized_trampoline_fact_cont.scm
; 这样应该是可以的吧?

#lang eopl

(define cur_n 'uninitialized)
(define cont 'uninitialized)
(define val 'uninitialized)

(define pc 'uninitialized)

(define fact
  (lambda (arg_n) 
    (set! cont (lambda () 
                  (set! pc #f)))
    (set! cur_n arg_n)
    (set! pc fact/k)
    (trampoline!)))

(define trampoline!
  (lambda () 
    (if pc
        (begin
          (pc)
          (trampoline!))
        val)))  ;返回最后的值

(define fact/k
  (lambda ()
    (if (zero? cur_n) 
        (begin
          (set! val 1)
          (set! pc cont))
        (begin
          (let ((cur_n_snapshot cur_n)
                (cont_snapshot cont))
            (set! cont (lambda ()
                          (eopl:printf "cur_n_snapshot = ~s, val = ~s ~%" cur_n_snapshot val)
                          (set! val (* cur_n_snapshot val))
                          (set! pc cont_snapshot)))
            (set! cur_n (- cur_n 1))
            (set! pc fact/k))))))

(eopl:pretty-print (fact 5))