; 这个版本需要引入cont_snapshot变量...
#lang eopl

(define s 'uninitialized)
(define los 'uninitialized)
(define cont 'uninitialized)
(define val 'uninitialized)

(define (remove-first arg_s arg_los)
  (set! s arg_s)
  (set! los arg_los)
  (set! cont (lambda ()
              (begin
                (eopl:printf "End of computation.~%") 
                (eopl:printf "This sentence should appear only once.~%")
                val)))
  (remove-first/k))

(define (remove-first/k)
  (if (null? los)
      (begin 
        (set! val '())
        (cont))
      (let ((car_los_snapshot (car los))
            (cont_snapshot cont))
        (if (eqv? car_los_snapshot s)
            (begin
              (set! val (cdr los))
              (cont))
            (begin
              (set! cont (lambda ()
                            (begin
                              (set! val (cons car_los_snapshot val))
                              (cont_snapshot))))
              (set! los (cdr los))
              (remove-first/k))))))


(eopl:pretty-print (remove-first 'a '(a b c)))
(eopl:pretty-print (remove-first 'b '(e f g)))
(eopl:pretty-print (remove-first 'a4 '(c1 a4 c1 a4)))
(eopl:pretty-print (remove-first 'x '()))
(eopl:pretty-print (remove-first 2 '(c b d 1 2 3)))