#lang eopl

(define s 'uninitialized)
(define los 'uninitialized)
(define cont 'uninitialized)
(define val 'uninitialized)

(define (remove-first arg_s arg_los)
  (set! s arg_s)
  (set! los arg_los)
  (set! cont (end-cont))
  (remove-first/k))

(define (remove-first/k)
  (if (null? los)
      (begin
        (set! val '())
        (apply-cont))  ; 省略了set cont
      (if (eqv? (car los) s)
          (begin
            (set! val (cdr los))
            (apply-cont)) ; 省略了set cont
          (begin
            (set! cont (remove-first-cont1 (car los) cont))
            (set! los (cdr los))
            (remove-first/k)))))

(define (end-cont)
  (lambda ()
    (eopl:printf "End of computation.~%") 
    (eopl:printf "This sentence should appear only once.~%") 
    val))

(define (remove-first-cont1 saved_car_los saved_cont)
  (lambda ()
    (begin
      (set! cont saved_cont)
      ;(eopl:printf "saved_car_los = ~s, val = ~s ~%" saved_car_los val)
      (set! val (cons saved_car_los val))
      (apply-cont))))

(define (apply-cont)
  (cont))

(eopl:pretty-print (remove-first 'a '(a b c)))
(eopl:pretty-print (remove-first 'b '(e f g)))
(eopl:pretty-print (remove-first 'a4 '(c1 a4 c1 a4)))
(eopl:pretty-print (remove-first 'x '()))
(eopl:pretty-print (remove-first 2 '(c b d 1 2 3)))