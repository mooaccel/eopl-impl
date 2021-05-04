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
        (apply-cont))
      (if (eqv? (car los) s)
          (begin
            (set! val (cdr los))
            (apply-cont))
          (begin
            (set! cont (remove-first-cont1 (car los) cont))
            (set! los (cdr los))
            (remove-first/k)))))

(define (element? v)
  (or (number? v)
      (symbol? v)))

(define-datatype continuation continuation?
  (end-cont)
  (remove-first-cont1
    (car_los element?)
    (cont continuation?)))

(define (apply-cont)
  (cases continuation cont
    (end-cont ()
      (eopl:printf "End of computation.~%") 
      (eopl:printf "This sentence should appear only once.~%") 
      val)
    (remove-first-cont1 (saved_car_los saved_cont)
      (begin
        (set! cont saved_cont)
        (set! val (cons saved_car_los val))
        (apply-cont)))))


(eopl:pretty-print (remove-first 'a '(a b c)))
(eopl:pretty-print (remove-first 'b '(e f g)))
(eopl:pretty-print (remove-first 'a4 '(c1 a4 c1 a4)))
(eopl:pretty-print (remove-first 'x '()))
(eopl:pretty-print (remove-first 2 '(c b d 1 2 3)))

