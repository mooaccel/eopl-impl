#lang eopl

(define (remove-first s los)
  (remove-first/k s los (end-cont)))

(define (remove-first/k s los cont)
  (if (null? los)
      (apply-cont cont '())
      (if (eqv? (car los) s)
          (apply-cont cont (cdr los))
          (remove-first/k s 
                          (cdr los) 
                          (remove-first-cont1 (car los) cont)))))

(define (end-cont)
  (lambda (val)
    (eopl:printf "End of computation.~%") 
    (eopl:printf "This sentence should appear only once.~%") 
    val))

(define (remove-first-cont1 saved_car_los saved_cont)
  (lambda (val)
    (apply-cont saved_cont (cons saved_car_los val))))

(define (apply-cont cont val)
  (cont val))

(eopl:pretty-print (remove-first 'a '(a b c)))
(eopl:pretty-print (remove-first 'b '(e f g)))
(eopl:pretty-print (remove-first 'a4 '(c1 a4 c1 a4)))
(eopl:pretty-print (remove-first 'x '()))
(eopl:pretty-print (remove-first 2 '(c b d 1 2 3)))