#lang eopl

(define (remove-first s los)
  (remove-first/k s los (lambda (val) 
                          (begin
                            (eopl:printf "End of computation.~%") 
                            (eopl:printf "This sentence should appear only once.~%") 
                            val))))

(define (remove-first/k s los cont)
  (if (null? los)
      (cont '())
      (let ((car_los (car los)))
        (if (eqv? car_los s)
            (cont (cdr los))
            (remove-first/k s (cdr los) (lambda (remaning) 
                                          (cont (cons car_los remaning))))))))


(eopl:pretty-print (remove-first 'a '(a b c)))
(eopl:pretty-print (remove-first 'b '(e f g)))
(eopl:pretty-print (remove-first 'a4 '(c1 a4 c1 a4)))
(eopl:pretty-print (remove-first 'x '()))
(eopl:pretty-print (remove-first 2 '(c b d 1 2 3)))