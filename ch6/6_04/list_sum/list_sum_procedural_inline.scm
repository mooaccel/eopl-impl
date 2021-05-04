#lang eopl

;(define list-sum
;  (lambda (loi) 
;    (if (null? loi) 
;        0 
;        (+ (car loi) 
;           (list-sum (cdr loi))))))

(define (list-sum loi)
  (list-sum/k loi (lambda (val) 
                      (begin
                        (eopl:printf "End of computation.~%") 
                        (eopl:printf "This sentence should appear only once.~%") 
                        val))))

(define (list-sum/k loi cont)
  (if (null? loi)
      (cont 0)
      (list-sum/k (cdr loi)
                  (lambda (remaning_sum) 
                      (cont (+ (car loi) 
                               remaning_sum))))))

(eopl:pretty-print (list-sum '(1 2 3 4 5)))
(eopl:pretty-print (list-sum '()))