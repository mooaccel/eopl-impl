#lang eopl

(define (accumulate op initial sequence) 
  (if (null? sequence) 
      initial 
      (op (car sequence) 
          (accumulate op initial (cdr sequence)))))

; #lang eopl竟然没提供?
; 待优化todo
(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst))
         (cons (car lst) (filter pred (cdr lst))))
        (else 
         (filter pred (cdr lst)))))


(provide accumulate)
(provide filter)
; (eopl:pretty-print (accumulate + 0 (list 1 2 3)))
