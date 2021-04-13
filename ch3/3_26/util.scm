#lang eopl

(define (accumulate op initial sequence) 
  (if (null? sequence) 
      initial 
      (op (car sequence) 
          (accumulate op initial (cdr sequence)))))

(provide accumulate)
; (eopl:pretty-print (accumulate + 0 (list 1 2 3)))
