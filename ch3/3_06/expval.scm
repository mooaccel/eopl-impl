#lang eopl

(define-datatype expval expval?
  (num-val 
    (num number?)) 
  (bool-val 
    (bool boolean?)))

(define expval->num
  (lambda (val) 
    (cases expval val 
      (num-val (num) 
        num) 
      (else 
        (eopl:error 'num "expval-num need num-val")))))

(define expval->bool
  (lambda (val) 
    (cases expval val 
      (bool-val (bool) 
        bool) 
      (else 
        (eopl:error 'bool "expval-bool need bool-val")))))            

(provide num-val)
(provide bool-val)
(provide expval->num)
(provide expval->bool)