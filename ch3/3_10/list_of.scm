#lang eopl

(define (list-of pred)
  (lambda (val)
   (or (null? val)
       (and (pair? val)
            (pred (car val))
            ((list-of pred) (cdr val)))
   )))
  
(provide list-of)