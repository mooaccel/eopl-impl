#lang eopl

(define occurs-free?
  (lambda (var exp)
    (occurs-free?/k var exp (lambda (val) 
                              (eopl:printf "End of computation.~%") 
                              (eopl:printf "This sentence should appear only once.~%") 
                              val))))

(define occurs-free?/k
  (lambda (var exp cont)
    (cond ((symbol? exp)
            (cont (eqv? var exp)))
          ((eqv? (car exp) 'lambda)
            (occurs-free?/k var 
                           (caddr exp)
                           (lambda (bool_in_body)
                              (cont (and (not (eqv? var (car (cadr exp))))
                                         bool_in_body)))))
          (else
           (occurs-free?/k var 
                           (car exp)
                           (lambda (bool_in_rator) 
                               (occurs-free?/k var
                                               (cadr exp)
                                               (lambda (bool_in_rand)
                                                   (cont (or bool_in_rator
                                                             bool_in_rand))))))))))

(eopl:pretty-print (occurs-free? 'x 'x))
(eopl:pretty-print (occurs-free? 'x 'y))
(eopl:pretty-print (occurs-free? 'x '(lambda (x) (x y))))
(eopl:pretty-print (occurs-free? 'x '(lambda (y) (x y))))
(eopl:pretty-print (occurs-free? 'x '((lambda (x) x) (x y))))
(eopl:pretty-print (occurs-free? 'x '(lambda (y) (lambda (z) (x (y z))))))