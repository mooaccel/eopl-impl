(define empty-env 
  (lambda () (list 'empty-env)))

(define extend-env
  (lambda (var val env) (list 'extend-env var val env)))

(define apply-env
 (lambda (env search-var)
  (cond
   ((eqv? (car env) 'empty-env)
    (report-no-binding-found search-var))
   ((eqv? (car env) 'extend-env)
    (let ((saved-var (cadr env))
          (saved-val (caddr env))
          (saved-env (cadddr env)))
     (if (eqv? search-var saved-var)
         saved-val
         (apply-env saved-env search-var))))
   (else
    (report-invalid-env env)))))

(define report-no-binding-found
  (lambda (search-var) (error 'apply-env "No binding for: " search-var)))
(define report-invalid-env
  (lambda (env) (error 'apply-env "Bad environment: " env)))

(define env1 (extend-env 'd 
                      6 
                      (extend-env 'y 
                                  8 
                                  (extend-env 'x 
                                              7 
                                              (extend-env 'y 
                                                          14 
                                                          (empty-env))))))
env1
(equal? (apply-env env1 'y) 8)
(equal? (apply-env env1 'd) 6)
(equal? (apply-env env1 'x) 7)
(apply-env env1 'mm)