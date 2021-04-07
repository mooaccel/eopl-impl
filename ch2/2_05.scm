; This is called an a-list or association-list representation.

(define empty-env 
  (lambda () '()))

(define extend-env
  (lambda (var val env) (cons (cons var val) 
                              env)))

(define apply-env
  (lambda (env search_var)
    (cond ((null? env)
            (report-no-binding-found search_var))
          (else (let ((saved_var (caar env))
                      (saved_val (cdar env))
                      (saved_env (cdr env)))             
                (if (eqv? saved_var search_var)
                    saved_val
                    (apply-env saved_env search_var))
              )))))

(define report-no-binding-found
  (lambda (search-var) (error 'apply-env "No binding for: " search-var)))


(define env2 (extend-env 'd 
                      6 
                      (extend-env 'y 
                                  8 
                                  (extend-env 'x 
                                              7 
                                              (extend-env 'y 
                                                          14 
                                                          (empty-env))))))
env2
(equal? (apply-env env2 'y) 8)
(equal? (apply-env env2 'd) 6)
(equal? (apply-env env2 'x) 7)
(apply-env env2 'mm)