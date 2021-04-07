(load "./ch2/2_05.scm")

(define (has-binding? env s)
  (cond ((null? env)
          #f)
        (else (let ((saved_var (caar env))
                    (saved_val (cdar env))
                    (saved_env (cdr env)))             
                (if (eqv? saved_var s)
                    #t
                    (has-binding? saved_env s))
               ))))

(define env_test_has_binding_01 (extend-env 'd 
                                            6 
                                            (extend-env 'y 
                                                        8 
                                                        (extend-env 'x 
                                                                    7 
                                                                    (extend-env 'y 
                                                                                14 
                                                                                (empty-env))))))
(has-binding? env_test_has_binding_01 'd)
(has-binding? env_test_has_binding_01 'y)
(has-binding? env_test_has_binding_01 'x)
(has-binding? env_test_has_binding_01 'xmy)
;(equal? ( env_test_01) #f)
;(define env_test_02 (empty-env))
;(equal? (empty-env? env_test_02) #t)