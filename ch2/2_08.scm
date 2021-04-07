(load "./ch2/2_05.scm")

(define (empty-env? env)
  (null? env))

(define env_test_01 (extend-env 'd 
                      6 
                      (extend-env 'y 
                                  8 
                                  (extend-env 'x 
                                              7 
                                              (extend-env 'y 
                                                          14 
                                                          (empty-env))))))
(equal? (empty-env? env_test_01) #f)
(define env_test_02 (empty-env))
(equal? (empty-env? env_test_02) #t)