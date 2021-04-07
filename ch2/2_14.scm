(define report-no-binding-found
  (lambda (search-var) (error 'apply-env "In report-no-binding-found, No binding for: " search-var)))

(define (empty-env)
  (define (apply-env-aux search_var)
   (report-no-binding-found search_var))

  (define (has-binding-aux? search_var)
   ;(error "symbol not in env, not found!" search_var))
   #f)

  (define (dispatch mt) 
    (cond ((eqv? mt 'apply-env)
            apply-env-aux)
          ((eqv? mt 'has-binding?)
            has-binding-aux?)
          ((eqv? mt 'empty-env?)
            #t)
          (else (error "not exist this method in empty-env"))))

  dispatch)

(define (extend-env saved-var saved-val saved-env)

  (define (apply-env-aux search_var)
   (if (eqv? search_var saved-var)
       saved-val
       ((saved-env 'apply-env) search_var)))

  (define (has-binding-aux? search_var)
   (if (eqv? search_var saved-var)
       #t
       ((saved-env 'has-binding?) search_var)))

  (define (dispatch mt)
      (cond ((eqv? mt 'apply-env)
              apply-env-aux)
            ((eqv? mt 'has-binding?)
              has-binding-aux?)
            ((eqv? mt 'empty-env?)
              #f)
            (else (error "not exist this method in extend-env"))))

  dispatch)

(define (apply-env env search_var)
  ((env 'apply-env) search_var))

(define (empty-env? env)
  (env 'empty-env?))

(define (has-binding? env search_var)
  ((env 'has-binding?) search_var))
  

(define env_2_14_test_01 (extend-env 'd 
                                     6 
                                     (extend-env 'y 
                                                 8 
                                                 (extend-env 'x 
                                                             7 
                                                             (extend-env 'y 
                                                                         14 
                                                                         (empty-env))))))

(define env_2_14_test_02 (empty-env))

(equal? (apply-env env_2_14_test_01 'd) 6)
(equal? (apply-env env_2_14_test_01 'y) 8)
(equal? (apply-env env_2_14_test_01 'x) 7)
(equal? (empty-env? env_2_14_test_01) #f)

(equal? (empty-env? env_2_14_test_02) #t)
(equal? (has-binding? env_2_14_test_01 'd) #t)
(equal? (has-binding? env_2_14_test_01 'y) #t)
(equal? (has-binding? env_2_14_test_01 'x) #t)
(equal? (has-binding? env_2_14_test_01 'mm) #f)
;(apply-env env_2_14_test_01 'mm)