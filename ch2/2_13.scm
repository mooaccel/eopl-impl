(define report-no-binding-found
  (lambda (search-var) (error 'apply-env "In report-no-binding-found, No binding for: " search-var)))

; empty-env和extend-env返回两种lambda(相当于"对象"), 然后需要支持不同method. m(代表对象数量) * n(n代表method数量)
; 其实就是多态?
(define (empty-env)
  (define (apply-env-aux search_var)
   (report-no-binding-found search_var))

  (define (dispatch mt) 
    (cond ((eqv? mt 'apply-env)
            apply-env-aux)
          ((eqv? mt 'empty-env?)
            #t)
          (else (error "not exist this method in empty-env"))))

  dispatch)

; 为啥下面这么定义不行?
;(define (extend-env saved-var saved-val saved-env)
;  (define (apply-env-aux search_var)
;   (if (eqv? search_var saved-var)
;       saved-val
;       ((saved-env 'apply-env) search_var)))
;       ;(apply-env saved-env search_var)))  可以吗?
;
;  ; 必须返回lambda?
;  (lambda (mt) (
;      (cond ((eqv? mt 'apply-env)
;              apply-env-aux)
;            ((eqv? mt 'empty-env?)
;              #f)
;            (else (error "not exist this method in extend-env")))))
;  )
(define (extend-env saved-var saved-val saved-env)
  (define (apply-env-aux search_var)
   (if (eqv? search_var saved-var)
       saved-val
       ((saved-env 'apply-env) search_var)))

  (define (dispatch mt)
      (cond ((eqv? mt 'apply-env)
              apply-env-aux)
            ((eqv? mt 'empty-env?)
              #f)
            (else (error "not exist this method in extend-env"))))

  dispatch)

(define (apply-env env search_var)
  ((env 'apply-env) search_var))

(define (empty-env? env)
  (env 'empty-env?))
  

(define env_2_13_test_01 (extend-env 'd 
                                     6 
                                     (extend-env 'y 
                                                 8 
                                                 (extend-env 'x 
                                                             7 
                                                             (extend-env 'y 
                                                                         14 
                                                                         (empty-env))))))

(define env_2_13_test_02 (empty-env))

(equal? (apply-env env_2_13_test_01 'd) 6)
(equal? (apply-env env_2_13_test_01 'y) 8)
(equal? (apply-env env_2_13_test_01 'x) 7)
(equal? (empty-env? env_2_13_test_01) #f)

(equal? (empty-env? env_2_13_test_02) #t)
;(apply-env env_2_13_test_01 'mm)