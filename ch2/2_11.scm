; 在做2_10的时候首先想到的就是这个思路...
; ribcage representation

(define empty-env 
  (lambda () '()))

(define extend-env
  (lambda (var val env) (cons (list (list var) (list val))
                              env)))

; 还不如换成(pair, pair, pair) pair := var val ?
(define (extend-env* lst1 lst2 env)
  (cons (list lst1 lst2)
        env))

(define (is-in-saved-vars-aux? s saved_vars)
  (if (null? saved_vars)
      #f
      (let ((car_saved_vars (car saved_vars)))
        (if (eqv? car_saved_vars s)
            #t
            (is-in-saved-vars-aux? s (cdr saved_vars))))))

(define (obtain-val-aux search_var saved_vars saved_vals)
  (let ((car_saved_vars (car saved_vars))
        (car_saved_vals (car saved_vals)))
    ; just for debug
    ; (display "=======")
    ; (display saved_vars)
    ; (display saved_vals)
    (if (eqv? car_saved_vars search_var)
        car_saved_vals
        (obtain-val-aux search_var (cdr saved_vars) (cdr saved_vals)))))

(define apply-env
  (lambda (env search_var)
    (cond ((null? env)
            (report-no-binding-found search_var))
          (else (let ((saved_vars (caar env))
                      (saved_vals (car (cdar env)))
                      (saved_env (cdr env)))             
                (if (is-in-saved-vars-aux? search_var saved_vars)
                    (obtain-val-aux search_var saved_vars saved_vals)  
                    (apply-env saved_env search_var)))))))

(define report-no-binding-found
  (lambda (search-var) (error 'apply-env "No binding for: " search-var)))

(define env_2_11_test_01 (extend-env 'd 
                      6 
                      (extend-env 'y 
                                  8 
                                  (extend-env* '(q w e o)
                                              '(61 62 63 64) 
                                              (extend-env 'y 
                                                          14 
                                                          (empty-env))))))
(define env_2_11_test_02 (extend-env* '(h j k) '(100 200 300) env_2_11_test_01))
env_2_11_test_02
(equal? (apply-env env_2_11_test_02 'h) 100)
(equal? (apply-env env_2_11_test_02 'j) 200)
(equal? (apply-env env_2_11_test_02 'k) 300)
(equal? (apply-env env_2_11_test_02 'y) 8)
(equal? (apply-env env_2_11_test_02 'd) 6)
(equal? (apply-env env_2_11_test_02 'q) 61)
(equal? (apply-env env_2_11_test_02 'w) 62)
(equal? (apply-env env_2_11_test_02 'e) 63)
(equal? (apply-env env_2_11_test_02 'o) 64)
;(apply-env env_2_11_test_02 'mm)