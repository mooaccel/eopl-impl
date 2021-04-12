#lang eopl

(define (identifier? var)
  (symbol? var))
; todo, 可能需要改变, 目前是什么都能放, 比如num-val, proc-val...
(define (scheme-val? var)
  #t)

(define-datatype env-exp env-exp?
  (empty-env)
  (extend-env
    (idents (list-of identifier?))
    (scheme_vals (list-of scheme-val?))
    (env_exp env-exp?)))

(define (apply-env env search_var)
  (cases env-exp env
    (empty-env ()
      (eopl:error 'apply-env "~s not found" search_var))
    (extend-env (idents scheme_vals env_exp)
        (if (is-in-saved-vars-aux? search_var idents)
            (obtain-val-aux search_var idents scheme_vals)  
            (apply-env env_exp search_var)))))

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
    (if (eqv? car_saved_vars search_var)
        car_saved_vals
        (obtain-val-aux search_var (cdr saved_vars) (cdr saved_vals)))))

(provide identifier?)
(provide empty-env)
(provide extend-env)
(provide apply-env)

; (define test_init_env_01 (extend-env (list 'x 'y 'z)  
;                                      (list 2 3 4) 
;                                      (empty-env)))
; (eopl:pretty-print test_init_env_01)
; (eopl:pretty-print (apply-env test_init_env_01 'x))
; (eopl:pretty-print (apply-env test_init_env_01 'y))
; (eopl:pretty-print (apply-env test_init_env_01 'z))
; 
; (define test_init_env_02 (extend-env (list 'd)
;                                      (list 6)
;                                      (extend-env (list 'y)
;                                                  (list 8)
;                                                  (extend-env '(q w e o)
;                                                              '(61 62 63 64) 
;                                                              (extend-env (list 'y)
;                                                                          (list 14)
;                                                                          (empty-env))))))
; (newline)
; (eopl:pretty-print (apply-env test_init_env_02 'd))
; (eopl:pretty-print (apply-env test_init_env_02 'y))
; (eopl:pretty-print (apply-env test_init_env_02 'q))
; (eopl:pretty-print (apply-env test_init_env_02 'w))
; (eopl:pretty-print (apply-env test_init_env_02 'e))
; (eopl:pretty-print (apply-env test_init_env_02 'o))
; ;;(eopl:pretty-print (apply-env test_init_env_02 'mm))
; ;todo, 如果输入的参数为空, 这一层的env还需要制造出来吗?感觉没必要了呀...
; (eopl:pretty-print test_init_env_02)
; (eopl:pretty-print (extend-env '() '() test_init_env_02))
; ;(eopl:pretty-print test_init_env_02)
; (eopl:pretty-print (extend-env '(mm dd) 
;                                '(33 44) 
;                                (extend-env '() '() test_init_env_02)))
; #(struct:extend-env
;   (mm dd)
;   (33 44)
;   #(struct:extend-env
;     ()
;     ()
;     #(struct:extend-env
;       (d)
;       (6)
;       #(struct:extend-env
;         (y)
;         (8)
;         #(struct:extend-env
;           (q w e o)
;           (61 62 63 64)
;           #(struct:extend-env (y) (14) #(struct:empty-env)))))))
; 是空没关系, apply-env能识别出来, 暂时不用管这里, 到时候可能优化env的时候再管吧