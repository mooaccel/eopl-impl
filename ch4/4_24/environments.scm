(module environments (lib "eopl")
 
  (require "data-structures.scm")  ; for what? extend-env, num-val等
  (require "store.scm")

  (provide init-env empty-env extend-env apply-env
           extend-env-rec)


;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;
  ; 用于测试, 随便选个初始都行
  ; (define init-env 
  ;   (lambda () 
  ;     (empty-env)))
  (define init-env 
    (lambda ()
      (extend-env (list 'i) 
                  (list (newref (num-val 1)))
                  (extend-env (list 'v) 
                              (list (newref (num-val 5)))
                              (extend-env (list 'x) 
                                          (list (newref (num-val 10)))
                                          (empty-env))))))

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

  ; 返回新的env
  (define (extend-env-rec listof_proc_name listof_bound_vars listof_proc_body saved_env)
    ; 不创建size = 10的原因是proc_name对应的vec不好设置
    (define (create-listof-vec-aux len)
      (if (eq? len 0)
          '()
          (cons (make-vector 1) 
                (create-listof-vec-aux (- len 1)))))
    ; 带个后缀避免重名, 其实感觉没啥必要...
    (define (listof-vec-set! listof_vec_aux listof_bound_vars_aux listof_proc_body_aux new_env_aux)
      (if (null? listof_vec_aux)
          '()
          (begin (vector-set! (car listof_vec_aux)
                              0  ; index
                              (proc-val (procedure (car listof_bound_vars_aux)
                                                   (car listof_proc_body_aux)
                                                   new_env_aux)))
                 (listof-vec-set! (cdr listof_vec_aux)
                                  (cdr listof_bound_vars_aux)
                                  (cdr listof_proc_body_aux)
                                  new_env_aux))))

    (let ((listof_vec (create-listof-vec-aux (length listof_proc_name))))
      (let ((new_env (extend-env listof_proc_name 
                                 (map (lambda (vec_item) 
                                          (newref vec_item))
                                      listof_vec)
                                 saved_env)))
        (listof-vec-set! listof_vec listof_bound_vars listof_proc_body new_env)
        new_env)))

  ; 返回ref, if exist
  (define apply-env
    (lambda (env search-sym)
      (cases environment env
        (empty-env ()
          (eopl:error 'apply-env "No binding for ~s" search-sym))
        (extend-env (vars val_refs saved_env)
          (cond ((location search-sym vars)
                  => (lambda (idx) 
                        (list-ref val_refs idx)))
                (else (apply-env saved_env search-sym))))
  )))

  (define location
    (lambda (sym syms)
      (cond
        ((null? syms) #f)
        ((eqv? sym (car syms)) 0)
        ((location sym (cdr syms))
         => (lambda (n) 
              (+ n 1)))
        (else #f))))

  )