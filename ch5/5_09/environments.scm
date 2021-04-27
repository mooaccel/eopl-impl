(module environments (lib "eopl")
 
  (require "data-structures.scm")  ; for what? extend-env, num-val等
  (require "store.scm")

  (provide init-env empty-env extend-env apply-env)

;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;
  ; i, v, x 没啥必要... 空就行...
  ;; init-env : () -> Env
  (define init-env 
    (lambda ()
      (extend-env 
        'i (newref (num-val 1))
        (extend-env
          'v (newref (num-val 5))
          (extend-env
            'x (newref (num-val 10))
            (empty-env))))))
  ;(define init-env 
  ;  (lambda () 
  ;    (empty-env)))

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

  ; 目前是只支持存一个
  (define apply-env
    (lambda (env search_sym)
      (cases environment env
        (empty-env ()
          (eopl:error 'apply-env "No binding for ~s" search_sym))
        (extend-env (var val saved_env)
	        (if (eqv? search_sym var)
	            val
	            (apply-env saved_env search_sym)))
        (extend-env-rec (proc_name bound_var proc_body saved_env)
          (if (eqv? search_sym proc_name)
              (newref       ; 需要返回ref
                (proc-val 
                  (procedure bound_var 
                                           proc_body 
                                           env)))
              (apply-env saved_env search_sym)))
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