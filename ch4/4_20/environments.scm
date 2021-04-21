(module environments (lib "eopl")
 
  (require "data-structures.scm")  ; for what? extend-env, num-val等
  (require "store.scm")

  (provide init-env empty-env extend-env apply-env)

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

  ; 目前是只支持存一个
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