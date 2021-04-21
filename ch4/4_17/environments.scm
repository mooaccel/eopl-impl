(module environments (lib "eopl")
 
  (require "data-structures.scm")  ; for what? extend-env, num-val等
  (require "store.scm")

  (provide init-env empty-env extend-env apply-env)

;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;
  ; 用于测试, 随便选个初始都行
  (define init-env 
    (lambda () 
      (empty-env)))

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

  ; 目前是只支持存一个
  (define apply-env
    (lambda (env search-sym)
      (cases environment env
        (empty-env ()
          (eopl:error 'apply-env "No binding for ~s" search-sym))
        (extend-env (vars val_refs saved-env)
          (cond ((location search-sym vars)
                  => (lambda (idx) 
                        (list-ref val_refs idx)))
                (else (apply-env saved-env search-sym))))
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