(module environments (lib "eopl")
 
  (require "data-structures.scm")  ; for what? extend-env, num-val等

  (provide init-env empty-env extend-env apply-env)

;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;
  ; i, v, x 没啥必要... 空就行...
  ;; init-env : () -> Env
  ;; usage: (init-env) = [i=1, v=5, x=10]
  ;; (init-env) builds an environment in which i is bound to the
  ;; expressed value 1, v is bound to the expressed value 5, and x is
  ;; bound to the expressed value 10.
  ;; Page: 69
  (define init-env 
    (lambda ()
      (extend-env 
       'i (num-val 1)
       (extend-env
        'v (num-val 5)
        (extend-env
         'x (num-val 10)
         (empty-env))))))

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

  ; 目前是只支持存一个
  (define apply-env
    (lambda (env search-sym)
      (cases environment env
        (empty-env ()
          (eopl:error 'apply-env "No binding for ~s" search-sym))
        (extend-env (var val saved-env)
	        (if (eqv? search-sym var)
	            val
	            (apply-env saved-env search-sym)))
        (extend-env-rec* (p-names b-vars p-bodies saved-env)  ; extend-env-rec*和extend-env-rec一样吧? 就是命名区别?
          (cond 
            ((location search-sym p-names)
             => (lambda (n)
                  (proc-val
                    (procedure 
                      (list-ref b-vars n)
                      (list-ref p-bodies n)
                      env))))
            (else (apply-env saved-env search-sym))
          )))))

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