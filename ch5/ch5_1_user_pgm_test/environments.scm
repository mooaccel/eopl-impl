(module environments (lib "eopl.ss" "eopl") 
  
  ;; builds environment interface, using data structures defined in
  ;; data-structures.scm. 

  (require "data-structures.scm")

  (provide init-env empty-env extend-env apply-env)

;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;
  
  ;; init-env : () -> Env
  ;; usage: (init-env) = [i=1, v=5, x=10]
  ;; (init-env) builds an environment in which i is bound to the
  ;; expressed value 1, v is bound to the expressed value 5, and x is
  ;; bound to the expressed value 10.
  ;; Page: 69

  (define init-env 
    (lambda ()
      (extend-env (list 'i) 
                  (list (num-val 1))
                  (extend-env (list 'v)
                              (list (num-val 5))
                              (extend-env (list 'x)
                                          (list (num-val 10))
                                          (empty-env))))))

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

;  ;; Page: 86
;  (define apply-env
;    (lambda (env search-sym)
;      (cases environment env
;        (empty-env ()
;          (eopl:error 'apply-env "No binding for ~s" search-sym))
;        (extend-env (var val saved-env)
;	        (if (eqv? search-sym var)
;	          val
;	          (apply-env saved-env search-sym)))
;            (extend-env-rec (p-name b-var p-body saved-env)
;              (if (eqv? search-sym p-name)
;                (proc-val (procedure b-var p-body env))          
;                (apply-env saved-env search-sym))))))
;
  (define apply-env
    (lambda (env search_sym)
      (cases environment env
        (empty-env ()
          (eopl:error 'apply-env "No binding for ~s" search_sym))
        (extend-env (vars val_refs saved_env)
          (cond ((location search_sym vars)
                  => (lambda (idx) 
                        (list-ref val_refs idx)))
                (else (apply-env saved_env search_sym))))
        (extend-env-rec (p-name b-vars p-body saved_env)
          (if (eqv? search_sym p-name)
              (proc-val (procedure b-vars p-body env))
              (apply-env saved_env search_sym))))))

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