(module data-structures (lib "eopl")

  (require "lang.scm")                  ; for expression?
  (require "store.scm")                 ; for reference?

  (provide (all-defined-out))               ; too many things to list

  ;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;
  (define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
    (proc-val 
      (proc proc?))
    )

  ;;; extractors:
  (define expval->num
    (lambda (v)
      (cases expval v
	(num-val (num) num)
	(else (expval-extractor-error 'num v)))))

  (define expval->bool
    (lambda (v)
      (cases expval v
	(bool-val (bool) bool)
	(else (expval-extractor-error 'bool v)))))

  (define expval->proc
    (lambda (v)
      (cases expval v
	(proc-val (proc) proc)
	(else (expval-extractor-error 'proc v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                  variant value)))


;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;
  (define-datatype proc proc?
    (procedure
      (vars (list-of symbol?))
      (body expression?)
      (env environment?)))  ; 创建proc时保存当时的env(如果想优化参考3_26)
  
  (define-datatype environment environment?
    (empty-env)
    (extend-env 
      (vars (list-of symbol?))
      (val_refs (list-of reference?))       ; env保存的是ref
      (saved_env environment?))
    (extend-env-rec
      (listof_proc_name (list-of identifier?))
      (listof_bound_vars (list-of (list-of identifier?)))
      (listof_proc_body (list-of expression?))
      (saved_env environment?))
  )

  ;; env->list : Env -> List
  ;; used for pretty-printing and debugging
  (define env->list
    (lambda (env)
      (cases environment env
	      (empty-env () '())
	      (extend-env (vars val_refs saved_env)
	        (cons
	          (list 'vars: vars 'val_refs: val_refs)  ; 为了可读性.
	          (env->list saved_env)))
        (extend-env-rec (listof_proc_name listof_bound_vars listof_proc_body saved_env)
	        (cons
	          (list 'letrec listof_proc_name '...)
	          (env->list saved_env)))
      )))

  ;; expval->printable : ExpVal -> List
  ;; returns a value like its argument, except procedures get cleaned
  ;; up with env->list 
  (define expval->printable
    (lambda (val)
      (cases expval val
	      (proc-val (p)
	        (cases proc p
	          (procedure (vars body saved_env)
	            (list 'procedure vars 'body... 'saved_env: (env->list saved_env)))))
	      (else val))))


)