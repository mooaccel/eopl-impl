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
    (list-val
      (lst (list-of expval?)))
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

  (define expval->list
    (lambda (v)
      (cases expval v
	(list-val (lst) lst)
	(else (expval-extractor-error 'list v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                  variant value)))


;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;
  (define-datatype proc proc?
    (procedure
      (var symbol?)
      (body expression?)
      (env environment?)))

  (define (env-val? v)
    (reference? v))
  
  (define-datatype environment environment?
    (empty-env)
    (extend-env 
      (var symbol?)
      (val env-val?)       ; 现在env保存ref
      (saved-env environment?))
    (extend-env-rec
      (proc_name symbol?)
      (bound_var symbol?)
      (proc_body expression?)
      (saved_env environment?))
  )

  ;; env->list : Env -> List
  ;; used for pretty-printing and debugging
  (define env->list
    (lambda (env)
      (cases environment env
	      (empty-env () '())
	      (extend-env (sym val saved-env)  ; val现在是引用
	        (cons
	          (list sym val)
	          (env->list saved-env)))
	      (extend-env-rec (proc_name bound_var proc_body saved_env)
	        (cons
	          (list 'letrec proc_name '...)
	          (env->list saved_env))))))

  ;; expval->printable : ExpVal -> List
  ;; returns a value like its argument, except procedures get cleaned
  ;; up with env->list 
  (define expval->printable
    (lambda (val)
      (cases expval val
	      (proc-val (p)
	        (cases proc p
	          (procedure (var body saved-env)
	            (list 'procedure var '... (env->list saved-env)))))
	      (else val))))


  (define-datatype continuation continuation?
    (end-main-thread-cont)
    (end-subthread-cont)
    ;(end-cont) 
    ;(zero1-cont 
    ;  (cont continuation?)) 
    (let-exp-cont 
      (var symbol?)
      (body expression?) 
      (env environment?) 
      (cont continuation?))
    (if-test-cont 
      (exp2 expression?) 
      (exp3 expression?) 
      (env environment?) 
      (cont continuation?))
    (diff1-cont
      (exp2 expression?)
      (env environment?)
      (cont continuation?))
    (diff2-cont
      (val1 expval?)
      (cont continuation?))
    (multiplication1-cont
      (exp2 expression?)
      (env environment?)
      (cont continuation?))
    (multiplication2-cont
      (val1 expval?)
      (cont continuation?))
    (rator-cont
      (rand expression?)
      (env environment?)
      (cont continuation?))
    (rand-cont
      (rator expval?)  ; 保存rator expval
      (cont continuation?))
    (begin-exp-cont
      (remaining_exps (list-of expression?))
      (env environment?)
      (cont continuation?))
    (set-rhs-cont 
      (ident symbol?)
      (env environment?) 
      (cont continuation?))
    (spawn-cont 
      (cont continuation?))
    (unop-arg-cont 
      (unary_op unop?)  ; lang.scm
      (cont continuation?))
  )



)