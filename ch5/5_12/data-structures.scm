(module data-structures (lib "eopl.ss" "eopl")

  (require "lang.scm")                  ; for expression?

  (provide (all-defined-out))               ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean or a procval.

  (define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
    (proc-val 
      (proc proc?)))

;;; extractors:

  ;; expval->num : ExpVal -> Int
  (define expval->num
    (lambda (v)
      (cases expval v
	(num-val (num) num)
	(else (expval-extractor-error 'num v)))))

  ;; expval->bool : ExpVal -> Bool
  (define expval->bool
    (lambda (v)
      (cases expval v
	(bool-val (bool) bool)
	(else (expval-extractor-error 'bool v)))))

  ;; expval->proc : ExpVal -> Proc
  (define expval->proc
    (lambda (v)
      (cases expval v
	(proc-val (proc) proc)
	(else (expval-extractor-error 'proc v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))

  (define (expval->readable v)
    (cases expval v
      (num-val (value)
        value)
      (bool-val (boolean)
        boolean)
      (proc-val (proc)
        'proc)))

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

  ;; proc? : SchemeVal -> Bool
  ;; procedure : Var * Exp * Env -> Proc
  (define-datatype proc proc?
    (procedure
      (bvar symbol?)
      (body expression?)
      (env environment?)))

  ;; Page: 86
  (define-datatype environment environment?
    (empty-env)
    (extend-env 
      (bvar symbol?)
      (bval expval?)
      (saved-env environment?))
    (extend-env-rec
      (id symbol?)
      (bvar symbol?)
      (body expression?)
      (saved-env environment?)))

  (define-datatype continuation continuation?
    (end-cont) 
    (zero1-cont 
      (cont continuation?)) 
    (let-exp-cont 
      (var symbol?)   ; todo
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
    (rator-cont
      (rand expression?)
      (env environment?)
      (cont continuation?))
    (rand-cont
      (rator expval?)  ; 保存rator expval
      (cont continuation?))
    ;(call-exp-rand-cont 
    ;  (rator expression?)
    ;  (env environment?)
    ;  (cont continuation?))
    ;(call-exp-rator-cont 
    ;  (rand expval?)
    ;  (cont continuation?))
  )

  ;; env->list : Env -> List
  ;; used for pretty-printing and debugging
  (define env->list
    (lambda (env)
      (cases environment env
	      (empty-env () '())
	      (extend-env (var val saved_env)
	        (cons
	          (list 'var: var 'val: val)
	          (env->list saved_env)))
        (extend-env-rec (id bvar body saved_env)
          (cons
            (list id bvar 'body...)
            (env->list saved_env)))
      )))

  ;; expval->printable : ExpVal -> List
  ;; returns a value like its argument, except procedures get cleaned
  ;; up with env->list 
  (define expval->printable
    (lambda (val)
      (if (vector? val)
          (list 'vec...)
          (cases expval val
	           (proc-val (p)
	             (cases proc p
	               (procedure (vars body saved_env)
	                 (list 'procedure vars 'body... 'saved_env: (env->list saved_env)))))
	           (else val)))))
)
