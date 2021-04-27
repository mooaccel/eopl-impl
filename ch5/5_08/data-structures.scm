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
      (proc proc?))
    (pair-val
      (car_val expval?)
      (cdr_val expval?))
    (emptylist-val)
      )

  ; exer3.10需要, 这里不需要
  ;(define (list-val expvals)
  ;  (if (null? expvals)
  ;      (emptylist)
  ;      (pair-val (car expvals)
  ;                (list-val (cdr expvals)))))

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

  (define expval->pair
    (lambda (v)
      (cases expval v
	      (pair-val (car_val cdr_val)
          (cons car_val cdr_val))
	      (else (expval-extractor-error 'pair v)))))

  ; 判断expval v是不是emptylist-val
  (define expval-emptylist?
    (lambda (v)
      (cases expval v
	      (emptylist-val ()
          (bool-val #t))
	      (else 
          (bool-val #f)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

  ;; proc? : SchemeVal -> Bool
  ;; procedure : Var * Exp * Env -> Proc
  (define-datatype proc proc?
    (procedure
      (vars (list-of symbol?))
      (body expression?)
      (env environment?)))

  ;; Page: 86
  (define-datatype environment environment?
    (empty-env)
    ;(extend-env 
    ;  (bvar symbol?)
    ;  (bval expval?)
    ;  (saved-env environment?))
    (extend-env 
      (vars (list-of symbol?))
      (vals (list-of expval?))
      (saved_env environment?))
    (extend-env-rec
      (id symbol?)
      (bvar symbol?)
      (body expression?)
      (saved-env environment?))
  )

  (define-datatype continuation continuation?
    (end-cont) 
    (zero1-cont 
      (cont continuation?)) 
    ;(let-exp-cont 
    ;  (var symbol?)   ; todo
    ;  (body expression?) 
    ;  (env environment?) 
    ;  (cont continuation?))
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
      (rands (list-of expression?))
      (env environment?)
      (cont continuation?))
    (rands-cont
      (rator expval?)  ; 保存rator expval
      (cont continuation?))
    (let2-exp-cont-1 
      (var1 symbol?)
      (var2 symbol?)
      (exp2 expression?)
      (body expression?)
      (env environment?)
      (cont continuation?))
    (let2-exp-cont-2 
      (var1 symbol?) 
      (val1 expval?)  ; 保存val1
      (var2 symbol?)
      (body expression?)
      (env environment?)
      (cont continuation?))
    (cons-exp-cont-1
      (exp2 expression?)
      (env environment?)
      (cont continuation?))
    (cons-exp-cont-2
      (val1 expval?)
      (cont continuation?))
    (car-exp-cont
      (cont continuation?))
    (cdr-exp-cont
      (cont continuation?))
    (null?-exp-cont
      (cont continuation?))
    (list-exp-cont-car
      (remaining_exps (list-of expression?))
      (env environment?)
      (cont continuation?))
    (list-exp-cont-remaining
      (car_val expval?)
      (cont continuation?))
    (let-exp-cont
      (vars (list-of symbol?))
      (body expression?)
      (env environment?)
      (cont continuation?))
  )
)
