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

  ; todo
  ;(define (continuation?)
  ;  #t)
  (define (end-cont)
    '())
  (define (zero1-cont cont)
    (cons '() 
          cont))
  (define (let-exp-cont var body env cont)
    (cons (list var body env) 
          cont))
  (define (if-test-cont exp2 exp3 env cont)
    (cons (list exp2 exp3 env)
          cont))
  (define (diff1-cont exp2 env cont)
    (cons (list exp2 env)
          cont))
  (define (diff2-cont val1 cont)
    (cons (list val1)
          cont))
  (define (rator-cont rand env cont)
    (cons (list rand env)
          cont))
  (define (rand-cont rator cont)
    (cons (list rator)
          cont))

  ;(define-datatype continuation continuation?
  ;  (end-cont) 
  ;  (zero1-cont 
  ;    (cont continuation?)) 
  ;  (let-exp-cont 
  ;    (var symbol?)   ; todo
  ;    (body expression?) 
  ;    (env environment?) 
  ;    (cont continuation?))
  ;  (if-test-cont 
  ;    (exp2 expression?) 
  ;    (exp3 expression?) 
  ;    (env environment?) 
  ;    (cont continuation?))
  ;  (diff1-cont
  ;    (exp2 expression?)
  ;    (env environment?)
  ;    (cont continuation?))
  ;  (diff2-cont
  ;    (val1 expval?)
  ;    (cont continuation?))
  ;  (rator-cont
  ;    (rand expression?)
  ;    (env environment?)
  ;    (cont continuation?))
  ;  (rand-cont
  ;    (rator expval?)  ; 保存rator expval
  ;    (cont continuation?))
  ;)
)
