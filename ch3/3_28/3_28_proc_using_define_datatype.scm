#lang eopl

(require "./util.scm")

(define (identifier? var)
  (symbol? var))
(define (scheme-val? v)
  (expval? v))
(define-datatype environment environment?
  (empty-env)
  (extend-env
    (idents (list-of identifier?))
    (scheme_vals (list-of scheme-val?))
    (saved_env environment?)))
(define (apply-env env search_var)
  (cases environment env
    (empty-env ()
      (eopl:error 'apply-env "~s not found" search_var))
    (extend-env (idents scheme_vals saved_env)
      (if (is-in-saved-vars-aux? search_var idents)
          (obtain-val-aux search_var idents scheme_vals)  
          (apply-env saved_env search_var)))))
(define (is-in-saved-vars-aux? s saved_vars)
  (if (null? saved_vars)
      #f
      (let ((car_saved_vars (car saved_vars)))
        (if (eqv? car_saved_vars s)
            #t
            (is-in-saved-vars-aux? s (cdr saved_vars))))))
(define (obtain-val-aux search_var saved_vars saved_vals)
  (let ((car_saved_vars (car saved_vars))
        (car_saved_vals (car saved_vals)))
    (if (eqv? car_saved_vars search_var)
        car_saved_vals
        (obtain-val-aux search_var (cdr saved_vars) (cdr saved_vals)))))

; expressed value
(define-datatype expval expval?
  (num-val
   (num number?))
  ;(bool-val
  ;  (bool boolean?))
  (proc-val
   (proc proc?)))
(define expval->num
  (lambda (v) 
    (cases expval v
      (num-val (num) 
        num) 
      (else 
        (expval-extractor-error 'num v)))))
;(define expval->bool
;  (lambda (v)
;    (cases expval v
;      (bool-val (bool)
;        bool)
;      (else 
;        (expval-extractor-error 'bool v)))))
(define expval->proc
  (lambda (v)
    (cases expval v
      (proc-val (proc) 
        proc)
      (else 
        (expval-extractor-error 'proc v)))))
(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors 
           "Looking for a ~s variant, found ~s"
           variant value)))

; 目前暂时调整成只支持一个参数
(define-datatype proc proc?
  (procedure 
    (var identifier?) 
    (body expression?) 
    ))

(define (apply-procedure proc1 val env) 
  (cases proc proc1 
    (procedure (var body)
      ; (eopl:pretty-print "debug apply-procedure ...")
      ; (eopl:pretty-print "proc1---------------")
      ; (eopl:pretty-print proc1)
      ; (eopl:pretty-print "val---------------")
      ; (eopl:pretty-print val)
      ; (eopl:pretty-print "proc1 contain var body :")
      ; (eopl:pretty-print "var---------------")
      ; (eopl:pretty-print var)
      ; (eopl:pretty-print "body---------------")
      ; (eopl:pretty-print body)
      ; (eopl:pretty-print "env---------------")
      ; (eopl:pretty-print env)
      (value-of body 
                (extend-env (list var) 
                            (list val) 
                            env)))))

(define-datatype program program? 
  (a-program 
    (exp1 expression?)))
(define-datatype expression expression?
  (const-exp
   (num number?))
  (var-exp
   (id symbol?))
  (zero?-exp
   (expr expression?))
  (diff-exp
   (exp1 expression?)
   (exp2 expression?))
  (addition-exp
   (exp1 expression?)
   (exp2 expression?))
  (multiplication-exp
   (exp1 expression?)
   (exp2 expression?))
  (if-exp 
   (exp1 expression?) 
   (exp2 expression?) 
   (exp3 expression?))
  (let-exp
   (vars (list-of identifier?))
   (vals (list-of expression?))
   (body expression?))
  (proc-exp
   (var identifier?)
   (body expression?))
  (call-exp
   (rator expression?)
   (rand expression?)))

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    ))
(define the-grammar
  '((program (expression) a-program)
    (expression (identifier) var-exp)
    (expression (number) const-exp)
    (expression ("-" "(" expression "," expression ")") diff-exp)
    (expression ("+" "(" expression "," expression ")") addition-exp)
    (expression ("*" "(" expression "," expression ")") multiplication-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression ("let" (arbno identifier "=" expression) "in" expression) let-exp)
    (expression ("proc" "(" identifier ")" expression) proc-exp)
    (expression ("(" expression expression ")") call-exp)
))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(define value-of-program
  (lambda (pgm)
    (cases program pgm
	   (a-program (exp1)
		      (value-of exp1 empty_init_env)))))

(define empty_init_env (empty-env))

(define (value-of exp env)
  (cases expression exp
    (const-exp (num)
      (num-val num))
    (var-exp (var) 
      (apply-env env var))
    (zero?-exp (exp1)
      (let ((val1 (value-of exp1 env))) 
        (let ((num1 (expval->num val1))) 
          (if (zero? num1) 
              (num-val 1) 
              (num-val 0)))))
    (diff-exp (exp1 exp2)
      (let ((val1 (value-of exp1 env)) 
            (val2 (value-of exp2 env))) 
        (let ((num1 (expval->num val1)) 
              (num2 (expval->num val2))) 
          (num-val (- num1 num2)))))
    (addition-exp (exp1 exp2)
      ; (eopl:pretty-print "debug value-of addition-exp ...")
      ; (eopl:pretty-print "exp1---------------")
      ; (eopl:pretty-print exp1)
      ; (eopl:pretty-print "exp2---------------")
      ; (eopl:pretty-print exp2)
      ; (eopl:pretty-print "env---------------")
      ; (eopl:pretty-print env)
      (let ((val1 (value-of exp1 env)) 
            (val2 (value-of exp2 env))) 
        (let ((num1 (expval->num val1)) 
              (num2 (expval->num val2))) 
          (num-val (+ num1 num2)))))
    (multiplication-exp (exp1 exp2)
      (let ((val1 (value-of exp1 env)) 
            (val2 (value-of exp2 env))) 
        (let ((num1 (expval->num val1)) 
              (num2 (expval->num val2))) 
          (num-val (* num1 num2)))))
    (if-exp (exp1 exp2 exp3)
      (let ((val1 (value-of exp1 env)))
          (if (not (= (expval->num val1) 0))
              (value-of exp2 env)
              (value-of exp3 env))))
    (let-exp (vars exps body)
      (let ((val_exps (map (lambda (exp_item) 
                          (value-of exp_item env))
                       exps)))
        (value-of body (extend-env vars val_exps env))))
    (proc-exp (var body) 
      ; (eopl:pretty-print "debug value-of proc-exp ...")
      ; (eopl:pretty-print "var in create time---------------")
      ; (eopl:pretty-print var)
      ; (eopl:pretty-print "body in create time---------------")
      ; (eopl:pretty-print body)
      ; (eopl:pretty-print "env in create time---------------")
      ; (eopl:pretty-print env)
        (proc-val (procedure var 
                             body)
    ))
    (call-exp (rator rand)
      ; (eopl:pretty-print "debug value-of call-exp ...")
      ; (eopl:pretty-print "rator---------------")
      ; (eopl:pretty-print rator)
      ; (eopl:pretty-print "rand---------------")
      ; (eopl:pretty-print rand)
      ; (eopl:pretty-print "env ---------------")
      ; (eopl:pretty-print env)
      (let ((proc (expval->proc (value-of rator env)))
            (arg (value-of rand env)))
        (apply-procedure proc arg env)))
))

; 5 - (-3) = 8
(eopl:pretty-print (run
"
let a = 3
in let p = proc (x) -(x,a)
a = 5
in -(a,(p 2))
"
))

; p 2,  x = 2, a = 5, m = 100, i = 120 
; (p 2) = +(-3, 120) = 117
; 5 - 117 = -112
(eopl:pretty-print (run
"
let unuse_variable_01 = 10
in let m = 100
   in let a = 3    
      in let p = proc (x) let i = +(m, 20) in +(-(x, a), i)
             a = 5    
         in -(a,(p 2))
"
))

; 找不到x, 这个例子与3_26的不同了!
; (eopl:pretty-print (run
; "
; let f = proc (x) proc (y) +(x,y)
; in ((f 30) 40)
; "
; ))