#lang eopl

(define (identifier? var)
  (symbol? var))

(define (env-value? v)
  (or (expval? v)
      (vector? v)))

(define-datatype environment environment?
  (empty-env) 
  (extend-env 
    (var identifier?) 
    (val env-value?) 
    (saved_env environment?)) 
)

; apply-env返回expvar
(define (apply-env env search_var) 
  (cases environment env
    (empty-env () 
      (report-no-binding-found search_var)) 
    (extend-env (saved_var saved_val saved_env) 
      (if (eqv? saved_var search_var) 
          (if (vector? saved_val)
              (vector-ref saved_val 0)
              saved_val)
          (apply-env saved_env search_var))) 
))

(define extend-env-rec
  (lambda (p-name b-var body saved-env) 
    (let ((vec (make-vector 1))) 
      (let ((new-env (extend-env p-name vec saved-env))) 
        (vector-set! vec 0 (proc-val (procedure b-var body new-env))) 
        new-env))))

(define report-no-binding-found
  (lambda (search_var)
    (eopl:error 'report-no-binding-found
                "Lookup ~s in env, not found"
                search_var)))

(define-datatype expval expval?
  (num-val
   (num number?))
  (proc-val
   (proc proc?)))
(define expval->num
  (lambda (v) 
    (cases expval v
      (num-val (num) 
        num) 
      (else 
        (expval-extractor-error 'num v)))))
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

(define-datatype proc proc?
  (procedure 
    (var identifier?) 
    (body expression?) 
    (saved_env environment?)))
(define (apply-procedure proc1 val) 
  (cases proc proc1 
    (procedure (var body saved_env)
      (value-of body 
                (extend-env var 
                            val 
                            saved_env)))))

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
   (var1 identifier?)
   (val1 expression?)
   (body expression?))
  (proc-exp
   (var identifier?)
   (body expression?))
  (call-exp
   (rator expression?)
   (rand expression?))
  (letrec-exp 
   (proc_name identifier?)
   (bound_var identifier?)
   (proc_body expression?) 
   (letrec_body expression?))
)

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
    ;(expression ("let" (arbno identifier "=" expression) "in" expression) let-exp)
    (expression ("let" identifier "=" expression "in" expression) let-exp)
    (expression ("letrec" identifier "(" identifier ")" "=" expression "in" expression) letrec-exp)
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
      ; (eopl:pretty-print "in addition-exp")
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
    (let-exp (var1 exp1 body)
      (let ((val_exp1 (value-of exp1 env)))
        (value-of body (extend-env var1 val_exp1 env))))
    (proc-exp (var body) 
        (proc-val (procedure var 
                             body 
                             env)))
    (call-exp (rator rand)
      (let ((proc (expval->proc (value-of rator env)))
            (arg (value-of rand env)))
        (apply-procedure proc arg)))
    (letrec-exp (proc_name bound_var proc_body letrec_body)
      (value-of letrec_body (extend-env-rec proc_name
                                            bound_var
                                            proc_body
                                            env)))
))

; (eopl:pretty-print (run
; "
; let f = proc (x) +(x,10)
; in (f 20)
; "
; ))

(eopl:pretty-print (run
"
letrec double(x) = if zero?(x)
                   then 0 
                   else +((double -(x,1)), 2)
in (double 2)
"
))