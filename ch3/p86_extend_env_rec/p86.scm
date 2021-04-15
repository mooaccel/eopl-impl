#lang eopl

(define (identifier? var)
  (symbol? var))

(define-datatype environment environment?
  (empty-env) 
  (extend-env 
    (var identifier?) 
    (val expval?) 
    (saved_env environment?)) 
  (extend-env-rec 
    (proc_name identifier?) 
    (bound_var identifier?) 
    (proc_body expression?) 
    (saved_env environment?))
)

; (define (debug-procedure var body env)
;   (eopl:printf "procedure constructor, enter: var = ~a, body = ~a, env = ~a\n" var body env)
;   (let ((procedure_val (procedure var body env)))
;     (eopl:printf "procedure constructor, exit\n")
;     procedure_val))
; 
; (define (apply-env env search_var) 
;   (let ((cache_status 'no_cached)
;         (cache_value 'invalid))
;     (cases environment env 
;       (empty-env () 
;         (report-no-binding-found search_var)) 
;       (extend-env (saved_var saved_val saved_env) 
;         (if (eqv? saved_var search_var) 
;             saved_val 
;            (apply-env saved_env search_var))) 
;       (extend-env-rec (proc_name bound_var proc_body saved_env)
;         (if (eqv? search_var proc_name) 
;             (if (eqv? cache_status 'no_cached)
;                 (let ((proc_val (proc-val (debug-procedure bound_var proc_body env))))
;                  ;(begin
;                    (eopl:pretty-print "have not cache...")
;                    (set! cache_status 'cached)
;                    (set! cache_value proc_val)
;                    cache_value)
;                 ((eopl:pretty-print "have cache...")
;                  cache_value))
;             (apply-env env search_var)))
;         )))
(define (apply-env env search_var) 
  ; (eopl:pretty-print "================In apply-env, outer layer")
  ; (eopl:pretty-print "search_var")
  ; (eopl:pretty-print search_var)
  ; (eopl:pretty-print "env = ")
  ; (eopl:pretty-print env)

  (cases environment env
    (empty-env () 
      (report-no-binding-found search_var)) 
    (extend-env (saved_var saved_val saved_env) 
      (if (eqv? saved_var search_var) 
          saved_val 
          (apply-env saved_env search_var))) 
    (extend-env-rec (proc_name bound_var proc_body saved_env)
      ; (eopl:pretty-print "In apply-env")
      ; (eopl:pretty-print env)
      (if (eqv? search_var proc_name) 
          ;(begin
          ; (eopl:pretty-print "In apply-env, extend-env-rec")
          ; ; (eopl:pretty-print "proc_name = ")
          ; ; (eopl:pretty-print proc_name)
          ; ; (eopl:pretty-print "bound_var = ")
          ; ; (eopl:pretty-print bound_var)
          ; (eopl:pretty-print "proc_body = ")
          ; (eopl:pretty-print proc_body)
          ; ; (eopl:pretty-print "saved_env = ")
          ; ; (eopl:pretty-print saved_env)
          ; (eopl:pretty-print "env = ")
          ; (eopl:pretty-print env)
          ; (proc-val (procedure bound_var proc_body env)))
          (proc-val (procedure bound_var proc_body env))
          (apply-env saved_env search_var)))
))

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
      ; (eopl:pretty-print "in var-exp")
      ; (eopl:pretty-print "env = ")
      ; (eopl:pretty-print env)
      ; (eopl:pretty-print "var = ")
      ; (eopl:pretty-print var)
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
      ; (eopl:pretty-print "in if-exp")
      ; (eopl:pretty-print env)
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
      ; (eopl:pretty-print "debug value-of call-exp ...")
      ; (eopl:pretty-print "rator---------------")
      ; (eopl:pretty-print rator)
      ; (eopl:pretty-print "rand---------------")
      ; (eopl:pretty-print rand)
      ; (eopl:pretty-print "call-exp所处的env ---------------")
      ; (eopl:pretty-print env)
      (let ((proc (expval->proc (value-of rator env)))
            (arg (value-of rand env)))
        (apply-procedure proc arg)))
    (letrec-exp (proc_name bound_var proc_body letrec_body)
      ; (eopl:pretty-print "debug letrec-exp ...")
      ; (eopl:pretty-print "proc_name---------------")
      ; (eopl:pretty-print proc_name)
      ; (eopl:pretty-print "bound_var---------------")
      ; (eopl:pretty-print bound_var)
      ; (eopl:pretty-print "proc_body---------------")
      ; (eopl:pretty-print proc_body)
      ; (eopl:pretty-print "letrec_body---------------")
      ; (eopl:pretty-print letrec_body)
      ; (eopl:pretty-print "letrec-exp所处的env---------------")
      ; (eopl:pretty-print env)
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

; double 2 = +(double (1), 2) = +((double (0), 2) 2) = 4