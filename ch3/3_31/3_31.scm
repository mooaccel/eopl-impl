#lang eopl

(define (identifier? var)
  (symbol? var))
(define (scheme-val? v)
  (expval? v))

; env再次改造成3_23这样的可以存多个的模式
(define-datatype environment environment?
  (empty-env) 
  (extend-env 
    (idents (list-of identifier?))
    (scheme_vals (list-of scheme-val?))
    (saved_env environment?))
  (extend-env-rec 
    (proc_name identifier?) 
    (bound_vars (list-of identifier?))
    (proc_body expression?) 
    (saved_env environment?))
)

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

(define (apply-env env search_var)
  ; (eopl:pretty-print "======")
  ; (eopl:pretty-print env)
  (cases environment env
    (empty-env ()
      (eopl:error 'apply-env "~s not found" search_var))
    (extend-env (idents scheme_vals saved_env)
        (if (is-in-saved-vars-aux? search_var idents)
            (obtain-val-aux search_var idents scheme_vals)  
            (apply-env saved_env search_var)))
    (extend-env-rec (proc_name bound_vars proc_body saved_env)
      (if (eqv? search_var proc_name)
          (proc-val (procedure bound_vars proc_body env))
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
    (vars (list-of identifier?))
    (body expression?) 
    (saved_env environment?)))

(define (apply-procedure proc1 vals) 
  (cases proc proc1 
    (procedure (vars body saved_env)
      (value-of body
                (extend-env vars
                            vals 
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
   (rands (list-of expression?)))
  (letrec-exp 
   (proc_name identifier?)
   (bound_vars (list-of identifier?))
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
    (expression ("proc" "(" identifier ")" expression) proc-exp)
    (expression ("(" expression (arbno expression) ")") call-exp)
    (expression ("letrec" identifier "(" (separated-list identifier ",") ")" "=" expression "in" expression) letrec-exp)
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
    (call-exp (rator rands)
      ; (eopl:pretty-print "debug value-of call-exp ...")
      ; (eopl:pretty-print "rator---------------")
      ; (eopl:pretty-print rator)
      ; (eopl:pretty-print "rand---------------")
      ; (eopl:pretty-print rand)
      ; (eopl:pretty-print "call-exp所处的env ---------------")
      ; (eopl:pretty-print env)
      (let ((proc (expval->proc (value-of rator env)))
            (args (map (lambda (rand) 
                          (value-of rand env))
                       rands)))
        (apply-procedure proc args)))
    (letrec-exp (proc_name bound_vars proc_body letrec_body)
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
                                            bound_vars
                                            proc_body
                                            env)))
))

; (eopl:pretty-print (run
; "
; let f = proc (x) +(x,10)
; in (f 20)
; "
; ))

;  ; double 2 = +(double (1), 2) = +((double (0), 2) 2) = 4
;  (eopl:pretty-print (run
;  "
;  letrec double(x) = if zero?(x)
;                     then 0 
;                     else +((double -(x,1)), 2)
;  in (double 2)
;  "
;  ))

(eopl:pretty-print (run
"
letrec double(x, y) = if zero?(x)
                      then 0 
                      else +((double -(x,1) -(y, 2)), y)
in (double 3 10)
"
))