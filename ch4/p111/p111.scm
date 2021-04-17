; 代码copy from 3_26.scm
; 把free variable优化删除了
#lang eopl

(define empty-store 
  (lambda () 
    '()))

(define the-store 'uninitialized)

(define get-store 
  (lambda () 
    the-store))
    
(define initialize-store!
  (lambda () 
    (set! the-store (empty-store))))

(define reference?
  (lambda (v) 
    (integer? v)))

; ExpVal → Ref
(define newref  ; 为val创建引用.
  (lambda (val) 
    (let ((next-ref (length the-store))) 
      (set! the-store (append the-store (list val))) 
      next-ref)))

; Ref → ExpVal
(define deref
  (lambda (ref) 
    (eopl:pretty-print "In deref")
    (eopl:pretty-print ref)
    (list-ref the-store ref)))

(define (setref! ref val)
  (set! the-store
    (letrec ((setref-inner
               (lambda (store1 ref1) 
                 (cond ((null? store1) 
                         (report-invalid-reference ref the-store)) 
                       ((zero? ref1) 
                         (cons val (cdr store1))) 
                       (else (cons (car store1) 
                                   (setref-inner (cdr store1) (- ref1 1))))))))
        (setref-inner the-store ref))))
(define (report-invalid-reference ref store)
  (eopl:pretty-print 'setref "not find %s corresponding in %s" ref store))

(define (identifier? var)
  (symbol? var))
; environment
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
  (proc-val
   (proc proc?))
  (ref-val
   (ref reference?)))
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
(define expval->ref
  (lambda (v)
    (cases expval v
      (ref-val (ref) 
        ref)
      (else 
        (expval-extractor-error 'ref v)))))
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
                (extend-env (list var) 
                            (list val) 
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
   (vars (list-of identifier?))
   (vals (list-of expression?))
   (body expression?))
  (proc-exp
   (var identifier?)
   (body expression?))
  (call-exp
   (rator expression?)
   (rand expression?))
  (newref-exp
   (exp1 expression?))
  (deref-exp
   (exp1 expression?))
  (setref-exp 
   (exp1 expression?)
   (exp2 expression?))
)

;  Expression ::= newref (Expression)
;  newref-exp (exp1)
;  Expression ::= deref (Expression)
;  deref-exp (exp1)
;  Expression ::= setref (Expression , Expression)

;  (define the-lexical-spec
;    '((whitespace (whitespace) skip)
;      (comment ("%" (arbno (not #\newline))) skip)
;      (identifier
;       (letter (arbno (or letter digit "_" "-" "?")))
;       symbol)
;      (number (digit (arbno digit)) number)
;      (number ("-" digit (arbno digit)) number)
;      ))
;  (define the-grammar
;    '((program (expression) a-program)
;      (expression (identifier) var-exp)
;      (expression (number) const-exp)
;      (expression ("-" "(" expression "," expression ")") diff-exp)
;      (expression ("+" "(" expression "," expression ")") addition-exp)
;      (expression ("*" "(" expression "," expression ")") multiplication-exp)
;      (expression ("zero?" "(" expression ")") zero?-exp)
;      (expression ("if" expression "then" expression "else" expression) if-exp)
;      (expression ("let" (arbno identifier "=" expression) "in" expression) let-exp)
;      (expression ("proc" "(" identifier ")" expression) proc-exp)
;      (expression ("(" expression expression ")") call-exp)
;  ))
;  
;  (define scan&parse
;    (sllgen:make-string-parser the-lexical-spec the-grammar))
;  
;  (define run
;    (lambda (string)
;      (value-of-program (scan&parse string))))
;  
;  (define value-of-program
;    (lambda (pgm)
;      (cases program pgm
;  	   (a-program (exp1)
;  		      (value-of exp1 empty_init_env)))))
;  
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
        ; (eopl:pretty-print "val_exps = ")
        ; (eopl:pretty-print val_exps)
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
                             body 
                             env)))
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
        (apply-procedure proc arg)))
    (newref-exp (exp1)
      (let ((v1 (value-of exp1 env))) 
        (ref-val (newref v1))))
    (deref-exp (exp1)
      (let ((v1 (value-of exp1 env))) 
        (deref (expval->ref v1))))
    (setref-exp (exp1 exp2)
      (let ((v1 (value-of exp1 env))
            (v2 (value-of exp2 env)))          
        (begin
            (setref! (expval->ref v1)     ; store里存的是expval?
                     v2)
            (num-val 23))))  ; 随意的值
))

; (eopl:pretty-print (scan&parse
; "
; let x = 37
; in proc (y)
;     let z = -(y,x) 
;     in -(x,y)
; "
; ))

; "
; let g = let counter = newref(0)
;         in proc (dummy) 
;             begin 
;               setref(counter, -(deref(counter), -1));
;               deref(counter) 
;             end
; in let a = (g 11)
;    in let b = (g 11)
;       in -(a,b)
; "
(initialize-store!)

(eopl:pretty-print (value-of
  (let-exp (list 'g)
           (list (let-exp (list 'counter)
                          (list (newref-exp (const-exp 10)))  ; 'counter对应ref-val
                          (proc-exp 'dummy
                            (deref-exp (var-exp 'counter)))))
           (let-exp (list 'a)
                    (list (call-exp (var-exp 'g)
                                    (const-exp 11)))
                    (let-exp (list 'b)
                             (list (call-exp (var-exp 'g)
                                             (const-exp 11)))
                             (diff-exp (var-exp 'a)
                                       (var-exp 'b)))))
  empty_init_env
))
; (eopl:pretty-print (value-of
; (let-exp (list 'g)
;          (list (const-exp 3))
;          (diff-exp (var-exp 'g)
;                    (const-exp 10)))
; empty_init_env
; )
; )