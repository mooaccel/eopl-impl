; 代码copy from 3_26.scm
; 把free variable优化删除了
; letrec参考了ch3/3_34/3_34_v2.scm
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
    ; (eopl:pretty-print "In deref")
    ; (eopl:pretty-print ref)
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
(define (env-value? v)
  (or (expval? v)
      (vector? v)))
(define-datatype environment environment?
  (empty-env) 
  (extend-env 
    (idents (list-of identifier?))
    (scheme_vals (list-of env-value?))
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
; apply-env返回expvar
(define (apply-env env search_var) 
  (cases environment env
    (empty-env () 
      (report-no-binding-found search_var)) 
    (extend-env (idents scheme_vals saved_env) 
      (if (is-in-saved-vars-aux? search_var idents)
          (let ((saved_val (obtain-val-aux search_var idents scheme_vals)))
            (if (vector? saved_val)
                (vector-ref saved_val 0)
                saved_val))
          (apply-env saved_env search_var))) 
))
(define (extend-env-rec listof_proc_name listof_bound_vars listof_proc_body saved_env)
  (define (create-listof-vec-aux len)
    (if (eq? len 0)
        '()
        (cons (make-vector 1) 
              (create-listof-vec-aux (- len 1)))))
  ; 带个后缀避免重名, 其实感觉没啥必要...
  (define (listof-vec-set! listof_vec_aux listof_bound_vars_aux listof_proc_body_aux new_env_aux)
    (if (null? listof_vec_aux)
        '()
        (begin (vector-set! (car listof_vec_aux)
                            0  ; index
                            (proc-val (procedure (car listof_bound_vars_aux)
                                                 (car listof_proc_body_aux)
                                                 new_env_aux)))
               (listof-vec-set! (cdr listof_vec_aux)
                                (cdr listof_bound_vars_aux)
                                (cdr listof_proc_body_aux)
                                new_env_aux))))

  (let ((listof_vec (create-listof-vec-aux (length listof_proc_name))))
    (let ((new_env (extend-env listof_proc_name listof_vec saved_env)))  ; 每个vec都需要和proc_name对应上
      (listof-vec-set! listof_vec listof_bound_vars listof_proc_body new_env)
      new_env)))
(define report-no-binding-found
  (lambda (search_var)
    (eopl:error 'report-no-binding-found
                "Lookup ~s in env, not found"
                search_var)))

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
   (vars (list-of identifier?))
   (vals (list-of expression?))
   (body expression?))
  (proc-exp  ; 待改成多参数
   (vars (list-of identifier?))
   (body expression?))
  (call-exp
   (rator expression?)
   (rands (list-of expression?)))
  (letrec-exp
   (listof_proc_name (list-of identifier?))
   (listof_bound_vars (list-of (list-of identifier?)))
   (listof_proc_body (list-of expression?))
   (letrec_body expression?))
  (newref-exp
   (exp1 expression?))
  (deref-exp
   (exp1 expression?))
  (setref-exp 
   (exp1 expression?)
   (exp2 expression?))
  (begin-exp 
   (exp1 expression?)
   (remaining_exps (list-of expression?)))
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
    (expression ("let" (arbno identifier "=" expression) "in" expression) let-exp)
    (expression ("proc" "(" (arbno identifier) ")" expression) proc-exp)
    (expression ("(" expression (arbno expression) ")") call-exp)
    (expression ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression )
                     "in" expression) letrec-exp)  ; 分别生成几个list
    (expression ("newref" "(" expression ")") newref-exp)
    (expression ("deref" "(" expression ")") deref-exp)
    (expression ("setref" "(" expression "," expression ")") setref-exp)
    (expression ("begin" expression (arbno ";" expression) "end") begin-exp)
))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(define value-of-program
  (lambda (pgm)
    (initialize-store!)
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
    (proc-exp (vars body) 
        (proc-val (procedure vars
                             body 
                             env)))
    (call-exp (rator rands)
      (let ((proc (expval->proc (value-of rator env)))
            (args (map (lambda (rand) 
                          (value-of rand env))
                       rands)))
        (apply-procedure proc args)))
    (letrec-exp (listof_proc_name listof_bound_vars listof_proc_body letrec_body)
      (value-of letrec_body (extend-env-rec listof_proc_name
                                            listof_bound_vars
                                            listof_proc_body
                                            env)))
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
    (begin-exp (exp1 remaining_exps)
      (let ((v1 (value-of exp1 env)))
        (if (null? remaining_exps)
            v1
            (value-of (begin-exp (car remaining_exps) (cdr remaining_exps))
                      env))))
))

; -2
; (eopl:pretty-print (run
; "
; let g = let counter = newref(0)
;         in proc (dummy) 
;             begin
;               setref(counter, +(deref(counter), 2));
;               deref(counter) 
;             end
; in let a = (g 11)
;    in let b = (g 11)
;       in -(a,b)
; "
; ))


; let g = let counter = newref-exp(10)
;         in proc (dummy)
;             begin
;               setref(counter, -(deref(counter), -1));
;               deref(counter)
;             end
; in let a = (g 11)
;    in let b = (g 11)
;       in -(a,b)
; return -1
; (initialize-store!)
; (eopl:pretty-print (value-of
;   (let-exp (list 'g)
;            (list (let-exp (list 'counter)
;                           (list (newref-exp (const-exp 10)))  ; 'counter对应ref-val
;                           (proc-exp (list 'dummy)
;                                     (begin-exp (setref-exp (var-exp 'counter)
;                                                            (diff-exp (deref-exp (var-exp 'counter))
;                                                                      (const-exp -1)))
;                                                (list (deref-exp (var-exp 'counter)))))))
;            (let-exp (list 'a)
;                     (list (call-exp (var-exp 'g)
;                                     (list (const-exp 11))))
;                     (let-exp (list 'b)
;                              (list (call-exp (var-exp 'g)
;                                              (list (const-exp 11))))
;                              (diff-exp (var-exp 'a)
;                                        (var-exp 'b)))))
; empty_init_env
; )
; )


; ok
(eopl:pretty-print (run
"
let x = newref(0) 
in letrec even(dummy) = if zero?(deref(x)) 
                        then 
                        1 
                        else begin 
                              setref(x, -(deref(x),1)); 
                              (odd 888) 
                             end
          odd(dummy) = if zero?(deref(x)) 
                       then 
                       0 
                       else begin 
                              setref(x, -(deref(x),1)); 
                              (even 888) 
                            end 
   in begin 
        setref(x,13); 
        (odd 888) 
      end
"
))