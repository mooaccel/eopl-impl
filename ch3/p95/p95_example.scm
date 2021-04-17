; 代码copy from 3_26.scm(因为这里精简了一下exp)
; 把free variable优化删除了

; 目前通过lexical address改变let, proc只支持一个参数, 要想多个参数, 得改造
; 本质上来说
; static env, 存var, 通过var获取lexical address, 只在translator阶段使用
; nameless env, 存val, 即expval. 通过lexical address获取expval, value-of阶段使用它, 最终在value-of阶段可以不用出现variable name!
; 如果没有var, 那么想获得对应的val, 怎么做? 通过lexical address, 需要能(uniquely identifies它引用了哪个expval eopl3.6p91)
; 
#lang eopl

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
    (saved_env environment?)))

;  (define (apply-procedure proc1 val) 
;    (cases proc proc1 
;      (procedure (var body saved_env)
;        ; (eopl:pretty-print "debug apply-procedure ...")
;        ; (eopl:pretty-print "proc1---------------")
;        ; (eopl:pretty-print proc1)
;        ; (eopl:pretty-print "val---------------")
;        ; (eopl:pretty-print val)
;        ; (eopl:pretty-print "proc1 contain var body saved_env:")
;        ; (eopl:pretty-print "var---------------")
;        ; (eopl:pretty-print var)
;        ; (eopl:pretty-print "body---------------")
;        ; (eopl:pretty-print body)
;        ; (eopl:pretty-print "saved_env---------------")
;        ; (eopl:pretty-print saved_env)
;        (value-of body 
;                  (extend-env (list var) 
;                              (list val) 
;                              saved_env)))))

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
  ; (let-exp
  ;  (vars (list-of identifier?))
  ;  (vals (list-of expression?))
  ;  (body expression?))
  (let-exp
   (var identifier?)
   (val expression?)
   (body expression?))
  (proc-exp
   (var identifier?)
   (body expression?))
  (call-exp
   (rator expression?)
   (rand expression?))
  (nameless-var-exp  ; 存储lexical address
   (lexical_address number?))
  (nameless-let-exp
   (val_exp expression?)
   (body_exp expression?))
  (nameless-proc-exp
   (proc_exp expression?))
)

;      (var-exp (var) 
;        (nameless-var-exp (apply-senv senv var)))
;      ; ?
;      (let-exp (var exp1 body) 
;        (nameless-let-exp (translation-of exp1 senv) 
;                          (translation-of body (extend-senv var senv)))) 
;      (proc-exp (var body) 
;        (nameless-proc-exp (translation-of body (extend-senv var senv)))) 

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

; (define run
;   (lambda (string)
;     (value-of-program (scan&parse string))))
; 
; (define value-of-program
;   (lambda (pgm)
;     (cases program pgm
; 	   (a-program (exp1)
; 		      (value-of exp1 empty_init_env)))))

; (define empty_init_env (empty-env))

; (define (value-of exp env)
;   (cases expression exp
;     (const-exp (num)
;       (num-val num))
;     (var-exp (var) 
;       (apply-env env var))
;     (zero?-exp (exp1)
;       (let ((val1 (value-of exp1 env))) 
;         (let ((num1 (expval->num val1))) 
;           (if (zero? num1) 
;               (num-val 1) 
;               (num-val 0)))))
;     (diff-exp (exp1 exp2)
;       (let ((val1 (value-of exp1 env)) 
;             (val2 (value-of exp2 env))) 
;         (let ((num1 (expval->num val1)) 
;               (num2 (expval->num val2))) 
;           (num-val (- num1 num2)))))
;     (addition-exp (exp1 exp2)
;       ; (eopl:pretty-print "debug value-of addition-exp ...")
;       ; (eopl:pretty-print "exp1---------------")
;       ; (eopl:pretty-print exp1)
;       ; (eopl:pretty-print "exp2---------------")
;       ; (eopl:pretty-print exp2)
;       ; (eopl:pretty-print "env---------------")
;       ; (eopl:pretty-print env)
;       (let ((val1 (value-of exp1 env)) 
;             (val2 (value-of exp2 env))) 
;         (let ((num1 (expval->num val1)) 
;               (num2 (expval->num val2))) 
;           (num-val (+ num1 num2)))))
;     (multiplication-exp (exp1 exp2)
;       (let ((val1 (value-of exp1 env)) 
;             (val2 (value-of exp2 env))) 
;         (let ((num1 (expval->num val1)) 
;               (num2 (expval->num val2))) 
;           (num-val (* num1 num2)))))
;     (if-exp (exp1 exp2 exp3)
;       (let ((val1 (value-of exp1 env)))
;           (if (not (= (expval->num val1) 0))
;               (value-of exp2 env)
;               (value-of exp3 env))))
;     (let-exp (vars exps body)
;       (let ((val_exps (map (lambda (exp_item) 
;                           (value-of exp_item env))
;                        exps)))
;         (value-of body (extend-env vars val_exps env))))
;     (proc-exp (var body) 
;       ; (eopl:pretty-print "debug value-of proc-exp ...")
;       ; (eopl:pretty-print "var in create time---------------")
;       ; (eopl:pretty-print var)
;       ; (eopl:pretty-print "body in create time---------------")
;       ; (eopl:pretty-print body)
;       ; (eopl:pretty-print "env in create time---------------")
;       ; (eopl:pretty-print env)
;         (proc-val (procedure var 
;                              body 
;                              env)))
;     (call-exp (rator rand)
;       ; (eopl:pretty-print "debug value-of call-exp ...")
;       ; (eopl:pretty-print "rator---------------")
;       ; (eopl:pretty-print rator)
;       ; (eopl:pretty-print "rand---------------")
;       ; (eopl:pretty-print rand)
;       ; (eopl:pretty-print "env ---------------")
;       ; (eopl:pretty-print env)
;       (let ((proc (expval->proc (value-of rator env)))
;             (arg (value-of rand env)))
;         (apply-procedure proc arg)))
; ))

;  ; Program → Nameless-program
(define translation-of-program
  (lambda (pgm) 
    (cases program pgm 
      (a-program (exp1) 
        (a-program (translation-of exp1 (init-senv)))))))

;  translation-of will take two arguments: an expression and a static environment.
(define translation-of
  (lambda (exp senv)  ; 为什么需要senv? senv是var和lexical address的映射.
    (cases expression exp 
      (const-exp (num) 
        (const-exp num)) 
      (diff-exp (exp1 exp2) 
        (diff-exp (translation-of exp1 senv) 
                  (translation-of exp2 senv))) 
      (addition-exp (exp1 exp2)
        (let ((val1 (translation-of exp1 senv)) 
              (val2 (translation-of exp2 senv))) 
          (let ((num1 (expval->num val1)) 
                (num2 (expval->num val2))) 
            (num-val (+ num1 num2)))))
      (multiplication-exp (exp1 exp2)
        (let ((val1 (translation-of exp1 senv)) 
              (val2 (translation-of exp2 senv))) 
          (let ((num1 (expval->num val1)) 
                (num2 (expval->num val2))) 
            (num-val (* num1 num2)))))
      (zero?-exp (exp1) 
        (zero?-exp (translation-of exp1 senv))) 
      (if-exp (exp1 exp2 exp3) 
        (if-exp (translation-of exp1 senv) 
                (translation-of exp2 senv) 
                (translation-of exp3 senv))) 
      (var-exp (var) 
        (nameless-var-exp (apply-senv senv var)))
      ; ?
      (let-exp (var exp1 body) 
        (eopl:pretty-print "let-exp ====")
        (eopl:pretty-print senv)
        (nameless-let-exp (translation-of exp1 senv) 
                          (translation-of body (extend-senv var senv)))) 
      (proc-exp (var body) 
        (nameless-proc-exp (translation-of body (extend-senv var senv)))) 
      (call-exp (rator rand) 
        (call-exp (translation-of rator senv) 
                  (translation-of rand senv))) 
      ; translation-of也得写上这里的匹配..?
      (nameless-var-exp (address)
        #f)
      (nameless-let-exp (val_exp body_exp)
        #f)
      (nameless-proc-exp (proc_exp)
        #f)
      ;(else (report-invalid-source-expression exp))
      )))

; senv是什么?
(define empty-senv
  (lambda () 
    '()))
(define extend-senv
  (lambda (var senv) 
    (cons var senv)))
; 从senv列表里获取lexical address
; 这种表示方法只适用于let, proc里只有一个参数?
(define apply-senv
  (lambda (senv var)
    (cond ((null? senv)
            (eopl:error 'apply-senv "not find ~s in senv" var))
          ((eqv? var 
                 (car senv)) 
            0)
          (else 
            (+ 1 (apply-senv (cdr senv) 
                             var))))))
; ? i v x
(define init-senv
  (lambda () 
    ;(extend-senv 'i 
    ;             (extend-senv 'v 
    ;                          (extend-senv 'x 
                                           (empty-senv)))
    ;                                       )))

; (eopl:pretty-print (scan&parse
; "
; let x = 37
; in proc (y)
;     let z = -(y,x) 
;     in -(x,y)
; "
; ))
; 
; (eopl:pretty-print
;   (let-exp (list 'x)
;            (list (const-exp 37))
;            (proc-exp 'y
;                      (let-exp (list 'z)
;                               (list (diff-exp (var-exp 'y)
;                                               (var-exp 'x)))
;                               (diff-exp (var-exp 'x)
;                                         (var-exp 'y)))))
; )

; 暂时不用
;  ;(eopl:pretty-print
;  (translation-of-program
;  (a-program
;    (let-exp (list 'x)
;             (list (const-exp 37))
;             (proc-exp 'y
;                       (let-exp (list 'z)
;                                (list (diff-exp (var-exp 'y)
;                                                (var-exp 'x)))
;                                (diff-exp (var-exp 'x)
;                                          (var-exp 'y)))))
;  )
;  )
;  ;)

(eopl:pretty-print
(translation-of-program
(a-program
  (let-exp 'x
           (const-exp 37)
           (proc-exp 'y
                     (let-exp 'z
                              (diff-exp (var-exp 'y)
                                              (var-exp 'x))
                              (diff-exp (var-exp 'x)
                                        (var-exp 'y)))))
)
)
)




; 测试init-senv
; (eopl:pretty-print
;   (init-senv)
; )
; (i v x)
