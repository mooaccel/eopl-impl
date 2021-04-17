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

; nameless-environment
(define nameless-environment?
  (lambda (x) 
    ((list-of expval?) x)))
(define empty-nameless-env
  (lambda () '()))
(define extend-nameless-env
  (lambda (val nameless_env) 
    (cons val nameless_env)))
(define apply-nameless-env
  (lambda (nameless_env lexical_address) 
    (list-ref nameless_env lexical_address)))

; expressed value, expval
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

; proc
(define-datatype proc proc?  ; proc不用再保存var
  (procedure 
    (proc_body expression?) 
    (saved_nameless_env nameless-environment?)))
(define apply-procedure
  (lambda (proc1 val) 
    (cases proc proc1 
      (procedure (proc_body saved_nameless_env) 
        (value-of proc_body (extend-nameless-env val saved_nameless_env))))))

; program
(define-datatype program program? 
  (a-program 
    (exp1 expression?)))
; expression
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
    (expression ("let" identifier "=" expression "in" expression) let-exp)
    (expression ("proc" "(" identifier ")" expression) proc-exp)
    (expression ("(" expression expression ")") call-exp)
    (expression ("$letxref" number) nameless-var-exp) ; 换成了$符号, 换其他的也行, 不然parse会被忽略掉, 不知道书上是什么意思...
    (expression ("$let" expression "in" expression) nameless-let-exp)
    (expression ("$letxproc" expression) nameless-proc-exp)  ; x在这里估计代码匿名的意思?
))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define run
  (lambda (string)
    (value-of-program (translation-of-program (scan&parse string)))))
 
(define value-of-program
  (lambda (pgm)
    (cases program pgm
	   (a-program (exp1)
		      (value-of exp1 empty_init_nameless_env)))))

(define empty_init_nameless_env (empty-nameless-env))

(define (value-of exp env)  ; 这里的env是nameless env

  (cases expression exp
    (const-exp (num)
      (num-val num))
    (var-exp (var)
      #f)
    (let-exp (var val body)
      #f)
    (proc-exp (var body)
      #f)
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
    (nameless-var-exp (lexical_address)
      (apply-nameless-env env lexical_address))
    (nameless-let-exp (val_exp body_exp)
      (let ((val (value-of val_exp env)))
        (value-of body_exp (extend-nameless-env val env))))
    (nameless-proc-exp (proc_exp)
        (proc-val (procedure proc_exp
                             env)))
    (call-exp (rator rand)
      (let ((proc (expval->proc (value-of rator env)))
            (arg (value-of rand env)))
        (apply-procedure proc arg)))
))

;  ; Program → Nameless-program
(define translation-of-program
  (lambda (pgm) 
    (cases program pgm 
      (a-program (exp1) 
        (a-program (translation-of exp1 empty_init_static_env))))))

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
        ; (eopl:pretty-print "let-exp ====")
        ; (eopl:pretty-print senv)
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

(define empty_init_static_env (empty-senv))

; 暂时不需要init-senv
;  
;  ; ? i v x
;  (define init-senv
;    (lambda () 
;      ;(extend-senv 'i 
;      ;             (extend-senv 'v 
;      ;                          (extend-senv 'x 
;                                             (empty-senv)))
;      ;                                       )))

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

; p93页例子, 手写测试ast
; (eopl:pretty-print
; (value-of-program
; (translation-of-program
; (a-program
;   (let-exp 'x
;            (const-exp 37)
;            (proc-exp 'y
;                      (let-exp 'z
;                               (diff-exp (var-exp 'y)
;                                         (var-exp 'x))
;                               (diff-exp (var-exp 'x)
;                                         (var-exp 'y)))))
; )
; )
; )
; )
(eopl:pretty-print
(value-of-program
(translation-of-program
(a-program
  (let-exp 'x
           (const-exp 37)
           (let-exp 'proc_01
                    (proc-exp 'y
                              (let-exp 'z
                                       (diff-exp (var-exp 'y)
                                                 (var-exp 'x))
                                       (diff-exp (var-exp 'z)
                                                 (const-exp 80))))
                    (call-exp (var-exp 'proc_01)
                              (const-exp 10))))
)
)
)
)
;let x = 37
;in let proc_01 = proc (y)
;                  let z = -(y,x)   ; -27
;                  in -(z,80)    ; -27 - 80 = -107
;   in (proc_01 10)

(eopl:pretty-print (run
"
let x = 37
in let proc_01 = proc (y)
                  let z = -(y,x)
                  in -(z,80)
   in (proc_01 10)
"
))

(eopl:pretty-print (translation-of-program (scan&parse
"
let x = 37
in let proc_01 = proc (y)
                  let z = -(y,x)
                  in -(z,80)
   in (proc_01 10)
"
)))
; 不经过lexical address translator, 直接parse成ast
; (eopl:pretty-print (scan&parse
; "
; $let 37
; in +(100,200)
; "
; ))
(eopl:pretty-print (value-of-program (scan&parse
"
$let 37
in $let $letxproc
             $let -($letxref 0, $letxref 1)
             in -($letxref 0, 80)
   in ($letxref 0 
       10)
"
)))
(eopl:pretty-print (scan&parse
"
$let 37
in $let $letxproc
             $let -($letxref 0, $letxref 1)
             in -($letxref 0, 80)
   in ($letxref 0 
       10)
"
))

; (expression ("%letxref" number) nameless-var-exp)
; (expression ("%let" expression "in" expression) nameless-let-exp)
; (expression ("%letxproc" expression) nameless-proc-exp)