#lang eopl

; (require "../3_06/env.scm")
(require "./env.scm")
(require "./expval.scm")

(define-datatype program program? 
  (a-program 
    (exp1 expression?)))

(define-datatype expression expression?
 (const-exp 
  (num number?))
 (minus-exp
  (body_exp expression?))
 (diff-exp 
  (exp1 expression?) 
  (exp2 expression?))
 (zero?-exp 
  (exp1 expression?))
 (if-exp 
  (exp1 expression?) 
  (exp2 expression?) 
  (exp3 expression?))
 (var-exp 
  (var identifier?))
 (let-exp 
  (var identifier?) 
  (exp1 expression?) 
  (body expression?)))

;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;
; 怎么写the-lexical-spec和the-grammar?? todo 待研究
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
    (expression ("minus" "(" expression ")") minus-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression ("let" identifier "=" expression "in" expression) let-exp)
    ))

;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
; p386页
; (sllgen:make-define-datatypes the-lexical-spec the-grammar)
; 能看生成的是什么define-datatype.
(eopl:pretty-print (sllgen:list-define-datatypes the-lexical-spec the-grammar))
; 除了var-exp的(var-exp (var-exp15 symbol?))有点不一样, why?todo

; 暂时先不借助sllgen:make-define-datatypes生成define-datatype. 出于初学原因.

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(define value-of-program
  (lambda (pgm)
    (cases program pgm
	   (a-program (exp1)
		      (value-of exp1 test_init_env)))))

(define value-of
  (lambda (exp env)
    (cases expression exp
      (const-exp (num)        ; const-exp这种名字既能当constructor, 又能当cases里的匹配符. 估计define-datatype底下就这么实现的
        (num-val num))
      (minus-exp (body_exp)
        (let ((body_val (value-of body_exp env)))
          (num-val (- (expval->num body_val)))))
      (var-exp (var) 
        (apply-env env var))  ; apply-env返回的value已经是内部表示了, num-val/bool-val这种了
      (diff-exp (exp1 exp2)
        (let ((val1 (value-of exp1 env)) 
              (val2 (value-of exp2 env))) 
          (let ((num1 (expval->num val1)) 
                (num2 (expval->num val2))) 
            (num-val (- num1 num2)))))
      (zero?-exp (exp1)
        (let ((val1 (value-of exp1 env))) 
          (let ((num1 (expval->num val1))) 
            (if (zero? num1) 
                (bool-val #t) 
                (bool-val #f)))))
      (if-exp (exp1 exp2 exp3)
        (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
                (value-of exp2 env)
                (value-of exp3 env))))
      (let-exp (var exp1 body)
        (let ((val1 (value-of exp1 env)))
          (value-of body (extend-env var val1 env)))))))

(define test_init_env
    (extend-env 'x 
                (num-val 33) 
                (extend-env 'y
                            (num-val 22) 
                            (empty-env))))

(define test_01_ast_example
  (minus-exp (const-exp 100)))
(define test_02_ast_example
  (minus-exp (diff-exp (var-exp 'y)
                       (minus-exp (const-exp 100)))))
(eopl:pretty-print (value-of test_01_ast_example
                             test_init_env))
(eopl:pretty-print (value-of test_02_ast_example
                             test_init_env))

(eopl:pretty-print (run "x"))
(eopl:pretty-print (run "y"))
(eopl:pretty-print (run "-(x,y)"))
(eopl:pretty-print (run "-(y,x)"))
(eopl:pretty-print (run "10"))
(eopl:pretty-print (run "-(1, x)"))
(eopl:pretty-print (run  "if zero?(-(11,11)) then 3 else 4"))
(eopl:pretty-print (run  "minus(4)"))
(eopl:pretty-print (scan&parse "minus(4)"))
(eopl:pretty-print (scan&parse "-(100,20)"))

(eopl:pretty-print (run "-(100,20)"))
;(value-of-program (scan&parse "x"))