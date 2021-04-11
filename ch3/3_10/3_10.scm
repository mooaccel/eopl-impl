#lang eopl

(require "../3_06/env.scm")
(require "../3_10/expval.scm")
(require "../3_10/list_of.scm")

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
 (addition-exp
  (exp1 expression?)
  (exp2 expression?))
 (multiplication-exp
  (exp1 expression?)
  (exp2 expression?))
 (div-exp
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
  (body expression?))
 (equal?-exp
  (exp1 expression?)
  (exp2 expression?))
 (greater?-exp
  (exp1 expression?)
  (exp2 expression?))
 (less?-exp
  (exp1 expression?)
  (exp2 expression?))
 (cons-exp
  (exp1 expression?)
  (exp2 expression?))
 (car-exp
  (exp1 expression?))
 (cdr-exp
  (exp1 expression?))
 (emptylist-exp)
 (nulllist?-exp
  (exp1 expression?))
 (list-exp
  (exps (list-of expression?)))
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
    (expression ("/" "(" expression "," expression ")") div-exp)
    (expression ("==" "(" expression "," expression ")") equal?-exp)
    (expression (">" "(" expression "," expression ")") greater?-exp)
    (expression ("<" "(" expression "," expression ")") less?-exp)
    (expression ("minus" "(" expression ")") minus-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression ("let" identifier "=" expression "in" expression) let-exp)
    (expression ("cons" "(" expression "," expression ")") cons-exp)
    (expression ("car" "(" expression ")") car-exp)
    (expression ("cdr" "(" expression ")") cdr-exp)
    (expression ("emptylist") emptylist-exp)
    (expression ("nulllist?" "(" expression ")") nulllist?-exp)
    (expression ("list" "(" (separated-list expression ",") ")" ) list-exp)  ; separated-list是什么?
    ))

; 暂时先不借助sllgen:make-define-datatypes生成define-datatype. 出于初学原因.
; 只想使用scan&parse的能力.
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
      (div-exp (exp1 exp2)
        (let ((val1 (value-of exp1 env)) 
              (val2 (value-of exp2 env))) 
          (let ((num1 (expval->num val1)) 
                (num2 (expval->num val2))) 
            (num-val (/ num1 num2)))))
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
          (value-of body (extend-env var val1 env))))
      (equal?-exp (exp1 exp2)
        (let ((val1 (value-of exp1 env))
              (val2 (value-of exp2 env)))
          (let ((num1 (expval->num val1))
                (num2 (expval->num val2)))
            (bool-val (= num1 num2)))
          ))
      (greater?-exp (exp1 exp2)
        (let ((val1 (value-of exp1 env))
              (val2 (value-of exp2 env)))
          (let ((num1 (expval->num val1))
                (num2 (expval->num val2)))
            (bool-val (> num1 num2)))
          ))
      (less?-exp (exp1 exp2)
        (let ((val1 (value-of exp1 env))
              (val2 (value-of exp2 env)))
          (let ((num1 (expval->num val1))
                (num2 (expval->num val2)))
            (bool-val (< num1 num2)))
          ))
      (cons-exp (exp1 exp2)
        (let ((val1 (value-of exp1 env))
              (val2 (value-of exp2 env)))
          (cons-val val1 val2)
        ))
      (car-exp (exp1)
        (let ((val1 (value-of exp1 env)))
          (expval_cons->car val1)
        ))
      (cdr-exp (exp1)
        (let ((val1 (value-of exp1 env)))
          (expval_cons->cdr val1)
        ))
      (emptylist-exp ()
        (cons-emptylist-val))
      (nulllist?-exp (exp1)
        (let ((val1 (value-of exp1 env)))
          (expval_cons->nulllist? val1)
        ))
      (list-exp (exps)
        (list-val (map (lambda (exp_item) 
                         (value-of exp_item env))
                       exps)))
  )))

(define test_init_env
                            (empty-env))

; (define test_01_example
;   (let-exp 'x
;            (const-exp 4)
;            (list-exp (list (var-exp 'x)
;                            (diff-exp (var-exp 'x) (const-exp 1))
;                            (diff-exp (var-exp 'x) (const-exp 3))))))
; (eopl:pretty-print (value-of test_01_example
;                              test_init_env))
(eopl:pretty-print (run "list(1, 2, 3)"))
(eopl:pretty-print (run 
"let x = 4
 in list (x, -(x,1), - (x, 3))"
))
(eopl:pretty-print (run 
"let x = 4
 in list (x, -(x,1), list (100 ,200),- (x, 3))"
))