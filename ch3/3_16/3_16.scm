; 代码copy from 3_13
#lang eopl

(require "../3_16/env.scm")
(require "../3_16/expval.scm")

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
  (vars (list-of identifier?)) 
  (exps (list-of expression?))
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
 (cond-exp
  (cond_exps (list-of expression?))
  (act_exps  (list-of expression?)))
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
  (exps (list-of expression?)))  ; list-of #lang eopl内置了
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
    (expression ("let" (arbno identifier "=" expression) "in" expression) let-exp) ; arbno可以出现0个吗? let这里可以
    (expression ("cond" (arbno expression "==>" expression) "end") cond-exp)  ; arbno是什么?
    (expression ("cons" "(" expression "," expression ")") cons-exp)
    (expression ("car" "(" expression ")") car-exp)
    (expression ("cdr" "(" expression ")") cdr-exp)
    (expression ("emptylist") emptylist-exp)
    (expression ("nulllist?" "(" expression ")") nulllist?-exp)
    (expression ("list" "(" (separated-list expression ",") ")" ) list-exp)  ; separated-list是什么?
))

;(eopl:pretty-print (sllgen:show-define-datatypes the-lexical-spec the-grammar))
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
                (num-val 1) 
                (num-val 0)))))
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
      (cond-exp (cond_exps act_exps)
        (cond-exp-handle-aux cond_exps act_exps env))
      (equal?-exp (exp1 exp2)
        (let ((val1 (value-of exp1 env))
              (val2 (value-of exp2 env)))
          (let ((num1 (expval->num val1))
                (num2 (expval->num val2)))
            (if (= num1 num2)
                (num-val 1)
                (num-val 0)))
          ))
      (greater?-exp (exp1 exp2)
        (let ((val1 (value-of exp1 env))
              (val2 (value-of exp2 env)))
          (let ((num1 (expval->num val1))
                (num2 (expval->num val2)))
            (if (> num1 num2)
                (num-val 1)
                (num-val 0)))
          ))
      (less?-exp (exp1 exp2)
        (let ((val1 (value-of exp1 env))
              (val2 (value-of exp2 env)))
          (let ((num1 (expval->num val1))
                (num2 (expval->num val2)))
            (if (< num1 num2)
                (num-val 1)
                (num-val 0)))
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

(define (cond-exp-handle-aux cond_exps act_exps env)
  (if (null? cond_exps)
      (eopl:error 'cond-exp-handle-aux "cond fail, no exist cond")
      (let ((cond_exp (car cond_exps))
            (act_exp (car act_exps)))
        (let ((val_cond_exp (value-of cond_exp env)))
          (if (not (= (expval->num val_cond_exp) 0))
              (value-of act_exp env)
              (cond-exp-handle-aux (cdr cond_exps)
                                   (cdr act_exps)
                                   env))))))

(define test_init_env
                            (empty-env))


(define test_01_example
  (let-exp (list 'x)
           (list (const-exp 30))
           (let-exp (list 'x 'y)
                    (list (diff-exp (var-exp 'x) 
                                    (const-exp 1))
                          (diff-exp (var-exp 'x)
                                    (const-exp 2)))
                    (diff-exp (var-exp 'x)
                              (var-exp 'y)))))

(eopl:pretty-print (value-of test_01_example
                             test_init_env))
(eopl:pretty-print (run 
"
let x = 30
in let x = -(x,1) y = -(x,2) 
   in +(x,y)
"
))
(eopl:pretty-print (run 
"
let x = 30
in let x = -(x,1) y = -(x,2) 
   in -(x,y)
"
))


; (eopl:pretty-print (run
; "
; let x = 100
; in let
;    in -(x, 21)
; "
; ))



; 总结: 感觉3.16很简单... 就只要改改env就行了