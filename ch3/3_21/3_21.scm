#lang eopl

(require "./env.scm")

; ========= expval
(define-datatype expval expval?
  (num-val 
    (num number?)) 
  (cons-val
    (car_val expval?)
    (cdr_val expval?))
  (cons-emptylist-val)
  (proc-val 
    (proc proc?))
)

(define (list-val expvals)
  (if (null? expvals)
      (cons-emptylist-val)
      (cons-val (car expvals)
                (list-val (cdr expvals)))))

(define expval->num
  (lambda (val) 
    (cases expval val 
      (num-val (num) 
        num) 
      (else 
        (eopl:error 'num "expval-num need num-val")))))

; 输出还是expval
(define expval_cons->car
  (lambda (val) 
    (cases expval val 
      (cons-val (car_val cdr_val) 
        car_val) 
      (else 
        (eopl:error 'bool "expval_cons->car need cons-val")))))            

(define expval_cons->cdr
  (lambda (val) 
    (cases expval val 
      (cons-val (car_val cdr_val) 
        cdr_val) 
      (else 
        (eopl:error 'bool "expval_cons->cdr need cons-val")))))            

(define expval_cons->nulllist?
  (lambda (val) 
    (cases expval val 
      (cons-emptylist-val ()
        (num-val 1))
      (else 
        (num-val 0)))))

(define expval->proc
  (lambda (val) 
    (cases expval val 
      (proc-val (proc)
        proc)
      (else 
        (eopl:error 'num "expval-proc need proc-val")))))

; ============= procedure
(define proc?
  (lambda (val) 
    (procedure? val)))
(define procedure
  (lambda (vars body env) 
    (lambda (vals) 
      (value-of body (extend-env vars   ; todo, env底下的接口目前有点奇怪...
                                 vals
                                 env)))))  ; procedure所在的env被放到了lambda里(capture?)
(define apply-procedure
  (lambda (proc1 vals) 
    (proc1 vals)))

; ================ expression, value-of
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
 (let*-exp 
  (vars (list-of identifier?)) 
  (exps (list-of expression?))
  (body expression?))
 (unpack-exp
  (idents (list-of identifier?))
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
 (proc-exp
  (vars (list-of identifier?))
  (body expression?))
 (call-exp
  (rator expression?)
  (rands (list-of expression?)))
 (letproc-exp
  (proc_name identifier?)
  (var identifier?)
  (proc_body expression?)
  (let_body expression?))
)

; sllgen:show-define-datatypes生成的是这样的:
; (proc-exp (proc-exp40 (list-of symbol?)) (proc-exp41 expression?))
; (call-exp (call-exp42 expression?) (call-exp43 (list-of expression?)))

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
    (expression ("let" (arbno identifier "=" expression) "in" expression) let-exp) ; arbno可以出现0个吗? 可以, separated-list也可以
    (expression ("let*" (arbno identifier "=" expression) "in" expression) let*-exp)
    (expression ("unpack" (arbno identifier) "=" expression "in" expression) unpack-exp)
    (expression ("cond" (arbno expression "==>" expression) "end") cond-exp)  ; arbno是什么?
    (expression ("cons" "(" expression "," expression ")") cons-exp)
    (expression ("car" "(" expression ")") car-exp)
    (expression ("cdr" "(" expression ")") cdr-exp)
    (expression ("emptylist") emptylist-exp)
    (expression ("nulllist?" "(" expression ")") nulllist?-exp)
    (expression ("list" "(" (separated-list expression ",") ")" ) list-exp)  ; separated-list是什么?
    (expression ("proc" "(" (separated-list identifier ",") ")" expression) proc-exp)  ; proc也可以没有参数
    (expression ("(" expression (arbno expression) ")") call-exp)  ; (f)可以没有参数, 只有rator
    (expression ("letproc" identifier "=" "(" identifier ")" expression "in" expression) letproc-exp)
))

; (eopl:pretty-print (sllgen:show-define-datatypes the-lexical-spec the-grammar))
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
      (let*-exp (vars exps body)
          ;(eopl:pretty-print vars)
          ;(eopl:pretty-print exps)
          (let ((val_exps (eval-exps-with-update-env-aux vars exps env)))
            ; 思路受到/ch3/p69_p72_value_of/p68_let例子的env变化情况示意图.png启发, 虽然是不同的场景
            ;(eopl:pretty-print val_exps)
            (value-of body (extend-env vars 
                                       val_exps 
                                       env))
          ))
      (unpack-exp (idents exp1 body)
        (let ((val_exp (value-of exp1 env)))
          ; 类似于let, 把variable binding放入env       
          ; 得到的val_exp必须是cons-val形成连接
          ; 
          (value-of body  
                    ; 注意, unpack-exp-aux'(), 而且entend-env也为(), 目前是进行extend的, 这里可以进行消除, todo
                    (extend-env idents 
                                (unpack-exp-aux idents val_exp)
                                env))))
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
      (proc-exp (vars body) 
        ; (eopl:pretty-print "debug value-of proc-exp ...")
        ; (eopl:pretty-print env)
        ; (eopl:pretty-print body)
        (proc-val (procedure vars body env)))  ; procedure create timepoint的env存放到procedure里
      (call-exp (rator rands)
        ; just for debug
        ; (eopl:pretty-print "debug value-of call-exp ...")
        ; (eopl:pretty-print env)
        (let ((proc (expval->proc (value-of rator env)))   ; 此处的env(调用时的)用来得到proc和arg
              (args (map (lambda (rand_exp) 
                            (value-of rand_exp env))
                         rands)))
          (apply-procedure proc args)))
      (letproc-exp (proc_name var proc_body let_body)
        (let ((proc_val (proc-val (procedure var proc_body env))))
          (value-of let_body (extend-env (list proc_name) 
                                         (list proc_val)
                                         env))
        ))
  )))

; #(struct:cons-val
;   #(struct:num-val 7)
;   #(struct:cons-val #(struct:num-val 3) #(struct:cons-emptylist-val)))
(define (unpack-exp-aux idents val_exp)
  ; just for debug
  ; (eopl:pretty-print "===")
  ; (eopl:pretty-print idents)
  ; (eopl:pretty-print val_exp)
  (if (null? idents)
      (if (= (expval->num (expval_cons->nulllist? val_exp)) 0)  ; 如果不空
          (eopl:error 'unpack-exp-aux "idents and val_exp length not equal!")
          '())
      (cons (expval_cons->car val_exp) 
            (unpack-exp-aux (cdr idents) (expval_cons->cdr val_exp)))))

; 返回一个求值好的vals
(define (eval-exps-with-update-env-aux vars exps env)
  ; (eopl:pretty-print "debug....")
  ; (eopl:pretty-print vars)
  ; (eopl:pretty-print exps)
  ; (eopl:pretty-print env)
  (if (null? exps)
      '()
      (let ((val_car_exp (value-of (car exps) env)))
        ;(eopl:pretty-print val_car_exp)
        (cons val_car_exp (eval-exps-with-update-env-aux (cdr vars) 
                                                         (cdr exps) 
                                                         (extend-env (list (car vars))
                                                                     (list val_car_exp)
                                                                     env))))))

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
; (eopl:pretty-print
;   (value-of
;   (let-exp (list 'f)
;            (list (proc-exp 'x
;                            (proc-exp 'y
;                                      (addition-exp (var-exp 'x)
;                                                    (var-exp 'y)))))
;            (call-exp (call-exp (var-exp 'f)
;                                (const-exp 30))
;                      (const-exp 40)))
;   test_init_env)
; )

(eopl:pretty-print "========= 3.21基础测试")
; 没有参数
(eopl:pretty-print (run
"
let f = proc () +(20,10)
in (f)
"
))

(eopl:pretty-print (run
"
let f = proc (x) +(x,10)
in (f 4)
"
))

(eopl:pretty-print (run
"
let f = proc (x, y) +(x,y)
in (f 3 4)
"
))

(eopl:pretty-print "========= 3.20的测试")
(eopl:pretty-print (run
"
let f = proc (x) proc (y) +(x,y)
in ((f 30) 40)
"
))
(eopl:pretty-print (run
"
let f = proc (x) proc (y) -(x, -(0, y)) 
in ((f 10) 20)
"
))
(eopl:pretty-print (run
"
let f = proc (x) proc (y) proc(z) +(x, *(y, z)) 
in (((f 10) 20) 30)
"
))
; 579
(eopl:pretty-print (run
"
let f = proc (x) proc (y) proc(z) proc(m) +(-(+(x, *(y, z)), m), 9)
in ((((f 10) 20) 30) 40)
"
))

(eopl:pretty-print "========= 3.21多参数+多层")
(eopl:pretty-print (run
"
let f = proc (x, y) proc (m, n) +(*(x,y) , *(m, n)) 
in ((f 10 20) 20 30)
"
))

; 6*17*46 = 4692
(eopl:pretty-print (run
"
let f = proc (x, y, z) proc (m, n) proc(q, w, e, o)  
                                                      *(
                                                        *(
                                                        +(x,+(y,z)),
                                                        +(m,n)
                                                        ),
                                                        +(+(q,w) , +( e,o))
                                                      )
in (((f 1 2 3) 8 9) 10 11 12 13)
"
))