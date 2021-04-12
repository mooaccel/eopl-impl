; 代码copy from 3_13
#lang eopl

(require "./env.scm")

; 这里貌似会引起循环依赖比较麻烦, 放到一个文件算了

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
  (lambda (var body env) 
    (lambda (val) 
      (value-of body (extend-env (list var)   ; todo, env底下的接口目前有点奇怪...
                                 (list val) 
                                 env)))))  ; procedure所在的env被放到了lambda里(capture?)

(define apply-procedure
  (lambda (proc1 val) 
    ;(eopl:pretty-print "apply-procedure debug...")
    ;(eopl:pretty-print proc1)
    ;(eopl:pretty-print val)
    (proc1 val)))


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
  (var identifier?)  ; 一个参数和多个参数todo
  (body expression?))
 (call-exp
  (rator expression?)
  (rand expression?))  ; 现在只能有一个参数?
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
    (expression ("let*" (arbno identifier "=" expression) "in" expression) let*-exp)
    (expression ("unpack" (arbno identifier) "=" expression "in" expression) unpack-exp)
    (expression ("cond" (arbno expression "==>" expression) "end") cond-exp)  ; arbno是什么?
    (expression ("cons" "(" expression "," expression ")") cons-exp)
    (expression ("car" "(" expression ")") car-exp)
    (expression ("cdr" "(" expression ")") cdr-exp)
    (expression ("emptylist") emptylist-exp)
    (expression ("nulllist?" "(" expression ")") nulllist?-exp)
    (expression ("list" "(" (separated-list expression ",") ")" ) list-exp)  ; separated-list是什么?
    (expression ("proc" "(" identifier ")" expression) proc-exp)
    (expression ("(" expression expression ")") call-exp)  ;目前只支持单参数?
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
      (proc-exp (var body) 
        ; (eopl:pretty-print "debug value-of proc-exp ...")
        ; (eopl:pretty-print env)
        (proc-val (procedure var body env)))  ; procedure create timepoint的env存放到procedure里
      (call-exp (rator rand)
        ; just for debug
        ; (eopl:pretty-print "debug value-of call-exp ...")
        ; (eopl:pretty-print env)
        (let ((proc (expval->proc (value-of rator env)))   ; 此处的env(调用时的)用来得到proc和arg
              (arg (value-of rand env))) 
          (apply-procedure proc arg)))
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
; 简单例子:
;  ; 定义一个procedure
;  (define proc_val_01 (value-of
;      (proc-exp 'z
;                (diff-exp (var-exp 'z)
;                          (var-exp 'x)))
;      (extend-env (list 'x)
;                  (list 200) 
;                  test_init_env)
;  ))
;  
;  ; 调用procedure
;  (eopl:pretty-print (value-of
;      (call-exp
;        (proc-exp 'z
;                  (diff-exp (var-exp 'z)
;                            (var-exp 'x)))
;        ; (proc-exp 'z
;        ;           (multiplication-exp (var-exp 'z)
;        ;                               (const-exp 5)))
;        (const-exp 10)
;      )
;      (extend-env (list 'x)
;                  (list (num-val 200))
;                  test_init_env)
;  ))

; ============================== p77例子(必须理解透彻的), begin
(eopl:pretty-print (value-of
  (let-exp (list 'x)
           (list (const-exp 200))
           (let-exp (list 'f)
                    (list (proc-exp 'z
                                    (diff-exp (var-exp 'z)
                                              (var-exp 'x))))
                    (let-exp (list 'x)
                             (list (const-exp 100))
                             (let-exp (list 'g)
                                      (list (proc-exp 'z
                                                      (diff-exp (var-exp 'z)
                                                                (var-exp 'x))))
                                      (diff-exp (call-exp (var-exp 'f)     ; call-exp里这样调用?
                                                          (const-exp 1))
                                                (call-exp (var-exp 'g)
                                                          (const-exp 1)))))))
  test_init_env
))
; 注意区别两个时间点的env, 
; 1.precedure制造时的:
; "debug value-of proc-exp ..."
; #(struct:extend-env (x) (#(struct:num-val 200)) #(struct:empty-env))
; "debug value-of proc-exp ..."
; #(struct:extend-env
;   (x)
;   (#(struct:num-val 100))
;   #(struct:extend-env
;     (f)
;     (#(struct:proc-val #<procedure:...cedure/value_of.scm:74:4>))
;     #(struct:extend-env (x) (#(struct:num-val 200)) #(struct:empty-env))))
; 2.precedure调用时的: 在这个例子里两个call-exp的env是一样的
; #(struct:extend-env
;   (g)
;   (#(struct:proc-val #<procedure:...cedure/value_of.scm:74:4>))
;   #(struct:extend-env
;     (x)
;     (#(struct:num-val 100))
;     #(struct:extend-env
;       (f)
;       (#(struct:proc-val #<procedure:...cedure/value_of.scm:74:4>))
;       #(struct:extend-env (x) (#(struct:num-val 200)) #(struct:empty-env)))))
; ==================================================== p77例子 end

; (eopl:pretty-print (run
; "
; unpack = emptylist
; in -(100,10)
; "
; ))
;                             
; (eopl:pretty-print (run
; "
; unpack x = cons(3,emptylist)      
; in -(x,10)
; "
; ))
; 
; (eopl:pretty-print (run
; "
; let u = 7
; in unpack x y = cons(u,cons(3,emptylist))      
;    in -(x,y)
; "
; ))
; 
; ; 812
; (eopl:pretty-print (run
; "
; let j = 10 k = 20 l = 30
; in unpack x y z = cons(-(j,1),cons(-(k,1),cons(-(l,1),emptylist)))
;    in *(+(x,y),z)
; "
; ))

(eopl:pretty-print (scan&parse
"
let x = 200
in let f = proc (z) -(z,x) in let g = proc (z) -(z,x)
in let x = 100
in -((f 1), (g 1))
"
))
; #(struct:a-program
;   #(struct:let-exp
;     (x)
;     (#(struct:const-exp 200))
;     #(struct:let-exp
;       (f)
;       (#(struct:proc-exp
;          z
;          #(struct:diff-exp #(struct:var-exp z) #(struct:var-exp x))))
;       #(struct:let-exp
;         (g)
;         (#(struct:proc-exp
;            z
;            #(struct:diff-exp #(struct:var-exp z) #(struct:var-exp x))))
;         #(struct:let-exp
;           (x)
;           (#(struct:const-exp 100))
;           #(struct:diff-exp
;             #(struct:call-exp #(struct:var-exp f) #(struct:const-exp 1))
;             #(struct:call-exp #(struct:var-exp g) #(struct:const-exp 1))))))))
(newline)
(eopl:pretty-print (run
"
let x = 200
in let f = proc (z) -(z,x) 
  in let x = 100
    in let g = proc (z) -(z,x)
      in -((f 1), (g 1))
"
))


; 多参数怎么做? todo , 会导致目前代码有什么变动?