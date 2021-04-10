#lang eopl

; ================= env
(define (identifier? var)
  (symbol? var))
; todo, 可能需要改变, 目前是什么都能放
(define (scheme-val? var)
  #t)
(define-datatype env-exp env-exp?
  (empty-env)
  (extend-env
    (ident identifier?)
    (scheme_val scheme-val?)
    (env_exp env-exp?)))

(define (apply-env env s)
  (cases env-exp env
    (empty-env ()
      (eopl:error 'apply-env "~s not found" s))
      ;(error "not find " s))
    (extend-env (ident scheme_val env_exp)
      (if (eqv? ident s)
          scheme_val
          (apply-env env_exp s)))))

; ================= Syntax data types for the LET language
(define-datatype program program? 
  (a-program 
    (exp1 expression?)))

(define-datatype expression expression?
 (const-exp 
  (num number?))
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

(eopl:pretty-print (const-exp 100))
; #(struct:const-exp 100
(eopl:pretty-print (diff-exp (const-exp 100)
                             (const-exp 90)))
; #(struct:diff-exp #(struct:const-exp 100) #(struct:const-exp 90))

; ExpVal好像表示的是具体的值
; ================== ExpVal(内部表示) <=> Int/Bool的互转
; ExpVal, Expressed values for the LET language
(define-datatype expval expval?
; expression和expval怎么交互的?
; value-of的返回会送给expval->*进行解析? 也就是说value-of返回的也只是内部表示
  (num-val 
    (num number?)) 
  (bool-val 
    (bool boolean?)))

(define expval->num
  (lambda (val) 
    (cases expval val 
      (num-val (num) 
        num) 
      (else 
        (eopl:error 'num "expval-num need num-val")))))
(define expval->bool
  (lambda (val) 
    (cases expval val 
      (bool-val (bool) 
        bool) 
      (else 
        (eopl:error 'bool "expval-bool need bool-val")))))            

; ExpVal具体长啥样? 如下: 其实就是内部的一种表示...
(eopl:pretty-print (num-val 100))
; #(struct:num-val 100)
(eopl:pretty-print (bool-val #f))
; #(struct:bool-val #f)
(eopl:pretty-print (expval->num (num-val 100)))
; 100
; 不符合的会报错
; (eopl:pretty-print (expval->bool (num-val 100)))
(eopl:pretty-print (expval->bool (bool-val #t)))
; #t



; num-val在哪定义的?
; ! 注意, 在env里面, val已经是(num-val *)这种包装过一层的内部表示了
(define init-env
  (lambda () 
    (extend-env 'i 
                (num-val 1) 
                (extend-env 'v 
                            (num-val 5) 
                            (extend-env 'x 
                                        (num-val 10) (empty-env))))))
(define init_env_01 (init-env))
(eopl:pretty-print init_env_01)


; ================== Interpreter for the LET language
; 输出program
;  (define run
;    (lambda (string) 
;      (value-of-program (scan&parse string))))
;  
;  (define value-of-program
;    (lambda (pgm)   ; pgm代表什么?
;      (cases program pgm 
;        (a-program (exp1) 
;          (value-of exp1 (init-env))))))

; 测试'v对应的输出是什么.
; (newline)
; (display (apply-env (init-env) 'v))
; #(struct:num-val 5)
; value-of, 输入是expression返回的是expval
(define value-of
  (lambda (exp env)
    (cases expression exp
      (const-exp (num) 
        (num-val num))
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
          (value-of body (extend-env var val1)))))))

; 测试zero?
; (display (zero? 0))  ; #t
; (display (zero? 100))  ; #f

(define test_exp_01 (diff-exp (var-exp 'x) 
                              (var-exp 'v)))
(newline)
(eopl:pretty-print test_exp_01)
(eopl:pretty-print (value-of test_exp_01 init_env_01))