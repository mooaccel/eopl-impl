#lang eopl

(require "./util.scm")

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
    (saved_env environment?)
    ))

(define (apply-procedure proc1 val) 
  (cases proc proc1 
    (procedure (var body saved_env)
      ; (eopl:pretty-print "debug apply-procedure ...")
      ; (eopl:pretty-print "proc1---------------")
      ; (eopl:pretty-print proc1)
      ; (eopl:pretty-print "val---------------")
      ; (eopl:pretty-print val)
      ; (eopl:pretty-print "proc1 contain var body saved_env:")
      ; (eopl:pretty-print "var---------------")
      ; (eopl:pretty-print var)
      ; (eopl:pretty-print "body---------------")
      ; (eopl:pretty-print body)
      ; (eopl:pretty-print "saved_env---------------")
      ; (eopl:pretty-print saved_env)
      (eopl:printf "apply-procedure, enter: ~a = ~a\n" var val)
      (let ((value (value-of body 
                             (extend-env (list var) 
                                         (list val) 
                                         saved_env))
                                         ))
          (eopl:printf "apply-procedure, exit: value = ~a\n" value)
          value
                            ))))

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
   (rand expression?)))

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

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(define value-of-program
  (lambda (pgm)
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
        (value-of body (extend-env vars val_exps env))))
    (proc-exp (var body) 
      ; (eopl:pretty-print "debug value-of proc-exp ...")
      ; (eopl:pretty-print "var in create time---------------")
      ; (eopl:pretty-print var)
      ; (eopl:pretty-print "body in create time---------------")
      ; (eopl:pretty-print body)
      ; (eopl:pretty-print "env in create time---------------")
      ; (eopl:pretty-print env)
      (let ((free_variables (free-variable body (list var))))
        (proc-val (procedure var 
                             body 
                             ;env))
                             (optimized-env env free_variables)))
        ))
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
))

; 解析body, 找var-exp, 然后找free variable
; 把free variable在env中的值提取出来

; 输入bound_vars, list of indentifier, 需要排除在外的(这里面的都不是free variable)
; 返回list of free variable
(define (free-variable body_expression bound_vars)
  ; (eopl:pretty-print "free-variable debug...")
  ; (eopl:pretty-print "body_expression -----------")
  ; (eopl:pretty-print body_expression)
  ; (eopl:pretty-print "bound_vars -----------")
  ; (eopl:pretty-print bound_vars)
  (cases expression body_expression
    (const-exp (num)
      '())
    (var-exp (v) 
      (if (not (memq v bound_vars))
          (list v)   ; 不在bound_var里, 而且使用了, 说明是free variable
          '()))
    (zero?-exp (exp1)
      (free-variable exp1 bound_vars))
    (diff-exp (exp1 exp2)
      (append (free-variable exp1 bound_vars)
              (free-variable exp2 bound_vars)))
    (addition-exp (exp1 exp2)
      (append (free-variable exp1 bound_vars)
              (free-variable exp2 bound_vars)))
    (multiplication-exp (exp1 exp2)
      (append (free-variable exp1 bound_vars)
              (free-variable exp2 bound_vars)))
    (if-exp (exp1 exp2 exp3)
      (append (append (free-variable exp1 bound_vars)
                      (free-variable exp2 bound_vars))
              free-variable exp3 bound_vars))
    (let-exp (vars exps body)
      ; 需要处理exps, 原理见ch3/3_26/3_26_free_variable分析.png
      (append (accumulate append 
                          '()
                          (map (lambda (exp_item) 
                                 (free-variable exp_item bound_vars))  ; exps里不可能使用vars吧... vars就是依赖exps定义的
                               exps))
              (free-variable body (append vars bound_vars))))
    (proc-exp (var body) 
      (free-variable body (append (list var)
                                  bound_vars)))
    (call-exp (rator rand)
      (append (free-variable rator bound_vars)
              (free-variable rand bound_vars)))
))
  
; 把env里的key在free_variables的重新组合成一个新的env(只有一层)
(define (optimized-env env free_variables)
  (define (obtain-pair-from-two-list-aux l1 l2)
    (if (null? l1)
        '()
        (cons (list (car l1)
                    (car l2))
              (obtain-pair-from-two-list-aux (cdr l1) 
                                             (cdr l2)))))
  ; 返回(pre_vars_accumu, pre_vals_accumu)
  (define (obtain-new-vars-vals-aux candidate_pairs cur_free_vars pre_vars pre_vals)

    (define (vars-vals-accumu-aux pre_vars_acc pre_vals_acc remaining_pairs)
      (if (null? remaining_pairs)
          (cons pre_vars_acc pre_vals_acc)
          (let ((pr (car remaining_pairs)))
            ; (eopl:pretty-print "pr")
            ; (eopl:pretty-print pr)
            (vars-vals-accumu-aux (cons (car pr) pre_vars_acc)
                                  (cons (cadr pr) pre_vals_acc)
                                  (cdr remaining_pairs)))))

    (let ((free_pairs (filter (lambda (pair) 
                                    (memq (car pair) cur_free_vars))
                              candidate_pairs)))
      (vars-vals-accumu-aux pre_vars pre_vals free_pairs)
    ))

  ; 返回一个一层的新env, 从最底层的new_vars new_vals构造
  (define (optimized-env-aux env_aux free_variables_aux new_vars new_vals)
    (if (null? free_variables_aux)  ; 有可能不需要走到env_aux的最里层
        (extend-env new_vars new_vals (empty-env))
        (cases environment env_aux
          (empty-env ()
            (extend-env new_vars new_vals (empty-env)))
          (extend-env (idents scheme_vals saved_env)
            ; 筛选出当前层里面的free variable
            (let ((cur_layer_free_variables (filter (lambda (v) 
                                                    (memq v idents))
                                            free_variables_aux)))
              (let ((new_vars_vals_pair (obtain-new-vars-vals-aux (obtain-pair-from-two-list-aux idents
                                                                                                scheme_vals)
                                                                 cur_layer_free_variables
                                                                 new_vars
                                                                 new_vals)))
                  (let ((remaining_free_variables (filter (lambda (fv) 
                                                            (not (memq fv cur_layer_free_variables)))
                                                          free_variables_aux)))
                    (optimized-env-aux saved_env
                                       remaining_free_variables
                                       (car new_vars_vals_pair)                                  
                                       (cdr new_vars_vals_pair)))))))
        ))

  (optimized-env-aux env free_variables '() '()))

; 应该返回(m a)
(eopl:pretty-print (free-variable
(let-exp (list 'i)
         (list (addition-exp (var-exp 'm)
                             (const-exp 20)))
         (addition-exp (diff-exp (var-exp 'x)
                                 (var-exp 'a))
                       (var-exp 'i)))
(list 'x)
))

; ; 测试optimized-env 
; (define test_init_env (extend-env (list 'q 'q1)
;                                   (list (num-val 10) (num-val 11))
;                                   (extend-env (list 'w 'w1 'w2)
;                                               (list (num-val 20) (num-val 21) (num-val 22))
;                                               (extend-env (list 'e)
;                                                           (list (num-val 30))
;                                                           (extend-env (list 'r)
;                                                                       (list (num-val 40))
;                                                                       (empty-env))))))
; (eopl:pretty-print
;   (optimized-env test_init_env '(w e w2 q1))
; )

; 使用优化后的env, 如果想看细节, 打开那些注释, 然后只运行下面这个例子
; -114
(eopl:pretty-print (run
"
let unuse_variable_01 = 10
in let m = 100
   in let a = 3    
      in let p = proc (x) let i = +(m, 20) in +(-(x, a), i)
             a = 5    
         in -(a,(p 2))
"
))

;  ; 
;  ; 70
;  (eopl:pretty-print (run
;  "
;  let unuse_var_01 = 1000
;      ufsd = 2000
;      dfsdfsa = 3000
;  in 
;  let f = proc (x) proc (y) +(x,y)
;  in ((f 30) 40)
;  "
;  ))
;  
;  ; 30
;  (eopl:pretty-print (run
;  "
;  let unuse_var_01 = 1000
;      ufsdadfsaf = 2000
;      dfsdfsa = 3000
;  in
;  let f = proc (x) proc (y) -(x, -(0, y)) 
;  in ((f 10) 20)
;  "
;  ))
;  
;  ; 610
;  (eopl:pretty-print (run
;  "
;  let unuse_var_01 = 1000
;      ufsdadfsaf = 2000
;      dfsdfsa = 3000
;  in
;  let f = proc (x) proc (y) proc(z) +(x, *(y, z)) 
;  in (((f 10) 20) 30)
;  "
;  ))
;  
;  ; 579
;  (eopl:pretty-print (run
;  "
;  let unuse_var_01 = 1000
;      ufsdadfsaf = 2000
;      dfsdfsa = 3000
;  in
;  let f = proc (x) proc (y) proc(z) proc(m) +(-(+(x, *(y, z)), m), 9)
;  in ((((f 10) 20) 30) 40)
;  "
;  ))