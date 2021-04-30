(module interp (lib "eopl.ss" "eopl")

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program 
           value-of/k)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ; Program → FinalAnswer
  (define value-of-program
    (lambda (pgm) 
      (cases program pgm 
        (a-program (exp1) 
          (value-of/k exp1 (init-env) (end-cont) (end-cont)))))) ; end-cont constructor

  (define value-of/k
    (lambda (exp env cont handlers_cont)  ; 正常值通过apply-cont送入cont, 异常值通过raise-exp然后送入handlers_cont(可能是一串hanlder), 不管在两条路中的哪条, 都有足够信息运行到结束
      (cases expression exp

        (const-exp (num)
          (apply-cont cont
                      (num-val num)
                      handlers_cont))

        (var-exp (var) 
          (apply-cont cont
                      (apply-env env var)
                      handlers_cont))

        (diff-exp (exp1 exp2)  ; cont是diff-exp的continuation
          ; value-of exp1, exp2之后再去计算某些东西, 然后再进入原先的cont
          (value-of/k exp1
                      env
                      (diff1-cont exp2 env cont)
                      handlers_cont))

        ; cont是zero?-exp的continuation
        (zero?-exp (exp1)
          (value-of/k exp1        ; value-of/k exp1内部会触发apply-cont
                      env 
                      (zero1-cont cont)
                      handlers_cont))
              
        (if-exp (exp1 exp2 exp3)
          (value-of/k exp1
                      env
                      (if-test-cont exp2 exp3 env cont)
                      handlers_cont))

        (let-exp (var exp1 body)
          (value-of/k exp1  
                      env
                      (let-exp-cont var body env cont)
                      handlers_cont))

        (proc-exp (var body)
          (apply-cont cont
                      (proc-val (procedure var body env))
                      handlers_cont))

        (call-exp (rator rand)
          (value-of/k rator
                      env
                      (rator-cont rand env cont)
                      handlers_cont))

        (letrec-exp (p-name b-var p-body letrec-body)
          (value-of/k letrec-body
                      (extend-env-rec p-name b-var p-body env)
                      cont
                      handlers_cont))

        (cons-exp (exp1 exp2)
          (value-of/k exp1
                      env
                      (cons-exp-cont-1 exp2 env cont)
                      handlers_cont))
        
        (car-exp (exp1)
          (value-of/k exp1
                      env
                      (car-exp-cont cont)
                      handlers_cont))
        (cdr-exp (exp1)
          (value-of/k exp1
                      env
                      (cdr-exp-cont cont)
                      handlers_cont))

        (emptylist-exp ()
          (apply-cont cont
                      (emptylist-val)
                      handlers_cont))

        (null?-exp (exp1)
          (value-of/k exp1
                      env
                      (null?-exp-cont cont)
                      handlers_cont))

        (list-exp (exps)
          (if (null? exps)  ; 测试list为空. todo
              (apply-cont cont (emptylist-val) handlers_cont)
              (let ((car_exps (car exps))
                    (remaining_exps (cdr exps)))
                (value-of/k car_exps
                            env
                            (list-exp-cont-car remaining_exps env cont)
                            handlers_cont))))

        (try-exp (exp1 var handler_exp)
          ;(eopl:pretty-print "In try-exp")
          ;(eopl:pretty-print cont)
          (value-of/k exp1
                      env
                      (try-cont var handler_exp env cont handlers_cont)
                      (try-cont var handler_exp env cont handlers_cont)))  ; 形成一串exception handlers

        (raise-exp (exp1)
          ;(eopl:pretty-print "In raise-exp")
          ;(eopl:pretty-print cont)
          ;(eopl:pretty-print "==== In raise-exp ====")
          (value-of/k exp1  ; exception, exp1本身一般是一个正常值
                      env
                      (raise-cont cont handlers_cont)
                      handlers_cont))  ;这个参数有什么用? 除非是raise raise 2这种...写法
                                 ; raise raise 2, 分别是cont + handlers_cont
                                 ; raise 2, 分别是(raise-cont cont trycont) + handlers_cont, 最终还是会来到trycont

        )))

  (define (apply-cont cont val handlers_cont) 
    (cases continuation cont 
        (end-cont () 
          (begin 
            (eopl:printf "End of computation.~%") 
            val)) 
        (zero1-cont (saved_cont) 
          (apply-cont saved_cont 
                      (bool-val (zero? (expval->num val)))
                      handlers_cont))
        (let-exp-cont (var body saved_env saved_cont)
          (value-of/k body
                      (extend-env (list var)
                                  (list val) 
                                  saved_env)
                      saved_cont
                      handlers_cont
                      ))
        (if-test-cont (exp2 exp3 saved_env saved_cont)
          (if (expval->bool val)
              (value-of/k exp2 saved_env saved_cont handlers_cont)
              (value-of/k exp3 saved_env saved_cont handlers_cont)))
        (diff1-cont (exp2 saved_env saved_cont)
          (value-of/k exp2
                      saved_env
                      (diff2-cont val ; exp1的结果存起来
                                  saved_cont)
                      handlers_cont))         
        (diff2-cont (val1 saved_cont)
          (let ((num1 (expval->num val1))
                (num2 (expval->num val)))
            (apply-cont saved_cont  ; 得到diff-exp的结果后, 返回原先diff-exp的continuation(即saved_cont)
                        (num-val (- num1 num2))
                        handlers_cont)))
        (rator-cont (rand saved_env saved_cont)
          (value-of/k rand
                      saved_env
                      (rand-cont val saved_cont)
                      handlers_cont))
        (rand-cont (rator saved_cont)
          (let ((proc1 (expval->proc rator)))
            (apply-procedure proc1 val saved_cont handlers_cont)))

        (cons-exp-cont-1 (exp2 saved_env saved_cont)
          (value-of/k exp2
                      saved_env
                      (cons-exp-cont-2 val saved_cont)
                      handlers_cont))
        (cons-exp-cont-2 (val1 saved_cont)
          (apply-cont saved_cont
                      (pair-val val1
                                val)
                      handlers_cont))
        (car-exp-cont (saved_cont)
          (apply-cont saved_cont
                      (car (expval->pair val))
                      handlers_cont))

        (cdr-exp-cont (saved_cont)
          (apply-cont saved_cont
                      (cdr (expval->pair val))
                      handlers_cont))

        (null?-exp-cont (saved_cont)
          (apply-cont saved_cont
                      (expval-emptylist? val)
                      handlers_cont))

        (list-exp-cont-car (remaining_exps saved_env saved_cont)
          (value-of/k (list-exp remaining_exps)   ; 内部构造list-exp
                      saved_env
                      (list-exp-cont-remaining val saved_cont)
                      handlers_cont))

        (list-exp-cont-remaining (car_val saved_cont)
          (apply-cont saved_cont
                      (pair-val car_val
                                val)
                      handlers_cont))
        
        (try-cont (var handler_exp saved_env saved_cont saved_handlers_cont)
          (apply-cont saved_cont val handlers_cont))

        (raise-cont (saved_cont saved_handlers_cont)
          (apply-handler val saved_handlers_cont))  ;直接获得saved_handlers_cont

    ))

  (define apply-handler
    (lambda (val cont)   ; 参数顺序无所谓吧
      (cases continuation cont 
        (try-cont (var handler_exp saved_env saved_cont saved_handlers_cont); saved_trycont一串handler
          (value-of/k handler_exp   ; 这一层的exception handler
                      (extend-env (list var) 
                                  (list val) 
                                  saved_env)
                      saved_cont             ; 在这一层这一个exception处理完成之后就是个正常值了, 然后该向saved_cont走, 哪怕是套了多层, 在比较外层的handler处理, 最终完成处理后因为try-cont里保存了正常路径超哪走, 所以最后还是会回到正常路径
                      saved_handlers_cont))  ; 有可能在handler_exp把异常重新raise
        (end-cont () 
          (report-uncaught-exception))   
        (else
          (eopl:error 'apply-handler "other cont"))
        )))

  (define (report-uncaught-exception) 
    (eopl:error 'report-uncaught-exception "not find exception handler..."))

  (define apply-procedure
    (lambda (proc1 arg cont handlers_cont)
      (cases proc proc1
        (procedure (var body saved_env)
          (value-of/k body 
                      (extend-env (list var) 
                                  (list arg)
                                  saved_env)
                      cont
                      handlers_cont)))))
  
)