(module interp (lib "eopl.ss" "eopl")

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program 
           value-of-program-v2
           value-of/k)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ; Program → FinalAnswer
  (define value-of-program
    (lambda (pgm) 
      (cases program pgm 
        (a-program (exp1) 
          (value-of/k exp1 (init-env) (end-cont)))))) ; end-cont constructor

  (define value-of-program-v2
    (lambda (pgm env cont) 
      (cases program pgm 
        (a-program (exp1) 
          (value-of/k exp1 env cont)))))

  (define value-of/k
    (lambda (exp env cont)  ; cont是exp的continuation
      (cases expression exp

        (const-exp (num)
          (apply-cont cont
                      (num-val num)))

        (var-exp (var) 
          (apply-cont cont
                      (apply-env env var)))

        (diff-exp (exp1 exp2)  ; cont是diff-exp的continuation
          ; value-of exp1, exp2之后再去计算某些东西, 然后再进入原先的cont
          (value-of/k exp1
                      env
                      (diff1-cont exp2 env cont))) ; 多久触发这个diff1-cont? 在exp1得到结果后, apply-cont

        ; cont是zero?-exp的continuation
        (zero?-exp (exp1)
          (value-of/k exp1        ; value-of/k exp1内部会触发apply-cont
                      env 
                      (zero1-cont cont)))
              
        (if-exp (exp1 exp2 exp3)
          (value-of/k exp1
                      env
                      (if-test-cont exp2 exp3 env cont)))

        (let-exp (var exp1 body)
          (value-of/k exp1  
                      env
                      (let-exp-cont var body env cont)))

        (proc-exp (var body)
          (apply-cont cont
                      (proc-val (procedure var body env))))

        (call-exp (rator rand)
          (value-of/k rator
                      env
                      (rator-cont rand env cont)))

        (letrec-exp (p-name b-var p-body letrec-body)
          (value-of/k letrec-body
                      (extend-env-rec p-name b-var p-body env)
                      cont))

        (cons-exp (exp1 exp2)
          (value-of/k exp1
                      env
                      (cons-exp-cont-1 exp2 env cont)))
        
        (car-exp (exp1)
          (value-of/k exp1
                      env
                      (car-exp-cont cont)))
        (cdr-exp (exp1)
          (value-of/k exp1
                      env
                      (cdr-exp-cont cont)))

        (emptylist-exp ()
          (apply-cont cont
                      (emptylist-val)))

        (null?-exp (exp1)
          (value-of/k exp1
                      env
                      (null?-exp-cont cont)))

        (list-exp (exps)
          (if (null? exps)  ; 测试list为空. todo
              (apply-cont cont (emptylist-val))
              (let ((car_exps (car exps))
                    (remaining_exps (cdr exps)))
                (value-of/k car_exps
                            env
                            (list-exp-cont-car remaining_exps env cont)))))
        (try-exp (exp1 var handler_exp)  ; cont是try-exp的continuation
          (eopl:pretty-print "In try-exp")
          (eopl:pretty-print cont)
          (value-of/k exp1  ; 正常值或exception.最后送入try-cont
                      env
                      (try-cont var handler_exp env cont)))  ; 进入try-cont的方式有两种, apply-cont或者apply-handler

        (raise-exp (exp1)  ; raise-exp的cont是什么? 感觉是一层一层的套在try-cont之上的
          (eopl:pretty-print "In raise-exp")
          (eopl:pretty-print cont)
          (eopl:pretty-print "==== In raise-exp ====")
          (value-of/k exp1  ; exception
                      env
                      (raise-cont cont)))

        )))

  (define (apply-cont cont val) 
    (cases continuation cont 
        (end-cont () 
          (begin 
            (eopl:printf "End of computation.~%") 
            val)) 
        (zero1-cont (saved_cont) 
          (apply-cont saved_cont 
            (bool-val (zero? (expval->num val)))))
        (let-exp-cont (var body saved_env saved_cont)
          (value-of/k body
                      (extend-env (list var)
                                  (list val) 
                                  saved_env)
                      saved_cont))
        (if-test-cont (exp2 exp3 saved_env saved_cont)
          (if (expval->bool val)
              (value-of/k exp2 saved_env saved_cont)
              (value-of/k exp3 saved_env saved_cont)))
        (diff1-cont (exp2 saved_env saved_cont)
          (value-of/k exp2
                      saved_env
                      (diff2-cont val ; exp1的结果存起来
                                  saved_cont)))         
        (diff2-cont (val1 saved_cont)
          (let ((num1 (expval->num val1))
                (num2 (expval->num val)))
            (apply-cont saved_cont  ; 得到diff-exp的结果后, 返回原先diff-exp的continuation(即saved_cont)
                        (num-val (- num1 num2)))))
        (rator-cont (rand saved_env saved_cont)
          (value-of/k rand
                      saved_env
                      (rand-cont val saved_cont)))
        (rand-cont (rator saved_cont)
          (let ((proc1 (expval->proc rator)))
            (apply-procedure proc1 val saved_cont)))

        (cons-exp-cont-1 (exp2 saved_env saved_cont)
          (value-of/k exp2
                      saved_env
                      (cons-exp-cont-2 val saved_cont)))
        (cons-exp-cont-2 (val1 saved_cont)
          (apply-cont saved_cont
                      (pair-val val1
                                val)))
        (car-exp-cont (saved_cont)
          (apply-cont saved_cont
                      (car (expval->pair val))))

        (cdr-exp-cont (saved_cont)
          (apply-cont saved_cont
                      (cdr (expval->pair val))))

        (null?-exp-cont (saved_cont)
          (apply-cont saved_cont
                      (expval-emptylist? val)))

        (list-exp-cont-car (remaining_exps saved_env saved_cont)
          (value-of/k (list-exp remaining_exps)   ; 内部构造list-exp
                      saved_env
                      (list-exp-cont-remaining val saved_cont)))

        (list-exp-cont-remaining (car_val saved_cont)
          (apply-cont saved_cont
                      (pair-val car_val
                                val)))
        
        (try-cont (var handler_exp saved_env saved_cont)
          (apply-cont val saved_cont))  ; 通过apply-cont try-cont val到这里, 说明是正常值

        (raise-cont (saved_cont)
          (apply-handler val saved_cont))  ;处理val, 即exception

    ))

  (define apply-handler
    (lambda (val cont) 
      (cases continuation cont 
        (try-cont (var handler_exp saved_env saved_cont) 
          (value-of/k handler_exp   ; exception handler
                      (extend-env (list var) 
                                  (list val) 
                                  saved_env)
                      saved_cont))
        (end-cont () 
          (report-uncaught-exception))   
        (zero1-cont (saved_cont) 
          (apply-handler val saved_cont)) 
        (let-exp-cont (var body saved_env saved_cont)
          (apply-handler val saved_cont))
        (if-test-cont (exp2 exp3 saved_env saved_cont)
          (apply-handler val saved_cont))
        (diff1-cont (exp2 saved_env saved_cont) 
          (apply-handler val saved_cont)) 
        (diff2-cont (val1 saved_cont) 
          (apply-handler val saved_cont))
        (rator-cont (rand saved_env saved_cont)
          (apply-handler val saved_cont))
        (rand-cont (rator saved_cont)
          (apply-handler val saved_cont))
        (cons-exp-cont-1 (exp2 saved_env saved_cont)
          (apply-handler val saved_cont))
        (cons-exp-cont-2 (val1 saved_cont)
          (apply-handler val saved_cont))
        (car-exp-cont (saved_cont)
          (apply-handler val saved_cont))
        (cdr-exp-cont (saved_cont)
          (apply-handler val saved_cont))
        (null?-exp-cont (saved_cont)
          (apply-handler val saved_cont))
        (list-exp-cont-car (remaining_exps saved_env saved_cont)
          (apply-handler val saved_cont))
        (list-exp-cont-remaining (car_val saved_cont)
          (apply-handler val saved_cont))
        (else   ; 可能处于raise-cont么???得思考下
          (eopl:printf 'apply-handler "other cont"))
        )))
  (define (report-uncaught-exception) 
    (eopl:printf 'report-uncaught-exception "not find exception handler..."))

  (define apply-procedure
    (lambda (proc1 arg cont)
      (cases proc proc1
        (procedure (var body saved_env)
          (value-of/k body 
                      (extend-env (list var) 
                                  (list arg)
                                  saved_env)
                      cont)))))
  
)