(module interp (lib "eopl")
  
  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")  ; initialize-store!
  (require "scheduler.scm")
  (require "mutex.scm")
  
  (provide value-of-program 
           value-of/k 
           instrument_let 
           instrument_newref

           instrument_value_of_k
           ;instrument_apply_cont
           instrument_ready_queue_size
           ) 
  ; instrument_newref在store.scm定义, 可以再更上层top.scm使用, 在中间这一层用provide

  (define instrument_let (make-parameter #f))

  (define instrument_value_of_k (make-parameter #f))
  ;(define instrument_apply_cont (make-parameter #f))

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program * Int -> ExpVal
  (define value-of-program
    (lambda (timeslice pgm)
      (initialize-store!)
      (initialize-scheduler! timeslice)
      (cases program pgm
        (a-program (exp1)
          (value-of/k exp1
                      (init-env)
                      (end-main-thread-cont))))))

  (define value-of/k
    (lambda (exp env cont)  ; cont是exp的continuation
      (begin
        (if (instrument_value_of_k)
            (begin
              (eopl:printf "Enter value-of/k... ~%exp = ~s ~%" exp)
              (eopl:printf "cont = ~s ~%" cont))
            'ignore)
        (cases expression exp
          (const-exp (num)
            (apply-cont cont
                        (num-val num)))

          (var-exp (var) 
            (apply-cont cont
                        (deref (apply-env env var))))
          
          (const-list-exp (numbers)
            (apply-cont cont
                        (list-val (map num-val 
                                       numbers))))

          ;(zero?-exp (exp1)
          ;  (value-of/k exp1
          ;              env 
          ;              (zero1-cont cont)))
  
          (diff-exp (exp1 exp2)
            (value-of/k exp1
                        env
                        (diff1-cont exp2 env cont)))

          (multiplication-exp (exp1 exp2)
            (value-of/k exp1
                        env
                        (multiplication1-cont exp2 env cont)))

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
  
          (letrec-exp (proc_name bound_var proc_body letrec_body)
            (value-of/k letrec_body
                        (extend-env-rec proc_name bound_var proc_body env)
                        cont))

          (set-exp (ident exp1)
            (value-of/k exp1
                        env
                        (set-rhs-cont ident env cont)))

          (begin-exp (exp1 remaining_exps)
            (value-of/k exp1
                        env
                        (begin-exp-cont remaining_exps env cont)))

          
          ; spawn-exp的exp1应该是proc-exp吧?
          (spawn-exp (exp1)
            (value-of/k exp1
                        env
                        (spawn-cont cont)))            

          (mutex-exp ()
            (apply-cont cont (mutex-val (new-mutex))))
          
          ; wait-exp的cont是什么?
          (wait-exp (mutex_exp)
            (value-of/k mutex_exp
                        env
                        (wait-cont cont)))

          (signal-exp (mutex_exp)
            (value-of/k mutex_exp
                        env
                        (signal-cont cont)))

          (yield-exp ()
            (place-on-ready-queue!
              (lambda () 
                (apply-cont cont
                            (num-val 99))))
            (run-next-thread))

          (unop-exp (unary_op exp)
            (value-of/k exp 
                        env
                        (unop-arg-cont unary_op cont)))

        ))))

  (define (apply-cont cont val) 
    ;(begin
    ;  (if (instrument_apply_cont)
    ;      (begin
    ;        ;(eopl:printf "Enter apply-cont ~s ~s ]~%" (expval->readable val) (cont->readable cont)))
    ;        (eopl:printf "= apply-cont, SEND ~s ~%"val)
    ;        (eopl:printf "= TO ~s ~%" cont))
    ;      'ignore)
    (if (time-expired?)
        (begin
          (eopl:pretty-print "time-expired? yes")
          (place-on-ready-queue!
            (lambda () (apply-cont cont val)))
          (run-next-thread))
        (begin
          (decrement-timer!)
          (cases continuation cont 
              ;(end-cont () 
              ;  (begin 
              ;    (eopl:printf "End of computation.~%") 
              ;    val)) 
              (end-main-thread-cont ()
                (set-final-answer! val)
                (run-next-thread))
              (end-subthread-cont ()  ; 子过程返回的值最终被遗弃了, 没有用它
                (run-next-thread))
              ;(zero1-cont (saved_cont) 
              ;  (apply-cont saved_cont 
              ;    (bool-val (zero? (expval->num val)))))
              (let-exp-cont (var body saved_env saved_cont)
                (value-of/k body
                            (extend-env var 
                                        (newref val) 
                                        saved_env)
                            saved_cont))
              (if-test-cont (exp2 exp3 saved_env saved_cont)
                (if (expval->bool val)
                    (value-of/k exp2 saved_env saved_cont)
                    (value-of/k exp3 saved_env saved_cont)))
              (diff1-cont (exp2 env saved_cont)
                (value-of/k exp2
                            env
                            (diff2-cont val ; exp1的结果存起来
                                        saved_cont)))         
              (diff2-cont (val1 saved_cont)
                (let ((num1 (expval->num val1))
                      (num2 (expval->num val)))
                  (apply-cont saved_cont  ; 得到diff-exp的结果后, 返回原先diff-exp的continuation(即saved_cont)
                              (num-val (- num1 num2)))))
              (multiplication1-cont (exp2 env saved_cont)
                (value-of/k exp2
                            env
                            (multiplication2-cont val
                                                  saved_cont)))
              (multiplication2-cont (val1 saved_cont)
                (let ((num1 (expval->num val1))
                      (num2 (expval->num val)))
                  (apply-cont saved_cont
                              (num-val (* num1 num2)))))
              (rator-cont (rand env saved_cont)
                (value-of/k rand
                            env
                            (rand-cont val saved_cont)))
              (rand-cont (rator saved_cont)
                (let ((proc1 (expval->proc rator)))
                  (apply-procedure/k proc1 val saved_cont)))

              (set-rhs-cont (ident env saved_cont)
                (begin
                  (setref! (apply-env env ident)
                           val)
                  (apply-cont saved_cont (num-val 34))))  ; 给原先的cont随便返回一个值就行
                                                          ; 或者改成返回ref设置之前, 之后的值

              (begin-exp-cont (remaining_exps saved_env saved_cont)
                (if (null? remaining_exps)
                    (apply-cont saved_cont val)
                    (value-of/k (begin-exp (car remaining_exps)  ; 如果进入这个分支, 那么val不用管了, 不需要了
                                           (cdr remaining_exps))
                                saved_env
                                saved_cont)))

              (spawn-cont (saved_cont)
                (let ((proc1 (expval->proc val)))  ; 这个proc有参数么?
                  (place-on-ready-queue!
                    (lambda () 
                      (apply-procedure/k proc1 
                                         (num-val 28) 
                                         (end-subthread-cont)))) 
                  (apply-cont saved_cont (num-val 73))  ; 随便返回的73
                ))

              (wait-cont (saved_cont)
                (wait-for-mutex (expval->mutex val)
                                (lambda () (apply-cont saved_cont (num-val 52))))) ; 这两个52/53是随意的值, 因为在begin-exp-cont会给抛弃掉

              (signal-cont (saved_cont)
                (signal-mutex (expval->mutex val)
                                (lambda () (apply-cont saved_cont (num-val 53))))) ; 如果signal之后还有值呢? end-cont抛弃?

              (unop-arg-cont (unary_op saved_cont)
                (apply-unop unary_op
                            val 
                            saved_cont))

      ))))

  (define apply-procedure/k
    (lambda (proc1 val cont)
      (cases proc proc1
        (procedure (var body saved_env)
      	    (let ((new_env (extend-env var 
                                       (newref val) 
                                       saved_env)))
      	      (when (instrument_let)
      		          (begin
      		            (eopl:printf
      		              "entering body of proc ~s with env =~%"
      		              var)
      		            (pretty-print (env->list new_env))
                      (eopl:printf "store =~%")
                      (pretty-print (store->readable (get-store-as-list)))
                      (eopl:printf "~%")))
              (value-of/k body 
                          new_env
                          cont))))))

  (define apply-unop
    (lambda (unary_op val cont)
      (cases unop unary_op

        (zero?-unop ()
          (apply-cont cont
            (bool-val
              (zero? (expval->num val)))))

        (car-unop ()
          (let ((lst (expval->list val)))
            (apply-cont cont (car lst))))
        (cdr-unop ()
          (let ((lst (expval->list val)))
            (apply-cont cont (list-val (cdr lst)))))
        (null?-unop ()
          (apply-cont cont 
            (bool-val (null? (expval->list val)))))

        (print-unop ()
          (begin
            (eopl:printf "~a~%" (expval->num val))
            (apply-cont cont (num-val 1))))  ; 随便返回一个值给cont

        )))



  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
        (lambda (p)
          (cons
            (car p)
            (expval->printable (cadr p))))
        l)))

  )