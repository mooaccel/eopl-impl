(module interp (lib "eopl")
  
  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")  ; initialize-store!
  
  (provide value-of-program value-of/k result-of/k instrument_let instrument_newref)  
  ; instrument_newref在store.scm定义, 可以再更上层top.scm使用, 在中间这一层用provide

  (define instrument_let (make-parameter #f))
  ;; say (instrument_let #t) to turn instrumentation on.
  ;;     (instrument_let #f) to turn it off again.


;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;
  ;; value-of-program : Program -> ExpVal
  ;; Page: 110
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)               ; new for explicit refs.
      (cases program pgm
        (a-program (stat1)
          (result-of/k stat1 (init-env) (end-commandcont))))))

  (define value-of/k
    (lambda (exp env cont)  ; cont是exp的continuation
      (cases expression exp
        (const-exp (num)
          (apply-cont cont
                      (num-val num)))

        (var-exp (var) 
          (apply-cont cont
                      (deref (apply-env env var))))

        (diff-exp (exp1 exp2)
          (value-of/k exp1
                      env
                      (diff1-cont exp2 env cont))) ; 多久触发这个diff1-cont? 在exp1得到结果后, apply-cont
        (addition-exp (exp1 exp2)
          (value-of/k exp1
                      env
                      (addition1-cont exp2 env cont)))
        (multiplication-exp (exp1 exp2)
          (value-of/k exp1
                      env
                      (multiplication1-cont exp2 env cont)))

        (zero?-exp (exp1)
          (value-of/k exp1        ; value-of/k exp1内部会触发apply-cont
                      env 
                      (zero1-cont cont)))

        (not-exp (exp1)
          (value-of/k exp1
                      env 
                      (not-exp-cont cont)))
              
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

        ;(letrec-exp (p-name b-var p-body letrec-body)
        ;  (value-of/k letrec-body
        ;              (extend-env-rec p-name b-var p-body env)
        ;              cont))

        )))

  ; 返回refs
  (define (new-uninitialized-references-aux idents)
    (if (null? idents)
        '()
        (cons (newref (num-val 0))
              (new-uninitialized-references-aux (cdr idents)))))

  (define result-of/k
    (lambda (stmt env commandcont)  ; commandcont是stmt的command continuation
      (cases statement stmt
        (set-stat (ident exp1)
          (value-of/k exp1
                      env
                      (set-rhs-cont ident env commandcont)))
        (print-stat (exp1)
          (value-of/k exp1
                      env
                      (print-cont commandcont)))
        (block-stat (stats)
          (if (null? stats)
              (apply-command-cont commandcont)
              (let ((car_stats (car stats)))
                 (begin
                    (result-of/k car_stats 
                                 env
                                 (block-stats-commandcont (cdr stats) 
                                                          env
                                                          commandcont))))))
        (if-stat (exp1 then_stat else_stat)
          (value-of/k exp1
                      env
                      (if-test-cont then_stat 
                                    else_stat 
                                    env 
                                    commandcont)))

        (while-stat (exp1 stat1)
          (value-of/k exp1
                      env
                      (while-cont exp1 stat1 env commandcont)))

        (declare-stat (idents stat1)  ; 如果idents为空, todo
          (let ((refs (new-uninitialized-references-aux idents)))   ; store上制造存储位置, 然后放入env
            (result-of/k stat1 
                         (extend-env idents refs env)
                         commandcont)))
    )))

  (define (apply-cont cont val) 
      (cases continuation cont 
          ;(end-cont () 
          ;  (begin 
          ;    (eopl:printf "End of computation.~%") 
          ;    val)) 
          (zero1-cont (saved_cont) 
            (apply-cont saved_cont 
                        (bool-val (zero? (expval->num val)))))
          (not-exp-cont (saved_cont) 
            (apply-cont saved_cont
                        (if (expval->bool val)
                            (bool-val #f)
                            (bool-val #t))))
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
          (addition1-cont (exp2 env saved_cont)
            (value-of/k exp2
                        env
                        (addition2-cont val
                                        saved_cont)))         
          (addition2-cont (val1 saved_cont)
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val)))
              (apply-cont saved_cont
                          (num-val (+ num1 num2)))))
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
          (let-exp-cont (var body saved_env saved_cont)
            (value-of/k body
                        (extend-env var 
                                    (newref val) 
                                    saved_env)
                        saved_cont))
          (rator-cont (rand env saved_cont)
            (value-of/k rand
                        env
                        (rand-cont val saved_cont)))
          (rand-cont (rator saved_cont)
            (let ((proc1 (expval->proc rator)))
              (apply-procedure/k proc1 val saved_cont)))
          

          ; 和statement有关的
          (set-rhs-cont (ident env saved_commandcont)
            (begin
              (setref! (apply-env env ident)
                       val)
              (apply-command-cont saved_commandcont)))
          (print-cont (saved_commandcont)
            (begin
              (eopl:pretty-print val)
              (apply-command-cont saved_commandcont)))

          (if-test-cont (then_stat else_stat env saved_commandcont)
            (if (expval->bool val)
                (result-of/k then_stat env saved_commandcont)
                (result-of/k else_stat env saved_commandcont)))

          (while-cont (exp1 stat1 env commandcont)
            (if (expval->bool val)
                (result-of/k stat1  ; 处理完stat1之后, 进入while-proceed-commandcont
                             env
                             (while-proceed-commandcont exp1 stat1 env commandcont))
                (apply-command-cont commandcont)))

      ))

  (define (apply-command-cont commandcont)
    (cases commandcontinuation commandcont
      (end-commandcont () 
        (begin 
          (eopl:printf "command continuation, end of computation...~%") 
          'end-commandcont))
      (block-stats-commandcont (stats env saved_commandcont)
        (result-of/k (block-stat stats)
                     env
                     saved_commandcont))
      (while-proceed-commandcont (exp1 stat1 env commandcont)
        (result-of/k (while-stat exp1 stat1)
                     env
                     commandcont))
  ))

  (define apply-procedure/k
    (lambda (proc1 val cont)
      (cases proc proc1
        (procedure (var body saved_env)
      	    (let ((new_env (extend-env (list var)
                                       (list (newref val))
                                       saved_env)))
              ; 在apply-procedure内部debug输出
      	      (when (instrument_let)  ; 改名叫instrument_apply_procedure
      		          (begin
      		            (eopl:printf
      		              "entering body of proc ~s with env =\n"
      		              var)
      		            (pretty-print (env->list new_env))
                      (eopl:printf "store =\n")
                      (pretty-print (store->readable (get-store-as-list)))
                      (eopl:printf "\n")
                      ))
              (value-of/k body 
                          new_env 
                          cont))))))


  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
        (lambda (p)
          (list
            'ref:
            (car p)
            'content_of_ref:
            (expval->printable (cadr p))))
        l)))
 
  )


