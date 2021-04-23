(module interp (lib "eopl")
  
  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")  ; initialize-store!
  (require "array.scm")
  
  (provide value-of-program value-of instrument_let instrument_newref)  
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
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 113
  (define value-of
    (lambda (exp env)
      (cases expression exp

        (const-exp (num) (num-val num))

        (var-exp (var)         ; var-exp和Explicit Refs语言有很大不同
          (deref (apply-env env var)))

        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))
      
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

        (let-exp (vars exps body)       
          (let ((vals_of_exps (map (lambda (exp_item) 
                                      (value-of exp_item env))
                                   exps)))
            (value-of body
              (extend-env vars 
                          (map (lambda (val_item) 
                                  (newref val_item))
                               vals_of_exps)
                          env))))

        (proc-exp (vars body)
          (proc-val (procedure vars body env)))

        (call-exp (rator rands)
          (let ((proc (expval->proc (value-of rator env)))
                (args (map (lambda (rand)
                              (value-of rand env))
                           rands)))
            (apply-procedure proc args)))

        (letrec-exp (listof_proc_name listof_bound_vars listof_proc_body letrec_body)
          (value-of letrec_body (extend-env-rec listof_proc_name
                                                listof_bound_vars
                                                listof_proc_body
                                                env)))

        (begin-exp (exp1 exps)
          (letrec 
            ((value-of-begins
               (lambda (e1 es)
                 (let ((v1 (value-of e1 env)))
                   (if (null? es)
                     v1
                     (value-of-begins (car es) (cdr es)))))))
            (value-of-begins exp1 exps)))

        ; (assign-exp (ident exp1)
        ;   (begin
        ;     (let ((val_of_exp1 (value-of exp1 env)))
        ;       (setref! (apply-env env ident) 
        ;                val_of_exp1))
        ;     (num-val 34)))
        ; 下面这样也可以...
        (assign-exp (ident exp1)
            (let ((val_of_exp1 (value-of exp1 env)))
              (setref! (apply-env env ident) 
                       val_of_exp1)))

        (newarray-exp (exp1 exp2)
          (let ((size_val (value-of exp1 env))
                (val2 (value-of exp2 env)))
              (array-val (make-array (expval->num size_val) val2))
            ))
        
        ; 返回expval
        (arrayref-exp (ident exp1)
          (let ((arr (expval->array (deref (apply-env env ident))))
                (idx_val (value-of exp1 env)))
            (arrayref arr (expval->num idx_val))))

        (arrayset-exp (ident exp1 exp2)
          (let ((arr (expval->array (deref (apply-env env ident))))
                (idx_val (value-of exp1 env))
                (val_of_exp2 (value-of exp2 env)))
            (begin
              (arrayset arr 
                        (expval->num idx_val)
                        val_of_exp2)
              (num-val 100))))  ; 随便什么值... 为了符合expression返回值的规范

        )))

  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; uninstrumented version
  ;;   (define apply-procedure
  ;;    (lambda (proc1 arg)
  ;;      (cases proc proc1
  ;;        (procedure (bvar body saved_env)
  ;;          (value-of body (extend-env bvar arg saved_env))))))
  ;; instrumented version
  (define apply-procedure
    (lambda (proc1 vals)
      (cases proc proc1
        (procedure (vars body saved_env)
      	    (let ((new_env (extend-env vars
                                       (map (lambda (val)
                                                (newref val))
                                            vals) 
                                       saved_env)))
              ; 在apply-procedure内部debug输出
      	      (when (instrument_let)  ; 改名叫instrument_apply_procedure
      		          (begin
      		            (eopl:printf
      		              "entering body of proc ~s with env =\n"
      		              vars)
      		            (pretty-print (env->list new_env))
                      (eopl:printf "store =\n")
                      (pretty-print (store->readable (get-store-as-list)))
                      (eopl:printf "\n")
                      ))
              (value-of body new_env))))))


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


