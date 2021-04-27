(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the LETREC language.  The \commentboxes are the
  ;; latex code for inserting the rules into the code in the book.
  ;; These are too complicated to put here, see the text, sorry.

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
    ; exp整个代表一个值, 它最终返回一个值...
    (lambda (exp env cont)  ; cont是exp的continuation
      ; (eopl:pretty-print "Enter value-of/k...")
      ; (eopl:pretty-print "exp = ")
      ; (eopl:pretty-print exp)
      ; (eopl:pretty-print "env = ")
      ; (eopl:pretty-print env)
      ; (eopl:pretty-print "cont = ")
      ; (eopl:pretty-print cont)
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

        ;(let-exp (var exp1 body)
        ;  (value-of/k exp1  
        ;              env
        ;              (let-exp-cont var body env cont)))

        (let2-exp (var1 exp1 var2 exp2 body)
          (value-of/k exp1  
                      env
                      (let2-exp-cont-1 var1 var2 exp2 body env cont)))

        (let-exp (vars exps body)
          (value-of/k (list-exp exps)
                      env                 
                      (let-exp-cont vars body env cont)))
          ;(if (null? vars)
          ;    (apply-cont )
          ;    (let ((car_vars (car vars))
          ;          (car_exps (car exps))
          ;          (remaining_vars (cdr vars))
          ;          (remaining_exps (cdr exps)))
          ;      (value-of/k car_exps
          ;                  env
          ;                  (let-exp-cont-1 car_vars remaining_vars remaining_exps body env cont)))))
        
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

        ;(list-exp (exps)
        ;  (list-val (map (lambda (exp_item) 
        ;                   (value-of exp_item env))
        ;                 exps)))

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
        ;(let-exp-cont (var body saved_env saved_cont)
        ;  (value-of/k body
        ;              (extend-env (list var)
        ;                          (list val) 
        ;                          saved_env)
        ;              saved_cont))
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
        (rator-cont (rand env saved_cont)
          (value-of/k rand
                      env
                      (rand-cont val saved_cont)))
        (rand-cont (rator saved_cont)
          (let ((proc1 (expval->proc rator)))
            (apply-procedure proc1 val saved_cont)))
        (let2-exp-cont-1 (var1 var2 exp2 body env cont)
          (value-of/k exp2
                      env
                      (let2-exp-cont-2 var1 val var2 body env cont)))
        (let2-exp-cont-2 (var1 val1 var2 body env cont)
          (value-of/k body
                      (extend-env (list var1 var2)
                                  (list val1 val)
                                  env)
                      cont))

        (cons-exp-cont-1 (exp2 env saved_cont)
          (value-of/k exp2
                      env
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

        (list-exp-cont-car (remaining_exps env saved_cont)
          (value-of/k (list-exp remaining_exps)   ; 内部构造list-exp
                      env
                      (list-exp-cont-remaining val saved_cont)))

        (list-exp-cont-remaining (car_val saved_cont)
          ;(eopl:pretty-print "In list-exp-cont-remaining")
          ;(eopl:printf "car_val = ~s." car_val)
          ;(eopl:pretty-print "saved_cont = ")
          ;(eopl:pretty-print saved_cont)
          (apply-cont saved_cont
                      (pair-val car_val
                                val)))

       ; ; value-of/k let-exp应该返回什么值?
       ; (let-exp-cont-1 (car_vars remaining_vars remaining_exps body env saved_cont)
       ;   (value-of/k (let-exp remaining_vars remaining_exps body)
       ;               env
       ;               (let-exp-cont-2 car_vars val  ; 去let-exp-cont-2里面, 组合结果, 然后extend-env, 然后value-of body
       ;                               body
       ;                               env
       ;                               saved_cont)))

       ; (let-exp-cont-2 (car_vars car_vals saved_cont)
       ;   (value-of/k body 
       ;               (extend-env **)
       ;               saved_cont))

        (let-exp-cont (vars body env cont)
          (value-of/k body
                      (extend-env vars 
                                  (transform-aux val) 
                                  env)
                      cont))
    ))

  ; list_val => (list *)
  (define (transform-aux val)
    (cases expval val
      (pair-val (car_val cdr_val)
        (cons (transform-aux car_val)
              (transform-aux cdr_val)))
      (emptylist-val ()
        '())
	    (else 
        val)))

  ;(define val1 (pair-val (num-val 100)
  ;                       (pair-val (num-val 200) (emptylist-val))))
  ;(eopl:pretty-print val1)
  ;(eopl:pretty-print (transform-aux val1))
  ;(define val2 (pair-val (pair-val (num-val 100) (emptylist-val))
  ;                       (pair-val (num-val 200) (emptylist-val))))
  ;(eopl:pretty-print val2)
  ;(eopl:pretty-print (transform-aux val2))

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