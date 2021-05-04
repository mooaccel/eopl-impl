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
          (value-of/k exp1 
                      (init-env) 
                      (cons
                        (lambda (val)
                          (report-uncaught-exception))
                        (lambda (val)
                          (begin 
                            (eopl:printf "End of computation.~%") 
                            val))))))))

  (define value-of/k
    (lambda (exp env cont)
      (cases expression exp

        (const-exp (num)
          ((cdr cont) (num-val num)))

        (var-exp (var) 
          ((cdr cont) (apply-env env var)))

        (diff-exp (exp1 exp2)
          (value-of/k exp1
                      env
                      (cons
                        (lambda (val1)
                          ((car cont) val1))
                        (lambda (val1)
                          (value-of/k exp2
                                      env
                                      (cons
                                        (lambda (val2)
                                          ((car cont) val2))
                                        (lambda (val2) 
                                          (let ((num1 (expval->num val1))
                                                (num2 (expval->num val2)))
                                            ((cdr cont)
                                                        (num-val (- num1 num2)))))))))))

        (zero?-exp (exp1)
          (value-of/k exp1
                      env 
                      (cons
                        (lambda (val)
                          ((car cont) val cont))
                        (lambda (val)
                          ((cdr cont)
                                      (bool-val (zero? (expval->num val))))))))
              
        (if-exp (exp1 exp2 exp3)
          (value-of/k exp1
                      env
                      (cons
                        (lambda (val)
                          ((car cont) val))
                        (lambda (val)
                          (if (expval->bool val)
                              (value-of/k exp2 env cont)
                              (value-of/k exp3 env cont))))))

        (let-exp (var exp1 body)
          (value-of/k exp1  
                      env
                      (cons
                        (lambda (val)
                          ((car cont) val))
                        (lambda (val)
                          (value-of/k body
                                      (extend-env (list var)
                                                  (list val) 
                                                  env)
                                      cont)))))

        (proc-exp (var body)
          ((cdr cont)
                      (proc-val (procedure var body env))))

        (call-exp (rator rand)
          (value-of/k rator
                      env
                      (cons
                        (lambda (rator_val)
                          ((car cont) rator_val))
                        (lambda (rator_val)
                          (value-of/k rand
                                      env
                                      (cons
                                        (lambda (rand_val)
                                          ((car cont) rand_val))
                                        (lambda (rand_val)
                                          (let ((proc1 (expval->proc rator_val)))
                                            (apply-procedure/k proc1 rand_val cont)))))))))

        (letrec-exp (p-name b-var p-body letrec-body)
          (value-of/k letrec-body
                      (extend-env-rec p-name b-var p-body env)
                      cont))

        (cons-exp (exp1 exp2)
          (value-of/k exp1
                      env
                      (cons
                        (lambda (val1)
                          ((car cont) val1))
                        (lambda (val1)
                          (value-of/k exp2
                                      env
                                      (cons
                                        (lambda (val2)
                                          ((car cont) val2))
                                        (lambda (val2)
                                          ((cdr cont)
                                                      (pair-val val1
                                                                val2)))))))))
        
        (car-exp (exp1)
          (value-of/k exp1
                      env
                      (cons
                        (lambda (val)
                          ((car cont) val))
                        (lambda (val)
                          ((cdr cont) 
                                      (car (expval->pair val)))))))

        (cdr-exp (exp1)
          (value-of/k exp1
                      env
                      (cons
                        (lambda (val)
                          ((car cont) val))
                        (lambda (val)
                          ((cdr cont)
                                      (cdr (expval->pair val)))))))

        (emptylist-exp ()
          ((cdr cont)
                      (emptylist-val)))

        (null?-exp (exp1)
          (value-of/k exp1
                      env
                      (cons
                        (lambda (val)
                          ((car cont) val cont))
                        (lambda (val)
                          ((cdr cont)
                                      (expval-emptylist? val))))))

        (list-exp (exps)
          (if (null? exps)
              ((cdr cont) (emptylist-val))
              (let ((car_exps (car exps))
                    (remaining_exps (cdr exps)))
                (value-of/k car_exps
                            env
                            (cons
                              (lambda (val_car_exps)
                                ((car cont) val_car_exps))
                              (lambda (val_car_exps)
                                (value-of/k (list-exp remaining_exps)   ; 内部构造list-exp
                                            env
                                            (cons
                                              (lambda (val_remaining_exps)
                                                ((car cont) val_remaining_exps))
                                              (lambda (val_remaining_exps)
                                                ((cdr cont)
                                                            (pair-val val_car_exps
                                                                      val_remaining_exps)))))))))))
  
        (try-exp (exp1 var handler_exp)
          (value-of/k exp1
                      env
                      (cons
                        (lambda (val)
                          (value-of/k handler_exp
                                      (extend-env (list var) 
                                                  (list val) 
                                                  env)
                                      cont))
                        (lambda (val)
                          ((cdr cont) val)))))

        (raise-exp (exp1)
          (value-of/k exp1
                      env
                      (cons
                        (lambda (val)
                          ((car cont) val))
                        (lambda (val)
                          ((car cont) val)))))

        )))

  (define (report-uncaught-exception) 
    (eopl:error 'report-uncaught-exception "not find exception handler..."))

  (define apply-procedure/k
    (lambda (proc1 arg cont)
      (cases proc proc1
        (procedure (var body saved_env)
          (value-of/k body 
                      (extend-env (list var) 
                                  (list arg)
                                  saved_env)
                      cont)))))
  
)