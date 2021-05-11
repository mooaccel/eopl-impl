(module interp (lib "eopl.ss" "eopl")
  
  (require "drscheme-init.scm")

  (require "cps-out-lang.scm")
  (require "data-structures.scm")       ; this includes environments

  (provide value-of-program value-of/k)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  (define (value-of-program cpsed_pgm)
    (cases cps-out-program cpsed_pgm
      (cps-a-program (tfexp)
        (value-of/k tfexp (init-env) (end-cont)))))

  (define value-of-simple-exp
    (lambda (sim_exp env)
      (cases simple-expression sim_exp
        (cps-const-exp (num)
          (num-val num))
        (cps-var-exp (ident)
          (apply-env env ident))
        (cps-diff-exp (sim_exp1 sim_exp2)
          (let ((val1 (expval->num (value-of-simple-exp sim_exp1 env)))
                (val2 (expval->num (value-of-simple-exp sim_exp2 env))))
            (num-val (- val1 val2))))
        (cps-zero?-exp (sim_exp1)
          (let ((val1 (value-of-simple-exp sim_exp1 env)))
            (if (zero? (expval->num val1))
                (bool-val #t)
                (bool-val #f))))
        (cps-sum-exp (sim_exps)
          (let ((nums (map (lambda (exp_item) 
                              (expval->num (value-of-simple-exp exp_item env)))
                           sim_exps)))
            (num-val (accumulate + 0 nums))))
        (cps-proc-exp (vars tfexp_body)
          (proc-val (procedure vars tfexp_body env)))
      )))

  ; 非常基础的函数
  (define (accumulate op initial sequence) 
    (if (null? sequence) 
        initial 
        (op (car sequence) 
            (accumulate op initial (cdr sequence)))))
  
  ;; value-of/k : TfExp * Env * Cont -> FinalAnswer
  (define value-of/k
    (lambda (exp env cont) 
      (cases tfexp exp
        (simple-exp->exp (sim_exp)
          (apply-cont cont 
                      (value-of-simple-exp sim_exp env)))
        (cps-let-exp (ident sim_exp1 tf_exp1)
          (let ((val1 (value-of-simple-exp sim_exp1 env)))
            (value-of/k tf_exp1
                        (extend-env* (list ident)
                                     (list val1)
                                     env)
                        cont)))
        (cps-letrec-exp (proc_names bound_varss proc_bodys letrec_body)
          (value-of/k letrec_body
                      (extend-env-rec** proc_names bound_varss proc_bodys env)
                      cont))
        (cps-if-exp (sim_exp tfexp1 tfexp2)
          (let ((val_sim_exp (value-of-simple-exp sim_exp env)))
            (if (expval->bool val_sim_exp)
                (value-of/k tfexp1 env cont)
                (value-of/k tfexp2 env cont))))
        (cps-call-exp (rator rands)
          (let ((rator_val (value-of-simple-exp rator env))
                (rands_val (map (lambda (rand)
                                  (value-of-simple-exp rand env))
                                rands)))
              (apply-procedure/k (expval->proc rator_val)
                                 rands_val
                                 cont)))
      )))

  ;; apply-cont : Cont * ExpVal -> Final-ExpVal
  ;; there's only one continuation, and it only gets invoked once, at
  ;; the end of the computation.
  (define apply-cont
    (lambda (cont val)
      (cases continuation cont
        (end-cont () 
          val))))
      
  ;; apply-procedure/k : Proc * ExpVal * Cont -> ExpVal
  ;; Page: 209 
  (define apply-procedure/k
    (lambda (proc1 args cont)
      (cases proc proc1
        (procedure (vars body saved-env)
          (value-of/k body
            (extend-env* vars args saved-env)
            cont)))))

  '(define apply-procedure/k
    (lambda (proc1 args cont)
      (cases proc proc1
        (procedure (vars body saved-env)
          (value-of/k body
            (extend-env* vars args saved-env)
            cont)))))

  ;; trace has to be in the module where the procedure is defined.
  ;; (trace value-of/k apply-cont)

  )
