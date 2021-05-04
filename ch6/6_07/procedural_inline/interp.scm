(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the LETREC language.  The \commentboxes are the
  ;; latex code for inserting the rules into the code in the book.
  ;; These are too complicated to put here, see the text, sorry.

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
                      (lambda (val) 
                          (begin 
                            (eopl:printf "End of computation.~%") 
                            val)))))))

  (define value-of/k
    (lambda (exp env cont)  ; cont是exp的continuation
      (cases expression exp

        (const-exp (num)
          (cont (num-val num)))

        (var-exp (var) 
          (cont (apply-env env var)))

        (diff-exp (exp1 exp2)
          (value-of/k exp1
                      env
                      (lambda (val1)
                        (value-of/k exp2
                                    env
                                    (lambda (val2) 
                                      (let ((num1 (expval->num val1))
                                            (num2 (expval->num val2)))
                                              (cont (num-val (- num1 num2)))))))))

        (zero?-exp (exp1)
          (value-of/k exp1
                      env 
                      (lambda (val)
                                (cont (bool-val (zero? (expval->num val)))))))
              
        (if-exp (exp1 exp2 exp3)
          (value-of/k exp1
                      env
                      (lambda (val)
                        (if (expval->bool val)
                            (value-of/k exp2 env cont)
                            (value-of/k exp3 env cont)))))

        (let-exp (var exp1 body)
          (value-of/k exp1  
                      env
                      (lambda (val)
                          (value-of/k body
                                      (extend-env var val env)
                                      cont))))
        
        (proc-exp (var body)
            (cont (proc-val (procedure var body env))))

        (call-exp (rator rand)
          (value-of/k rator
                      env
                      (lambda (rator_val)
                        (value-of/k rand
                                    env
                                    ;(rand-cont rator_val cont)))))
                                    (lambda (rand_val)
                                      (let ((proc1 (expval->proc rator_val)))
                                        (apply-procedure proc1 rand_val cont)))))))


        (letrec-exp (p-name b-var p-body letrec-body)
          (value-of/k letrec-body
                      (extend-env-rec p-name b-var p-body env)
                      cont))

        )))

  (define apply-procedure
    (lambda (proc1 arg cont)
      (cases proc proc1
        (procedure (var body saved_env)
          (value-of/k body 
                      (extend-env var arg saved_env)
                      cont)))))
  
)