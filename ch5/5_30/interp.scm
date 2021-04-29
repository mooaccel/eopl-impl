(module interp (lib "eopl.ss" "eopl")
  
  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program 
           value-of/k
           instrument_value_of_k
           instrument_apply_cont
  )

  (define instrument_value_of_k (make-parameter #f))
  (define instrument_apply_cont (make-parameter #f))

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ; Program → FinalAnswer
  (define value-of-program
    (lambda (pgm) 
      (cases program pgm 
        (a-program (exp1) 
          (value-of/k exp1 (init-env) (end-cont)))))) ; end-cont constructor

  (define value-of/k
    (lambda (exp env cont)  ; cont是exp的continuation
      (begin
        (if (instrument_value_of_k)
            (begin
              (eopl:printf "Enter value-of/k... ~%exp = ~s ~%" exp)
              (eopl:printf "env = ~s ~%" env)
              (eopl:printf "cont = ~s ~%" cont))
            'ignore)
        (cases expression exp

          (const-exp (num)
            (apply-cont cont
                        (num-val num)))

          (var-exp (var) 
            (apply-cont cont
                        (apply-env env var)))

          (diff-exp (exp1 exp2)
            (value-of/k exp1
                        env
                        (diff1-cont exp2 env cont))) ; 多久触发这个diff1-cont? 在exp1得到结果后, apply-cont

          (zero?-exp (exp1)
            (value-of/k exp1
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
                        (proc-val (procedure var body))))

          (call-exp (rator rand)
            (value-of/k rator
                        env
                        (rator-cont rand env cont)))

          ))))

  (define (apply-cont cont val) 
    (begin
      (if (instrument_apply_cont)
          (begin
            (eopl:printf "==apply-cont, send ~s to ~s ]~%" val cont))
          'ignore)
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
                        (extend-env var val saved_env)
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
                        (rand-cont val saved_env saved_cont)))
          (rand-cont (rator saved_env saved_cont)
            (let ((proc1 (expval->proc rator)))
              (apply-procedure proc1 val saved_env saved_cont)))
      )))
  
  (define apply-procedure
    (lambda (proc1 val env cont)
      (cases proc proc1
        (procedure (var body)
          (value-of/k body 
                      (extend-env var val env)
                      cont)))))
  
)