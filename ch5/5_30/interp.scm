(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the LETREC language.  The \commentboxes are the
  ;; latex code for inserting the rules into the code in the book.
  ;; These are too complicated to put here, see the text, sorry.

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program 
           value-of)
           ;value-of/k)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ; Program → FinalAnswer
  (define value-of-program
    (lambda (pgm) 
      (cases program pgm 
        (a-program (exp1) 
          (value-of exp1 (init-env))))))
          ;(value-of/k exp1 (init-env) (end-cont)))))) ; end-cont constructor

  (define (value-of exp env)
    (cases expression exp
      (const-exp (num)
        (num-val num))
      (var-exp (var) 
        (apply-env env var))
      (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
      (diff-exp (exp1 exp2)
        (let ((val1 (value-of exp1 env)) 
              (val2 (value-of exp2 env))) 
          (let ((num1 (expval->num val1)) 
                (num2 (expval->num val2))) 
            (num-val (- num1 num2)))))
      (if-exp (exp1 exp2 exp3)
        (let ((val1 (value-of exp1 env)))
          (if (expval->bool val1)
            (value-of exp2 env)
            (value-of exp3 env))))
      (let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
                      (extend-env var val1 env))))
      (proc-exp (var body) 
          (proc-val (procedure var 
                               body)))
      (call-exp (rator rand)
        (let ((proc (expval->proc (value-of rator env)))
              (arg (value-of rand env)))
          (apply-procedure proc arg env)))  ; 注意, dynamic binding需要传env
  ))

  ;(define value-of/k
  ;  (lambda (exp env cont)  ; cont是exp的continuation
  ;    (cases expression exp

  ;      (const-exp (num)
  ;        (apply-cont cont
  ;                    (num-val num)))

  ;      (var-exp (var) 
  ;        (apply-cont cont
  ;                    (apply-env env var)))

  ;      (diff-exp (exp1 exp2)  ; cont是diff-exp的continuation
  ;        ; value-of exp1, exp2之后再去计算某些东西, 然后再进入原先的cont
  ;        (value-of/k exp1
  ;                    env
  ;                    (diff1-cont exp2 env cont))) ; 多久触发这个diff1-cont? 在exp1得到结果后, apply-cont

  ;      ; cont是zero?-exp的continuation
  ;      (zero?-exp (exp1)
  ;        (value-of/k exp1        ; value-of/k exp1内部会触发apply-cont
  ;                    env 
  ;                    (zero1-cont cont)))
  ;            
  ;      (if-exp (exp1 exp2 exp3)
  ;        (value-of/k exp1
  ;                    env
  ;                    (if-test-cont exp2 exp3 env cont)))

  ;      (let-exp (var exp1 body)
  ;        (value-of/k exp1  
  ;                    env
  ;                    (let-exp-cont var body env cont)))
  ;      
  ;      (proc-exp (var body)
  ;        (apply-cont cont
  ;                    (proc-val (procedure var body env))))

  ;      (call-exp (rator rand)
  ;        (value-of/k rator
  ;                    env
  ;                    (rator-cont rand env cont)))

  ;      )))

  ;(define (apply-cont cont val) 
  ;  (cases continuation cont 
  ;    (end-cont () 
  ;      (begin 
  ;        (eopl:printf "End of computation.~%") 
  ;        val)) 
  ;      (zero1-cont (saved_cont) 
  ;        (apply-cont saved_cont 
  ;          (bool-val (zero? (expval->num val)))))
  ;      (let-exp-cont (var body saved_env saved_cont)
  ;        (value-of/k body
  ;                    (extend-env var val saved_env)
  ;                    saved_cont))
  ;      (if-test-cont (exp2 exp3 saved_env saved_cont)
  ;        (if (expval->bool val)
  ;            (value-of/k exp2 saved_env saved_cont)
  ;            (value-of/k exp3 saved_env saved_cont)))
  ;      (diff1-cont (exp2 env saved_cont)
  ;        (value-of/k exp2
  ;                    env
  ;                    (diff2-cont val ; exp1的结果存起来
  ;                                saved_cont)))         
  ;      (diff2-cont (val1 saved_cont)
  ;        (let ((num1 (expval->num val1))
  ;              (num2 (expval->num val)))
  ;          (apply-cont saved_cont  ; 得到diff-exp的结果后, 返回原先diff-exp的continuation(即saved_cont)
  ;                      (num-val (- num1 num2)))))
  ;      (rator-cont (rand env saved_cont)
  ;        (value-of/k rand
  ;                    env
  ;                    (rand-cont val saved_cont)))
  ;      (rand-cont (rator saved_cont)
  ;        (let ((proc1 (expval->proc rator)))
  ;          (apply-procedure proc1 val saved_cont)))
  ;  ))
  
  (define apply-procedure
    (lambda (proc1 val env)
      (cases proc proc1
        (procedure (var body)
          (value-of body (extend-env var val env))))))

  ;(define apply-procedure
  ;  (lambda (proc1 val cont)
  ;    (cases proc proc1
  ;      (procedure (var body saved_env)
  ;        (value-of/k body 
  ;                    (extend-env var val saved_env)
  ;                    cont)))))
  
)