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

        (try-exp (exp1 var handler_exp)  ; cont是try-exp的continuation
          (eopl:pretty-print "In try-exp")
          (eopl:pretty-print cont)
          (value-of/k exp1
                      env
                      (try-cont var handler_exp env cont)))

        (raise-exp (exp1)
          (eopl:pretty-print "In raise-exp")
          (eopl:pretty-print cont)
          (value-of/k exp1  ; exception
                      env
                      (raise-cont cont)))

        )))

  ; todo, 暂时不需要
  ;(define (continuation?)
  ;  #t)
  (define (end-cont)  
    (define (apply-cont-aux val)
      (eopl:printf "End of computation.~%") 
      val)
    (define (apply-handler-aux val)
      (eopl:error 'report-uncaught-exception "not find exception handler..."))
    (define (dispatch mt)
      (cond ((eq? mt 'apply-cont-aux)
              apply-cont-aux)
            ((eq? mt 'apply-handler-aux)
              apply-handler-aux)
            (else 
              (eopl:error "Unknown mt: " mt))))
    dispatch)
  (define (zero1-cont saved_cont)
    (define (apply-cont-aux val)
      (apply-cont saved_cont
                  (bool-val (zero? (expval->num val)))))
    (define (apply-handler-aux val)
      (apply-handler saved_cont
                     val))
    (define (dispatch mt)
      (cond ((eq? mt 'apply-cont-aux)
              apply-cont-aux)
            ((eq? mt 'apply-handler-aux)
              apply-handler-aux)
            (else 
              (eopl:error "Unknown mt: " mt))))
    dispatch)
  (define (let-exp-cont var body saved_env saved_cont)
    (define (apply-cont-aux val)
      (value-of/k body
                  (extend-env (list var)
                              (list val)
                              saved_env)
                  saved_cont))
    (define (apply-handler-aux val)
      (apply-handler saved_cont
                     val))
    (define (dispatch mt)
      (cond ((eq? mt 'apply-cont-aux)
              apply-cont-aux)
            ((eq? mt 'apply-handler-aux)
              apply-handler-aux)
            (else 
              (eopl:error "Unknown mt: " mt))))
    dispatch)
  (define (if-test-cont exp2 exp3 saved_env saved_cont)
    (define (apply-cont-aux val)
      (if (expval->bool val)
          (value-of/k exp2 saved_env saved_cont)
          (value-of/k exp3 saved_env saved_cont)))
    (define (apply-handler-aux val)
      (apply-handler saved_cont
                     val))
    (define (dispatch mt)
      (cond ((eq? mt 'apply-cont-aux)
              apply-cont-aux)
            ((eq? mt 'apply-handler-aux)
              apply-handler-aux)
            (else 
              (eopl:error "Unknown mt: " mt))))
    dispatch)
  (define (diff1-cont exp2 saved_env saved_cont)
    (define (apply-cont-aux val)
      (value-of/k exp2
                  saved_env
                  (diff2-cont val saved_cont)))
    (define (apply-handler-aux val)
      (apply-handler saved_cont
                     val))
    (define (dispatch mt)
      (cond ((eq? mt 'apply-cont-aux)
              apply-cont-aux)
            ((eq? mt 'apply-handler-aux)
              apply-handler-aux)
            (else 
              (eopl:error "Unknown mt: " mt))))
    dispatch)
  (define (diff2-cont saved_val1 saved_cont)
    (define (apply-cont-aux val)
      (let ((num1 (expval->num saved_val1))
            (num2 (expval->num val)))
        (apply-cont saved_cont
                    (num-val (- num1 num2)))))
    (define (apply-handler-aux val)
      (apply-handler saved_cont
                     val))
    (define (dispatch mt)
      (cond ((eq? mt 'apply-cont-aux)
              apply-cont-aux)
            ((eq? mt 'apply-handler-aux)
              apply-handler-aux)
            (else 
              (eopl:error "Unknown mt: " mt))))
    dispatch)
  (define (rator-cont rand saved_env saved_cont)
    (define (apply-cont-aux val)
      (value-of/k rand
                  saved_env
                  (rand-cont val saved_cont)))
    (define (apply-handler-aux val)
      (apply-handler saved_cont
                     val))
    (define (dispatch mt)
      (cond ((eq? mt 'apply-cont-aux)
              apply-cont-aux)
            ((eq? mt 'apply-handler-aux)
              apply-handler-aux)
            (else 
              (eopl:error "Unknown mt: " mt))))
    dispatch)
  (define (rand-cont rator_val saved_cont)
    (define (apply-cont-aux val)
      (let ((proc1 (expval->proc rator_val)))
        (apply-procedure proc1 val saved_cont)))
    (define (apply-handler-aux val)
      (apply-handler saved_cont
                     val))
    (define (dispatch mt)
      (cond ((eq? mt 'apply-cont-aux)
              apply-cont-aux)
            ((eq? mt 'apply-handler-aux)
              apply-handler-aux)
            (else 
              (eopl:error "Unknown mt: " mt))))
    dispatch)
  (define (try-cont var handler_exp saved_env saved_cont)
    (define (apply-cont-aux val)
      (apply-cont saved_cont val))
    (define (apply-handler-aux val)
      (value-of/k handler_exp   ; exception handler
                  (extend-env (list var)
                              (list val)
                              saved_env)
                  saved_cont))
    (define (dispatch mt)
      (cond ((eq? mt 'apply-cont-aux)
              apply-cont-aux)
            ((eq? mt 'apply-handler-aux)
              apply-handler-aux)
            (else 
              (eopl:error "Unknown mt: " mt))))
    dispatch)
  (define (raise-cont saved_cont)
    (define (apply-cont-aux val)
      (apply-handler saved_cont 
                     val))
    (define (apply-handler-aux val)
      (apply-handler saved_cont
                     val))
    (define (dispatch mt)
      (cond ((eq? mt 'apply-cont-aux)
              apply-cont-aux)
            ((eq? mt 'apply-handler-aux)
              apply-handler-aux)
            (else 
              (eopl:error "Unknown mt: " mt))))
    dispatch)


  (define (apply-cont cont val)
    ((cont 'apply-cont-aux) val))

  (define (apply-handler cont val)
    ((cont 'apply-handler-aux) val))
  
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