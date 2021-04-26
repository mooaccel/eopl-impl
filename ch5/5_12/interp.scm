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
           value-of/k
           instrument_value_of_k
           instrument_apply_cont)

  (define instrument_value_of_k (make-parameter #f))
  (define instrument_apply_cont (make-parameter #f))

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

  (define exp->readable
    (lambda (exp)
      (cases expression exp
        (const-exp (num)
          "const-exp")
        (var-exp (var)
          "var-exp")
        (diff-exp (exp1 exp2)
          "diff-exp")
        (zero?-exp (exp1)
          "zero?-exp")
        (if-exp (exp1 exp2 exp3)
          "if-exp")
        (let-exp (var exp1 body)
          "let-exp")
        (proc-exp (var body)
          "proc-exp")
        (call-exp (rator rand)
          "call-exp")
        (letrec-exp (p-name b-var p-body letrec-body)
          "letrec-exp")
      )))

  ;(define cont->readable
  ;  (lambda (cont)
  ;    (cases continuation cont 
  ;        (end-cont () 
  ;          "end-cont")
  ;        (zero1-cont (saved_cont) 
  ;          "zero1-cont")
  ;        (let-exp-cont (var body saved_env saved_cont)
  ;          "let-exp-cont")
  ;        (if-test-cont (exp2 exp3 saved_env saved_cont)
  ;          "if-test-cont")
  ;        (diff1-cont (exp2 env saved_cont)
  ;          "diff1-cont")
  ;        (diff2-cont (val1 saved_cont)
  ;          "diff2-cont")
  ;        (rator-cont (rand env saved_cont)
  ;          "rator-cont") 
  ;        (rand-cont (rator saved_cont)
  ;          "rand-cont")
  ;    )))


  (define value-of/k
    (lambda (exp env cont)  ; cont是exp的continuation
      (begin
        (if (instrument_value_of_k)
            (begin
              (eopl:printf "Enter value-of/k... ~%exp = ~s ~%" exp)
              ;(eopl:printf "cont = ~s ~%" (cont->readable cont)))
              (eopl:printf "cont = ~s ~%" cont))
            'ignore)
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

  ))))

  (define (apply-cont cont val) 
    (begin
      (if (instrument_apply_cont)
          (begin
            ;(eopl:printf "Enter apply-cont ~s ~s ]~%" (expval->readable val) (cont->readable cont)))
            (eopl:printf "==apply-cont, send ~s to ~s ]~%" (expval->readable val) cont))
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
          ;(call-exp-rand-cont (rator env saved_cont)
          ;  (value-of/k rator 
          ;              env
          ;              (call-exp-rator-cont val saved_cont)))
          ;(call-exp-rator-cont (rand saved_cont)
          ;  (let ((proc1 (expval->proc val)))
          ;    (apply-procedure proc1 rand saved_cont)))
    )))

  (define apply-procedure
    (lambda (proc1 arg cont)
      (cases proc proc1
        (procedure (var body saved_env)
          (value-of/k body 
                      (extend-env var arg saved_env)
                      cont)))))
  
)