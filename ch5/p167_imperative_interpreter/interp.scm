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

  ;;;;;;;;;;;;;;;; Imperative Interpreter ;;;;;;;;;;;;;;;;
  (define exp 'uninitialized) 
  (define env 'uninitialized) 
  (define cont 'uninitialized) 
  (define val 'uninitialized) 
  (define proc1 'uninitialized)

  (define value-of-program
    (lambda (pgm) 
      (cases program pgm 
        (a-program (exp1) 
          (set! exp exp1) 
          (set! env (init-env)) 
          (set! cont (end-cont)) 
          (value-of/k)))))

  (define value-of/k
    (lambda ()
      (cases expression exp
        (const-exp (num)
          (set! val (num-val num))
          (apply-cont))

        (var-exp (var) 
          (set! val (apply-env env var))
          (apply-cont))

        (diff-exp (exp1 exp2)
          (set! exp exp1)
          (set! cont (diff1-cont exp2 env cont))
          (value-of/k))

        (zero?-exp (exp1)
          (set! exp exp1)
          (set! cont (zero1-cont cont))
          (value-of/k))
              
        (if-exp (exp1 exp2 exp3)
          (set! exp exp1)
          (set! cont (if-test-cont exp2 exp3 env cont))
          (value-of/k))

        (let-exp (var exp1 body)
          (set! exp exp1)
          (set! cont (let-exp-cont var body env cont))
          (value-of/k))

        (proc-exp (var body)
          (set! val (proc-val (procedure var body env)))
          (apply-cont))

        (call-exp (rator rand)
          (set! exp rator)
          (set! cont (rator-cont rand env cont))
          (value-of/k))

        (letrec-exp (p-name b-var p-body letrec-body)
          (set! exp letrec-body)
          (set! env (extend-env-rec p-name b-var p-body env))
          (value-of/k))

        )))

  (define apply-cont
    (lambda ()
      (cases continuation cont
          (end-cont ()
            (begin 
              (eopl:printf "End of computation.~%") 
              val))
          (zero1-cont (saved_cont)
            (set! cont saved_cont)
            (set! val (bool-val (zero? (expval->num val))))
            (apply-cont))
          (let-exp-cont (var body saved_env saved_cont)
            (set! exp body)
            (set! env (extend-env var val saved_env))
            (set! cont saved_cont)
            (value-of/k))
          (if-test-cont (exp2 exp3 saved_env saved_cont)
            (if (expval->bool val)
                (begin 
                  (set! exp exp2)
                  (set! env saved_env)
                  (set! cont saved_cont)
                  (value-of/k))
                (begin
                  (set! exp exp3)
                  (set! env saved_env)
                  (set! cont saved_cont)
                  (value-of/k))))
          (diff1-cont (exp2 env saved_cont)
            (set! exp exp2)
            (set! cont (diff2-cont val saved_cont))
            (value-of/k))
          (diff2-cont (val1 saved_cont)
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val)))
              (set! cont saved_cont)
              (set! val (num-val (- num1 num2)))
              (apply-cont)))
          (rator-cont (rand env saved_cont)
            (set! exp rand)
            (set! cont (rand-cont val saved_cont))
            (value-of/k))
          (rand-cont (rator saved_cont)
            (set! proc1 (expval->proc rator))
            (set! cont saved_cont)
            (apply-procedure/k))
      )))

  (define apply-procedure/k
    (lambda ()
      (cases proc proc1
        (procedure (var body saved_env)
          (set! exp body)
          (set! env (extend-env var val saved_env))
          (value-of/k)))))
  
)