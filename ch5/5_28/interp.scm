(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the LETREC language.  The \commentboxes are the
  ;; latex code for inserting the rules into the code in the book.
  ;; These are too complicated to put here, see the text, sorry.

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program 
           value-of/k
           instrument_value_of_k
           instrument_apply_cont)

  (define instrument_value_of_k (make-parameter #f))
  (define instrument_apply_cont (make-parameter #f))

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
          (set! cont (end-cont)) 
          (set! exp exp1) 
          (set! env (init-env)) 
          (value-of/k)))))

  (define value-of/k
    (lambda ()
      (begin
        (if (instrument_value_of_k)
            (begin
              (eopl:printf "Enter value-of/k... ~%exp = ~s ~%" exp)
              (eopl:printf "cont = ~s ~%" cont))
            'ignore)
        (cases expression exp
          (const-exp (num)
            (set! val (num-val num))
            (apply-cont))

          (var-exp (var) 
            (set! val (apply-env env var))
            (apply-cont))

          (diff-exp (exp1 exp2)
            (set! cont (diff1-cont exp2 env cont))
            (set! exp exp1)
            (value-of/k))

          (zero?-exp (exp1)
            (set! cont (zero1-cont cont))
            (set! exp exp1)
            (value-of/k))

          (if-exp (exp1 exp2 exp3)
            (set! cont (if-test-cont exp2 exp3 env cont))
            (set! exp exp1)
            (value-of/k))

          (let-exp (var exp1 body)
            (set! cont (let-exp-cont var body env cont))
            (set! exp exp1)
            (value-of/k))

          (proc-exp (var body)
            (set! val (proc-val (procedure var body env)))
            (apply-cont))

          (call-exp (rator rand)
            (set! cont (rator-cont rand env cont))
            (set! exp rator)
            (value-of/k))

          (letrec-exp (p-name b-var p-body letrec-body)
            (set! exp letrec-body)
            (set! env (extend-env-rec p-name b-var p-body env))
            (value-of/k))

        ))))

  (define apply-cont
    (lambda ()
      (begin
        (if (instrument_apply_cont)
            (begin
              ;(eopl:printf "Enter apply-cont ~s ~s ]~%" (expval->readable val) (cont->readable cont)))
              (eopl:printf "==apply-cont, send ~s to ~s ]~%" val cont))
            'ignore)
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
              (set! cont saved_cont)
              (set! exp body)
              (set! env (extend-env var val saved_env))
              (value-of/k))
            (if-test-cont (exp2 exp3 saved_env saved_cont)
              (set! cont saved_cont)
              (if (expval->bool val)
                  (set! exp exp2)
                  (set! exp exp3))
              (set! env saved_env)
              (value-of/k))
            (diff1-cont (exp2 saved_env saved_cont)
              (set! cont (diff2-cont val saved_cont))
              (set! exp exp2)
              (set! env saved_env)
              (value-of/k))
            (diff2-cont (val1 saved_cont)
              (let ((num1 (expval->num val1))
                    (num2 (expval->num val)))
                (set! cont saved_cont)
                (set! val (num-val (- num1 num2)))
                (apply-cont)))
            (rator-cont (rand saved_env saved_cont)
              (set! cont (rand-cont val saved_cont))
              (set! exp rand)
              (set! env saved_env)
              (value-of/k))
            (rand-cont (rator saved_cont)
              (set! cont saved_cont)
              (set! proc1 (expval->proc rator))
              (apply-procedure/k))
      ))))

  (define apply-procedure/k
    (lambda ()
      (cases proc proc1
        (procedure (var body saved_env)
          (set! exp body)
          (set! env (extend-env var val saved_env))
          (value-of/k)))))
  
)