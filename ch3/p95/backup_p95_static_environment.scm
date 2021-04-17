; 书本上代码的备份

(define-datatype proc proc?
  (procedure 
    (body expression?) 
    (saved-nameless-env nameless-environment?)))
(define nameless-environment?
  (lambda (x) 
    ((list-of expval?) x)))
(define empty-nameless-env
  (lambda () 
    '()))
(define extend-nameless-env        ; 和后面的extend-senv有什么区别?
  (lambda (val nameless-env) 
    (cons val nameless-env)))
(define apply-nameless-env
  (lambda (nameless-env n) 
    (list-ref nameless-env n)))

(define apply-procedure
  (lambda (proc1 val) 
    (cases proc proc1 
      (procedure (body saved-nameless-env) 
        (value-of body (extend-nameless-env val saved-nameless-env))))))

(define empty-senv
  (lambda () 
    '()))
(define extend-senv
  (lambda (var senv) 
    (cons var senv)))
(define apply-senv
  (lambda (senv var) 
    (cond ((null? senv) 
            (report-unbound-var var))
          ((eqv? var 
                 (car senv)) 
            0)
          (else 
            (+ 1 (apply-senv (cdr senv) 
                             var))))))
                             
; Program → Nameless-program
(define translation-of-program
  (lambda (pgm) 
    (cases program pgm 
      (a-program (exp1) 
        (a-program (translation-of exp1 (init-senv)))))))

(define translation-of
  (lambda (exp senv) 
    (cases expression exp 
      (const-exp (num) 
        (const-exp num)) 
      (diff-exp (exp1 exp2) 
        (diff-exp (translation-of exp1 senv) 
                  (translation-of exp2 senv))) 
      (zero?-exp (exp1) 
        (zero?-exp (translation-of exp1 senv))) 
      (if-exp (exp1 exp2 exp3) 
        (if-exp (translation-of exp1 senv) 
                (translation-of exp2 senv) 
                (translation-of exp3 senv))) 
      (var-exp (var) 
        (nameless-var-exp (apply-senv senv var))) 
      ; ?
      (let-exp (var exp1 body) 
        (nameless-let-exp (translation-of exp1 senv) 
                          (translation-of body (extend-senv var senv)))) 
      (proc-exp (var body) 
        (nameless-proc-exp (translation-of body (extend-senv var senv)))) 
      (call-exp (rator rand) 
        (call-exp (translation-of rator senv) 
                  (translation-of rand senv))) 
      (else (report-invalid-source-expression exp)))))

(define run
  (lambda (string) 
    (value-of-program 
      (translation-of-program 
        (scan&parse string)))))



; ? i v x
(define init-senv
  (lambda () 
    (extend-senv 'i 
                 (extend-senv 'v 
                              (extend-senv 'x 
                                           (empty-senv))))))

