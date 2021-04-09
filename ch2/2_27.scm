#lang eopl

(define (identifier? var)
  (and (symbol? var)
       (not (eqv? var 'lambda))))

(define-datatype lc-exp lc-exp?
    (var-exp (var identifier?))
    (lambda-exp (bound-var identifier?) (body lc-exp?))
    (app-exp (rator lc-exp?) (rand lc-exp?)))

; 输入的datum是一个list
(define parse-expression
 (lambda (datum)
  (cond
   ((symbol? datum)
    (var-exp datum))
   ((pair? datum)
    (if (eqv? (car datum) 'lambda)
     (lambda-exp (car (cadr datum)) (parse-expression (caddr datum)))
     (app-exp (parse-expression (car datum)) (parse-expression (cadr datum)))))
   (else
    (report-invalid-concrete-syntax datum)))))

(define (report-invalid-concrete-syntax datum)
  (eopl:error 'report-invalid-concrete-syntax "invalid concrete syntax ~s" datum))

(define unparse-lc-exp
 (lambda (exp)
  (cases lc-exp exp
   (var-exp (var) 
    var)
   (lambda-exp (bound-var body)
    (list 'lambda (list bound-var) (unparse-lc-exp body)))
   (app-exp (rator rand) 
    (list (unparse-lc-exp rator) (unparse-lc-exp rand))))))
; 上面的代码copy from ./ch2/p53_lambda_ast_example.scm

(eopl:pretty-print (parse-expression '((lambda (a) (a b)) c)
  ))

(eopl:pretty-print (parse-expression '(lambda (x)
                                        (lambda (y) 
                                          ((lambda (x) (x y)) x)))
  ))
