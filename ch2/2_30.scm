#lang eopl

(define (identifier? var)
  (and (symbol? var)
       (not (eqv? var 'lambda))))

(define-datatype lc-exp lc-exp?
    (var-exp 
      (var identifier?))
    (lambda-exp 
      (bound-vars (list-of identifier?))
      (body lc-exp?))
    (app-exp 
      (rator lc-exp?) 
      (rands (list-of lc-exp?))))

(define (list-of pred)
  (lambda (val)
   (or (null? val)
       (and (pair? val)
            (pred (car val))
            ((list-of pred) (cdr val))))))

(define parse-expression
 (lambda (datum)
  (cond
   ((symbol? datum)
      (if (eqv? datum 'lambda)
          (eopl:error 'parse "lambda is not a valid symbol")
          (var-exp datum)))
   ((pair? datum)
      (if (eqv? (car datum) 'lambda)
          (if (not (= (length datum) 3))
              (eopl:error 'parse "lambda need both args and body")
              (if (not (list? (cadr datum))) 
                  (eopl:error 'parse "lambda args must be list")
                  (lambda-exp (cadr datum) (parse-expression (caddr datum)))))
          (app-exp (parse-expression (car datum)) 
                   (map (lambda (item) 
                          (parse-expression item))
                        (cdr datum)))))
   (else
    (report-invalid-concrete-syntax datum)))))

(define (report-invalid-concrete-syntax datum)
  (eopl:error 'report-invalid-concrete-syntax "invalid concrete syntax ~s" datum))

; ===================== 正确的语法
; (eopl:pretty-print (parse-expression '(lambda (x) (x))))  ; 可以没有rand
; (eopl:pretty-print (parse-expression '(lambda (x) x))) ; body可以是var-exp
; ===================== 错误的语法
; (eopl:pretty-print (parse-expression '(lambda (x) ())))  ; ()会被pair?判定为#f

; (eopl:pretty-print (parse-expression 'lambda))
; (eopl:pretty-print (parse-expression '(lambda (x))))
; (eopl:pretty-print (parse-expression '(lambda x (x y))))