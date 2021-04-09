; 2_29这道题开始支持多个参数, bound-vars
; app-exp的操作数可以是多个, rands
; 非常简单就变换过来了.. 从前面的例子, 很好...
#lang eopl

(define (identifier? var)
  (and (symbol? var)
       (not (eqv? var 'lambda))))

; todo { Identifier }^* Kleene star通过list-of高阶函数做, 那Kleene plus呢? 存在疑问
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
            ((list-of pred) (cdr val)))))
)

; 输入的datum是一个list
(define parse-expression
 (lambda (datum)
; for debug
;  (begin 
;    (display datum)
;    (newline)
  (cond
   ((symbol? datum)
    (var-exp datum))
   ((pair? datum)
    (if (eqv? (car datum) 'lambda)
        (lambda-exp (cadr datum) (parse-expression (caddr datum)))
        (app-exp (parse-expression (car datum)) (map (lambda (item) 
                                                        (parse-expression item))
                                                     (cdr datum)))))
   (else
    (report-invalid-concrete-syntax datum)))))
;(lambda (x y) (f x y z))
;(f x y z)

(define (report-invalid-concrete-syntax datum)
  (eopl:error 'report-invalid-concrete-syntax "invalid concrete syntax ~s" datum))

(define unparse-lc-exp
 (lambda (exp)
  (cases lc-exp exp
   (var-exp (var) 
    var)
   (lambda-exp (bound-vars body)
    (list 'lambda bound-vars (unparse-lc-exp body)))  ; bound-vars绑定到一个list上了
   (app-exp (rator rands) 
    (cons (unparse-lc-exp rator) (map (lambda (item) 
                                          (unparse-lc-exp item))
                                      rands))))))

(eopl:pretty-print (parse-expression '(lambda (x y z) (f x y z))
  ))
(eopl:pretty-print (parse-expression '(f x y z))
  )

(eopl:pretty-print (unparse-lc-exp (parse-expression '(lambda (x y z) (f x y z)))))
(eopl:pretty-print (unparse-lc-exp (parse-expression '(lambda (x) (f x y z)))))
; (x) 一个元素也是个list

; lambda
; (eopl:pretty-print
; (cadr '(lambda (x y z) (f x y z)))
; )
; (eopl:pretty-print
; (caddr '(lambda (x y z) (f x y z)))
; )