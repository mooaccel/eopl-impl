; Write an unparser that converts the abstract syntax of an lc-exp into a string 
; that matches the second grammar in this section (page 52).

#lang eopl

(define (identifier? var)
  (and (symbol? var)
       (not (eqv? var 'lambda))))

(define-datatype lc-exp lc-exp?
    (var-exp 
      (var identifier?))
    (lambda-exp 
      (bound-var identifier?) 
      (body lc-exp?))
    (app-exp 
      (rator lc-exp?) 
      (rand lc-exp?)))

(define (unparse exp)
   (cases lc-exp exp
	    (var-exp (var)
		    (symbol->string var))
	    (lambda-exp (bound-var body)
		    (format "(lambda (~a) ~a)" bound-var (unparse body)))
	    (app-exp (rator rand)
		    (format "(~a ~a)" (unparse rator) (unparse rand)))))

(define expA (var-exp 'a))
(define expB (var-exp 'b))
(define app (app-exp expA expB))
(define lexp (lambda-exp 'a app))
(display (equal? (unparse app) "(a b)"))
(display (equal? (unparse lexp) "(lambda (a) (a b))"))

; todo  不支持format怎么办?