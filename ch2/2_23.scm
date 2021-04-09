; Exercise 2.23 The deﬁnition of lc-exp ignores the condition in definition 1.1.8 
; that says “Identifier is any symbol other than lambda.” Modify the definition of
; identifier? to capture this condition. As a hint, remember that any predicate can be used in define-datatype, even ones you deﬁne.

; 2_23大部分代码 copy from ./ch2/p46_define_datatype.scm

#lang eopl
(define (identifier? var)
  (and (symbol? var)
       (not (eqv? var 'lambda))))

; LcExp ::= Identifier
;       ::= (lambda (Identifier) LcExp) 
;       ::= (LcExp LcExp)
(define-datatype lc-exp lc-exp?
    (var-exp (var identifier?))
    (lambda-exp (bound-var identifier?) (body lc-exp?))
    (app-exp (rator lc-exp?) (rand lc-exp?)))

(define occurs-free?
 (lambda (search-var exp)
  (cases lc-exp
         exp
         (var-exp (var) (eqv? var search-var))
         (lambda-exp (bound-var body)
                     (and (not (eqv? search-var bound-var))
                          (occurs-free? search-var body)))
         (app-exp (rator rand)
                  (or (occurs-free? search-var rator)
                      (occurs-free? search-var rand))))))

(display (equal? (occurs-free? 'x (var-exp 'x)) #t))
(newline)
(display (equal? (occurs-free? 'x (var-exp 'y)) #f))
(newline)
(display (equal? (occurs-free? 'x (lambda-exp 'x (app-exp (var-exp 'x) (var-exp 'y)))) #f))
(newline)

(display (equal? (occurs-free? 'x (app-exp (var-exp 'x) (var-exp 'y))) #t))
(newline)
(display (equal? (occurs-free? 'x (lambda-exp 'y (app-exp (var-exp 'x) (var-exp 'y)))) #t))
(newline)
(display (equal? (occurs-free? 'x (app-exp (lambda-exp 'x (var-exp 'x)) (app-exp (var-exp 'x) (var-exp 'y)))) #t))
(newline)
(display (equal? (occurs-free? 'x (lambda-exp 'y 
                                     (lambda-exp 'z 
                                                 (app-exp (var-exp 'x) 
                                                          (app-exp (var-exp 'y) (var-exp 'z)))))) #t))
(newline)
(define test_exp_01 (lambda-exp 'y 
                      (lambda-exp 'z 
                                  (app-exp (var-exp 'x) 
                                           (app-exp (var-exp 'y) (var-exp 'z))))))
(eopl:pretty-print test_exp_01)
(display "========")

(define test_val_5 (var-exp 'a))
; 下面这样是不行的.
;(define test_val_6 (var-exp 'lambda))
;(define test_val_6 (lambda-exp 'lambda (app-exp (var-exp 'x) (var-exp 'y))))