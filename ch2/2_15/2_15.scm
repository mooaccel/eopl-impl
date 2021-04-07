; 存'var还是var的value?
; 这里待探究. 存符号...
; var 用'fdsf 这种符号进行调用? 是了... 见2_15_base_test.scm
(define (var-exp var)
  (cons 'var-exp 
        (list var)))
; todo,这里是只支持一个参数么?
(define (lambda-exp var exp_body)
  (cons 'lambda-exp
        (list 'lambda var exp_body)))
(define (app-exp exp1 exp2)
  (cons 'app-exp
        (list exp1 exp2)))

(define (var-exp? exp)
  (let ((tag_exp (car exp)))
    (if (eqv? tag_exp 'var-exp)
        #t
        #f)))
(define (lambda-exp? exp)
  (let ((tag_exp (car exp)))
    (if (eqv? tag_exp 'lambda-exp)
        #t
        #f)))
(define (app-exp? exp)
  (let ((tag_exp (car exp)))
    (if (eqv? tag_exp 'app-exp)
        #t
        #f)))

(define (var-exp->var exp)
  (if (var-exp? exp)
      (cadr exp)
      (error "var-exp? predicate err")))
(define (lambda-exp->bound-var exp)
  (if (lambda-exp? exp)
      (car (cddr exp))
      (error "lambda-exp? predicate err")))
(define (lambda-exp->body exp)
  (if (lambda-exp? exp)
      (car (cdr (cddr exp)))
      (error "lambda-exp? predicate err")))
(define (app-exp->rator exp)
  (if (app-exp? exp)
      (car (cdr exp))
      (error "app-exp? predicate err")))
(define (app-exp->rand exp)
  (if (app-exp? exp)
      (car (cddr exp))
      (error "app-exp? predicate err")))