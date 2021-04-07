(load "./ch2/2_15/2_15.scm")

(define occurs-free?
 (lambda (search_var exp)
  (cond
   ((var-exp? exp)
    (eqv? search_var (var-exp->var exp)))
   ((lambda-exp? exp)
    (and (not (eqv? search_var (lambda-exp->bound-var exp)))
         (occurs-free? search_var (lambda-exp->body exp))))
   (else
    (or (occurs-free? search_var (app-exp->rator exp))
        (occurs-free? search_var (app-exp->rand exp)))))))

;(occurs-free? 'x 'x)
;(occurs-free? 'x 'y)
;(occurs-free? 'x '(lambda (x) (x y)))
;(occurs-free? 'x '(lambda (y) (x y)))
;(occurs-free? 'x '((lambda (x) x) (x y)))
;(occurs-free? 'x '(lambda (y) (lambda (z) (x (y z)))))

(equal? (occurs-free? 'x (var-exp 'x)) #t)
(equal? (occurs-free? 'x (var-exp 'y)) #f)
(equal? (occurs-free? 'x (lambda-exp 'x (app-exp (var-exp 'x) (var-exp 'y)))) #f)

;(equal? (occurs-free? 'x (lambda-exp 'y (app-exp 'x 'y))) #t)
; 不属于课本eopl上的测试, 能不能直接(occurs-free? 'x ('x 'y))???而不是套几层函数, 让2_15.scm的代码自己处理这些...也就是让底下的表示刚好等于这个...todo
(equal? (occurs-free? 'x (app-exp (var-exp 'x) (var-exp 'y))) #t)

(equal? (occurs-free? 'x (lambda-exp 'y (app-exp (var-exp 'x) (var-exp 'y)))) #t)
(equal? (occurs-free? 'x (app-exp (lambda-exp 'x 'x) (app-exp (var-exp 'x) (var-exp 'y)))) #t)
(equal? (occurs-free? 'x (lambda-exp 'y 
                                     (lambda-exp 'z 
                                                 (app-exp (var-exp 'x) 
                                                          (app-exp (var-exp 'y) (var-exp 'z)))))) #t)

