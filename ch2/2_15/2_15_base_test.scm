(load "./ch2/2_15/2_15.scm")

; 测试直接这么定义是不太行的, 会不符合grammer
; 应该像occur_free_test.scm那样... todo
(define var_exp_val1 (var-exp 'mm))
(define lambda_exp_val1 (lambda-exp 'var1 '(+ x (+ y 100))))
(define app_exp_val1 (app-exp 'var1 'var2))
(equal? var_exp_val1
        '(var-exp mm))
(equal? app_exp_val1
        '(app-exp var1 var2))
(equal? lambda_exp_val1 
        '(lambda-exp lambda var1 (+ x (+ y 100))))

(equal? (var-exp? var_exp_val1) #t)
(equal? (app-exp? app_exp_val1) #t)
(equal? (lambda-exp? lambda_exp_val1) #t)

(equal? (var-exp? lambda_exp_val1) #f)
(equal? (app-exp? var_exp_val1) #f)

(equal? (var-exp->var var_exp_val1) 'mm)
(equal? (lambda-exp->bound-var lambda_exp_val1) 'var1)
(equal? (lambda-exp->body lambda_exp_val1) '(+ x (+ y 100)))
(equal? (app-exp->rator app_exp_val1) 'var1)
(equal? (app-exp->rand app_exp_val1) 'var2)