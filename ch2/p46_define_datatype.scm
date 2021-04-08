#lang eopl

(define (identifier? var)
  (symbol? var))

; LcExp ::= Identiﬁer
;       ::= (lambda (Identifier) LcExp) 
;       ::= (LcExp LcExp)
(define-datatype lc-exp lc-exp?
    (var-exp (var identifier?))
    (lambda-exp (bound-var identifier?) (body lc-exp?))
    (app-exp (rator lc-exp?) (rand lc-exp?)))

; lc-exp, type-name, 可以取其他名字, 会在cases lc-exp中用到
; lc-exp? 这个符号经过define-datatype运行后会绑定到一个predicate上, 用于判断是不是lc-exp
; 每个分支有个variant-name, define-datatype会创造与其同名的constructor procedure
; (field-name predicate), 创建这种variant需要的参数列表, 其中每个参数都需要满足其对应的predicate
; 只有满足了这些条件才能构造成功对应的variant

; 相比2_15, 手写constructors, predicates, extractors, 通过define-datatype替代了constructors的工作, 会给我们创建好各个variant的constructor
; cases中会自动使用predicates(即替代了2_15中var-exp?, lambda-exp?, app-exp?这些函数), 模式匹配?
; 然后顺便自动binding了变量...
; identifier?这种predicate需要手动传进去
; lc-exp?会被binding到一个总的判断是不是lc-exp的predicate上


; 所以define-datatype和cases怎么实现的??? todo 非常重要的问题, 关于这个dsl tool的原理
(define occurs-free?
 ; exp是参数名字, 可以随便取
 (lambda (search-var exp)
  (cases lc-exp  ; lc-exp, 类型名
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
;  ;(equal? (occurs-free? 'x (lambda-exp 'y (app-exp 'x 'y))) #t)
;  ; 不属于课本eopl上的测试, 能不能直接(occurs-free? 'x ('x 'y))???而不是套几层函数, 让2_15.scm的代码自己处理这些...也就是让底下的表示刚好等于这个...todo
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
(cases lc-exp
       test_exp_01
       (var-exp (var) 
        (eopl:pretty-print var))
       (lambda-exp (bound-var body)
        (newline)
        (eopl:pretty-print bound-var)
        (newline)
        (eopl:pretty-print body))
       (app-exp (rator rand)
        (eopl:pretty-print rand)))
         ;         (or (occurs-free? search-var rator)
         ;             (occurs-free? search-var rand)))))