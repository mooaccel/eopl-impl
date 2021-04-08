ch1.2.4 p19出现了第一个版本
原来第一章的版本:
```scheme
(define occurs-free?
 (lambda (var exp)
  (cond
   ((symbol? exp)
    (eqv? var exp))
   ((eqv? (car exp) 'lambda)
    (and (not (eqv? var (car (cadr exp))))
         (occurs-free? var (caddr exp))))
   (else
    (or (occurs-free? var (car exp))
        (occurs-free? var (cadr exp)))))))
```
(car (cadr exp)), (caddr exp)显示这里缺了一层抽象, 然后

ch2.3 p43
加了一层中间层, 给lambda-calculus expressions的grammar定义了interface
然后occur-free? 基于这一层中间层写的, 最终可以做到This works on any representation of lambda-calculus expressions, so long as they are built using these constructors.
```scheme
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
```
这样抽象了一层, 代码更清晰了, 代码从此可以依赖于interface, 底下实现可以进行变动.
ch2.4 p46
嫌手写constructors/predicates/extractors麻烦, 借助define-datatype和cases自动生成这些手写代码
```scheme
(define occurs-free?
  (lambda (search-var exp) 
    (cases lc-exp exp     ; 检查exp, 匹配variant
      (var-exp (var) (eqv? var search-var)) 
      (lambda-exp (bound-var body)   ; bound-var, body自己进行binding
        (and (not (eqv? search-var bound-var)) 
             (occurs-free? search-var body))) 
      (app-exp (rator rand) 
        (or (occurs-free? search-var rator) 
            (occurs-free? search-var rand))))))
```
从而不用手动调用predicates/extractors了(依赖cases), 不用手动定义constructors了(依赖define-datatype)
> 手动调用constructor还是需要的


todo occurs-free?函数的意义? 也就是在上层哪里将会被使用? 待研究, 估计eopl后面会讲到?