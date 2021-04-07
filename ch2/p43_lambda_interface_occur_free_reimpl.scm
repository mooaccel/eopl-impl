; 原来第一章的版本:
(define occurs-free?
 (lambda (var exp)
  (cond
   ((symbol? exp)
    (eqv? var exp))
   ((eqv? (car exp) ’lambda)
    (and (not (eqv? var (car (cadr exp))))
         (occurs-free? var (caddr exp))))
   (else
    (or (occurs-free? var (car exp))
        (occurs-free? var (cadr exp)))))))

; 加了一层中间层, 给lambda-calculus expressions的grammar定义了interface
; 然后occur-free? 基于这一层中间层写的, 最终可以做到This works on any representation of lambda-calculus expressions, so long as they are built using these constructors.
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
