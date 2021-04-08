#lang eopl

; S-list ::= () 
;        ::= (S-exp . S-list) 
; S-exp ::= Symbol | S-list

; define-datatype让我们忽略representation/implementation, 关注interface/specification
; 这套tool把这个流程自动化了.
; 要想用这套生成的代码, 就得遵守api规则去写...不能直接猜实现那样的去写...
; 第一种方法
(define-datatype s-list s-list?
  (empty-s-list)
  (non-empty-s-list
    (first s-exp?)
    (rest s-list?)))

(define-datatype s-exp s-exp?
  (symbol-s-exp
    (sym symbol?))
  (s-list-s-exp
    (slst s-list?)))

(define val1 (non-empty-s-list (symbol-s-exp 'a) 
                               (non-empty-s-list (symbol-s-exp 'b) 
                                                 (empty-s-list))))
(eopl:pretty-print val1)

(display (equal? (s-exp? val1) #f))
(newline)
(display (equal? (s-list? val1) #t))
(newline)

(define val2 (non-empty-s-list (s-list-s-exp val1)
                               (non-empty-s-list (symbol-s-exp 'c) 
                                                 (empty-s-list))))
(eopl:pretty-print val2)