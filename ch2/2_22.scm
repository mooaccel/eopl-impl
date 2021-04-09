#lang eopl
; Using define-datatype, implement the stack data type of exercise 2.4
; empty-stack, push, pop, top, and empty-stack?.
; stack-exp := (empty-stack) 
;           := (extend-stack val stack-exp) 

; todo
(define (scheme-val? var)
  #t)

(define-datatype stack-exp stack-exp?
  (empty-stack)
  (extend-stack
    (stack_exp stack-exp?)
    (scheme_val scheme-val?)))

; 不然就把extend-stack改成push, 然后去掉这层封装
(define (push stk val)
  (extend-stack stk val))

(eopl:pretty-print (empty-stack))
(define test_stack_01 (push (empty-stack)
                            10))
(define test_stack_02 (push test_stack_01
                            20))
(define test_stack_03 (push test_stack_02
                            30))
(eopl:pretty-print test_stack_01)
(eopl:pretty-print test_stack_02)

(eopl:pretty-print test_stack_03)
; 输出如下:
; #(struct:extend-stack
;   #(struct:extend-stack #(struct:extend-stack #(struct:empty-stack) 10) 20)
;   30

; 可以存不同元素
(eopl:pretty-print (push test_stack_03 'sym))

(define (pop stk)
  (cases stack-exp stk
    (empty-stack ()
      #f)
    (extend-stack (stack_exp scheme_val)
      stack_exp)))
(display "===")
(newline)
(eopl:pretty-print (pop test_stack_03))
(eopl:pretty-print test_stack_03)
(eopl:pretty-print (pop (pop test_stack_03)))
(eopl:pretty-print (pop (pop (pop test_stack_03))))
(display (equal? (pop (empty-stack)) #f))
(newline)

(display "===")
(newline)

(define (top stk)
  (cases stack-exp stk
    (empty-stack ()
      #f)
    (extend-stack (stack_exp scheme_val)
      scheme_val)))

(display (equal? (top test_stack_03) 30))
(display (equal? (top (pop test_stack_03)) 20))
(display (equal? (top (pop (pop test_stack_03))) 10))
(display (equal? (top (pop (pop (pop test_stack_03)))) #f))
(newline)

(define (empty-stack? stk)
  (cases stack-exp stk
    (empty-stack ()
      #t)
    (extend-stack (stack_exp scheme_val)
      #f)))

(display (equal? (empty-stack? (empty-stack)) #t))
(display (equal? (empty-stack? test_stack_03) #f))