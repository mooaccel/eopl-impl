; 类似于2_12怎么定义queue? sicp3_22.scm

; 制造lambda
(define (empty-stack)
  (lambda (mt)
    (cond ((eqv? mt 'top)
            (error "empty-stack err, mt = " mt))
          ((eqv? mt 'pop)
            (error "empty-stack err, mt = " mt))
          (else (error "empty-stack don't support this mt" mt)))))

; 制造lambda
(define (push stk var)
  (lambda (mt)
    (cond ((eqv? mt 'top)
            var)
          ((eqv? mt 'pop)
            stk)
          (else (error "push don't support this mt" mt)))))

(define (pop stk)
  (stk 'pop))

(define (top stk)
  (stk 'top))

; ; todo 怎么定义?
; (define (empty-stack? stk)
;   ())


(define empty_stk (empty-stack))
(define x1 (push empty_stk 1))
(define x2 (push x1 2))
(define x3 (push x2 3))
(define x2_2 (push x1 20))

(equal? (top x3) 3)
(equal? (top x2) 2)
(equal? (top x1) 1)
;(top empty_stk)
(equal? (top (pop (pop x3))) 1)
;(pop x2)
(equal? (top x3) 3)
(equal? (top x2) 2)
(equal? (top x1) 1)


; 还可以这样...
(equal? (top x2_2) 20)
(equal? (top (pop x2_2)) 1)