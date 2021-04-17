(define-datatype answer answer?
  (an-answer 
    (val expval?) 
    (store store?)))

(define value-of
  (lambda (exp env store) 
    (cases expression exp 
      (const-exp (num) 
        (an-answer (num-val num) 
                   store))
      (var-exp (var)
        (an-answer (apply-store store (apply-env env var)) 
                   store))  ; ?
      (if-exp (exp1 exp2 exp3) 
        (cases answer (value-of exp1 env store)
          (an-answer (val new-store)
            (if (expval->bool val) 
                (value-of exp2 env new-store) 
                (value-of exp3 env new-store))))) 
      (deref-exp (exp1) 
        (cases answer (value-of exp1 env store) 
          (an-answer (v1 new-store) 
            (let ((ref1 (expval->ref v1))) 
              (an-answer (deref ref1) new-store))))) 
      ...)))

没看懂意思, apply-store是啥? 这么传递好麻烦啊
```C
说的是这个意思?? 4.2 EXPLICIT-REFS: A Language with Explicit References
实现在ch4/p111_lang_with_explicit_ref/p111_lang_with_explicit_ref.scm, 这种实现依赖于我们知道在哪会改变store
newref-exp, setref-exp
这两个表达式可能会改变store, 4_12的实现貌似是想把store搞的每个地方都是(exp递归的时候子表达式可能会产生effect), 但是这么实现感觉不清晰啊...
```