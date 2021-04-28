## 5.1 A Continuation-Passing Interpreter

What does a continuation represent? 

> **The continuation of an expression** represents a **procedure** that takes the result of the expression and completes the computation.

* AST里的每个exp都有它对应的continuation!!!

* continuation代表一个procedure, 接收the result of the expression(result_of_exp), 进行计算(计算一部分, 然后进入下一个cont, ... 最终到达end-cont), 最终得到FinalAnswer.

---
ch5的continuation可以重构之前第三章的LETREC语言或者第四章的IMPLICIT-REFS语言...改造的是value-of, 改成value-of/k

> 使control context explicit. 从而利用我们去掌握continuation

```scheme
ch5.1的ch5/p154_continuation_passing_inter, 即书本上的例子, 基于第三章的letrec语言, 增加continuation
ch5.1后面的习题, 前几道都是对ch5/p154_continuation_passing_inter加东西, 或者其他变种


exer5_09想使用continuation实现第四章的IMPLICIT-REFS语言, 效果应该等同于之前的IMPLICIT-REFS语言

总之, continuation可以对第三章的LETREC语言或者第四章的IMPLICIT-REFS语言在实现层面进行重构, 使之成为Continuation-Passing Interpreter
```

## 5.2 A Trampolined Interpreter

```C
如果是用C/C++实现ch5.1这种解释器的话, 感觉会出现的问题是call stack一直在增长...要很久很久以后才能返回
如果用scheme实现, 会出现这种问题吗? 宿主Scheme具有尾递归优化, 所以不用担心...

所以问题是在没有尾递归优化的语言里, 怎么实现[ch5.1的这种直到very end of the computation才能最终得到结果的计算]?
In such languages, one solution is to use a technique called trampolining.
```
> Programmers can use trampolined functions to implement tail-recursive function calls in stack-oriented programming languages. (https://en.wikipedia.org/wiki/Trampoline_(computing))

> The idea is to not make the final continuation call inside the function, but to exit and to return the continuation to a trampoline. That trampoline is simply a loop that invokes the returned continuations. Hence, there are no nested function calls and the stack won’t grow.

本质上是在中间的位置可以暂停计算? 而不用一口气算到底... 

> Each zero-argument procedure returned by apply-procedure/k represents a snapshot of the computation in progress.


---

#### 不是tail call可以用trampolining技巧吗?

> 结论: 不行.

why? 基于ch4/p120_implicit_refs, 测试
```scheme
  (eopl:pretty-print 
    (run "let f = proc (x) 
                    proc (y) 
                      begin 
                        set x = -(x,-1); 
                        -(x,y) 
                      end
          in ((f 44) 33)")
  )
不行, 问题出在value-of <<(f 44)>>返回bounce, 正常情况下应该返回proc-val.
然后紧接着被expval->proc提取, 所以失败

本质上是由于这部分的调用不是tail call导致的
```

而基于continuation改造的解释器都是tail call的