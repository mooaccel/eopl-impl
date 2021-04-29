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

5.2讲了一种优化技术trampolining

```C
如果是用C/C++实现ch5.1这种解释器的话, 感觉会出现的问题是call stack一直在增长...要很久很久以后才能返回
如果用scheme实现, 会出现这种问题吗? 宿主Scheme具有尾递归优化, 所以不用担心...

所以问题是在没有尾递归优化的语言里, 怎么实现[ch5.1的这种直到very end of the computation才能最终得到结果的计算]?
> 这就是优化的问题, 否则在没有尾递归优化的语言里, 可能会产生很深的call stack

In such languages, one solution is to use a technique called trampolining.
```
> Programmers can use trampolined functions to implement tail-recursive function calls in stack-oriented programming languages. (https://en.wikipedia.org/wiki/Trampoline_(computing))

> The idea is to not make the final continuation call inside the function, but to exit and to return the continuation to a trampoline. That trampoline is simply a loop that invokes the returned continuations. Hence, there are no nested function calls and the stack won’t grow.

本质上是在中间的位置可以暂停计算? 而不用一口气算到底... 
> 因为可以暂停计算, 恢复的时候可以获得所有必要的执行信息, 所以可以设计coroutine这种机制, 或者ch5.5 Threads?

> Each zero-argument procedure returned by apply-procedure/k represents a **snapshot** of the computation in progress.

> 感觉任何地方都可以加一层lambda () (...), 然后可以暂停?(毕竟是snapshot, 具有接下来运行所需的所有信息), 形象点的理解就是把一段计算分成多个子部分(想在哪划分, 就把lambda () (...)这一层套上去就行). 

---

#### 不是tail call可以用trampolining技巧吗?

> 结论: 貌似不行.

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

> 但是感觉如果目的是为了得到计算的snapshot, 从而形成暂停, 恢复, 可以用其他技术实现保存现场, 恢复现场吧. 比如内核里的中断处理, process context switch, 用户态coroutine context switch等


## 5.3 An Imperative Interpreter
ch5.3将ch5.1的例子重构为Imperative Interpreter
> 参考ch5/p167_imperative_interpreter/p167_imperative_interpreter.md

> 5.3是一种优化技术, 本质上计算过程还是ch5.1的那些

**A 0-argument tail call is the same as a jump.**

```C
0-argument tail call类似于goto这种jump

不是tail call的话, 可能其他地方还存在control context信息(比如fact的实现), 光用一个program counter转移执行位置, 而不进行上下文切换之类的可能还不行. (如果不是tail call, 直接jump不行, 本质原因感觉是由于上下文信息不足, 所以不能简单的只有program counter决定转移)

但是如果获取到了足够多的信息, 就可以简单的jump, (比如5.3把信息保存在全局变量里), 这种优化把带参数的procedure transform成了不带参数的procedure, 形成了tail call
这样跳转之后, 所需要的上下文信息都有, 所以可以简单的改成jump
```

5.3的优化让我们更加贴近机器语言一点

---

```C
一个猜想. 不知道对不对.

学完5.1/5.2/5.3之后的一个猜想: 任何递归程序(不止interpreter吧? 毕竟interpreter也只是一个普通程序), 都可以用contination这种方式改造吧? 然后变成tail-call形式. 这就是CPS吗? 感觉得看了ch6之后才能更加确认.

然后再经过5.2trampolining优化, 所以可以分成多段, 不至于爆栈(如果在没有尾递归优化的语言里), 不用一口气算完

如果再用上5.3的技术就可以改造成更加贴近机器语言, 更好用底层实现, 然后用类似goto这种jump替代0-argument tail call
```

## 5.4