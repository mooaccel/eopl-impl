## 6.1 Writing Programs in Continuation-Passing Style

> continuation-passing style不仅可以用于ch5的解释器, 普通程序也能cps

6.1关键是理解continuation的procedural representation的变种inline化

---

注意data structure representation, procedural representation, procedural representation with inline三种方法是**等效**的, 只不过第五章大部分用的data structure representation, 然后第六章开始研究procedural representation with inline

## 6.2 Tail Form

限制解释器只解释CPS-OUT, 好处在于在inter.scm的value-of里不用再维持cont参数了. 所以第六章实现的解释器不需要管理control context...

## 6.3 Converting to Continuation-Passing Style

为了从CPS-IN得到CPS-OUT, 有算法可以进行转换, 6.3即是研究这个转化算法.(6.1手写cps转换)

---

```C
第三章的解释器只是实现了基本功能，但是需要在宿主语言里build control context（因为第三章的value-of需要不断的递归），需要宿主语言来隐式的维持continuation。第五章，显式的把continuation表示了出来, 所以对于宿主语言而言, 它执行的过程中被优化成了build no control context, 不再需要宿主语言管理control context了。因为在第五章control context已经被我们显式的放在了continuation, 当成了value-of exp env cont的参数cont进行传递...

第六章限制解释器只能解释CPS-OUT, 从而把用户层的代码也转换成了cps的, 从而ch6/p209_cps_lang/interp.scm的value-of不需要维持cont参数. CPS-OUT的AST是Continuation-Passing Style的(用户的CPS-IN程序转换后,一堆cps-proc-exp+cps-call-exp)
```

---
```C
一旦AST是 Continuation-Passing Style的,那么底下那层的解释器就不需要保存执行信息了...
上一层知道自己该干嘛(Continuation-Passing Style), 下一层就不需要保存control context了, 这是Continuation-Passing Style的好处. 这里注意可能有三层.

1. 宿主语言scheme层
2. 自己实现的解释器层, value-of这些
3. 用户代码层

如果是5.1解释器本身是Continuation-Passing Style, 那么底下的宿主语言Scheme不需要保存control context了.

如果是6.2/6.3, 更进一步, 用户层写的都是Continuation-Passing Style的AST(被6.3的算法转化), 所以底下的自己写的解释器value-of不需要cont了. 6.2/6.3里得到好处的是自己写的解释器value-of不需要保存cont!!! 所以这一层解释器不需要保存control context. 更底下的Scheme宿主语言层则和5.1没区别,都不需要保存control context, 因为第六章的解释器也是tail-call的. 

第六章的重点是用户层写的程序经过6.3算法的转换都是Continuation-Passing Style的AST, 所以底下的自己写的解释器value-of不需要维持cont了. 6.2/6.3里得到好处的是自己写的解释器value-of不需要保存cont!!!(更底下的宿主语言Scheme层和5.1一样不需要维持control context)

6.1/6.2这样的实现和5.1还是不同的...至少在处理普通的程序上面, 6.1/6.2照样在解释器层不需要保存control context(所以value-of不需要cont了...). 所以第六章6.2/6.3这么一搞, 随便用户怎么写, 最终转换后都是CPS-OUT, 即Continuation-Passing Style AST, 然后放到自己写的这层解释器执行, 它不需要维持cont.
```

## 6.4 
未看