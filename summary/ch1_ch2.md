先看了sicp三章, 并做了大部分习题之后来看的eopl

> 看eopl之前最好有scheme的一些基础

ch1, ch2虽然感觉有些内容和sicp的差不多, 但还是值得一看, 尤其ch2

### ch1
讲了一些基本概念, 复习递归

### ch2 Data Abstraction
2.1 强调interface的重要性.
* Data abstraction divides a data type into two pieces: an interface and an implementation.

2.2 强调Strategies, 也就是可以有多种表示
> 2.2.1 介绍了env interface, 用env举例子
> 2.2.2 介绍了普通的Data Structure Representation
> 2.2.3 介绍了Procedual Representation, 感觉本质上貌似就是具有多态性质的OO? 不同对象的同一种method可以表现出不同的行为(m * n). m种对象, n种方法

> Procedual Representation是一种representation技术. 为了实现interface, 也可以采用其他的表示

2.3 强调设置一个中间层的好处 
拿occurs-free?函数举例子, 重新实现section 1.2.4的例子, 之前的可读性比较差, 也就是抽象的不好, 然后2.3加了个中间层.
再手写实现这个中间层, 然后基于这个中间层再去实现occurs-free?就更加清晰.

2.4 define-datatype/cases工具
考虑到手写的麻烦, 引入了define-datatype/cases工具, 自动生成这部分代码. (define-datatype/cases实现原理待探究.todo), 
这其实是一种dsl, 如果在需要compact/efficient的地方, 可能手写更好.
2.5 concrete syntax(or external representation)与abstract syntax(internal representation)之间的关系
concrete syntax => ast, parse
ast => concrete syntax， unparse
> abstract syntax, 就是日常中看到的语法, 可以改变

> ast是内部表示, 也可以改变.


---
ch1, ch2里的很多题都挺不错, 值得做一做.