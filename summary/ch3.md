### 3 Expressions

第二章结尾讲了AST的基础之后, 第三章开始研究Expression

#### 3.1  
(value-of exp ρ) = val

把1/2章铺垫的env用上了. 用于Expression求值

#### 3.2 LET: A Simple Language
最简单的解释器, 看完3.2的这些铺垫之后, 直接上代码, 把代码跑通, 然后做几个练习就明白了, 明白了整个处理流程

> 注意, scan&parse 想使用的话参考3_06/07/08的*_using_sllgen.scm例子. 
> 有些细节待研究.todo,比如: 1.sllgen生成define-datatype原理?

整体处理流程:
1. scan  生成各种token

2. parse 生成AST

3. value-of 对AST在env中进行求值

> 1,2属于font-end, 这部分不是这本书的重点研究的内容, sllgen(Scheme LL Generator)做了这部分的主要工作.
> sllgen 写好词法解析的lexical-spec和语法解析的grammar, 然后定义scan&parse函数, 即可靠sllgen 生成lexer和parser

```scheme
(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))
```

> 借助sllgen:make-define-datatype还可以直接生成define-datatype, 不用手写define-datatype了...
> 即下面这种代码都可以不用写

```scheme
(define-datatype expression expression?
 (const-exp 
  (num number?))
 (minus-exp
  (body_exp expression?))
 (diff-exp 
  (exp1 expression?) 
  (exp2 expression?))
 (addition-exp
  (exp1 expression?)
  (exp2 expression?))
 ...
```

> sllgen:make-define-datatype可以生成, 借助sllgen:list-define-datatype可以查看, 和手写的略微有点小区别.
> sllgen:make-define-datatype生成define-datatype, 然后define-datatype生成ch2讲的那些底下具体实现(constructors/predicates/extractors), 最终用户这边可以直接基于cases在上面开发value-of即可...


```scheme
(define (value-of exp env)
  ...)
```
value-of输入expression, 即AST. 输出ExpVal, 即已求值好的值(#(struct:bool-val #t) #(struct:num-val 2)这样, 包装了一层, 在基础的值上面)

* 关于ExpVal的一些细节:

```C
num-val : Int -> ExpVal        ⌈ ⌉    
expval->num : ExpVal -> Int    ⌊ ⌋

We also write ⌈ n ⌉ in place of (num-val n), and ⌊ val ⌋ in place of (expval->num val).

num-val       ⌈ n ⌉      ⌈ 里面是Int ⌉   
expval->num   ⌊ val ⌋    ⌊ 里面是ExpVal ⌋ , 经过⌊⌋之后变成Int
```

> 总之, 就是在ExpVal 和 Int/Bool之间互转, ExpVal是value-of内部使用的representation

> 这本书按照一步一步, 从易到难, 一步一步增量式加feature, 给的代码示例, 以及后面的习题都非常好, 学习之后帮助挺大.

#### 3.3 PROC: A Language with Procedures

在3.2的基础上增加procedure, 作为first class function?

这一小节最为关键的是让我彻底理解了closure的原理以及实现, 理解closure的前置条件是理解清楚ch1/ch2的env

> 注意这里的closure(闭包), 是偏实践上下文下的closure, 另外在偏数学/理论场景下有一种closure性质, 和这里的不同, 那里的大概是操作施加到一个集合上面, 最终元素还在集合里, 形成自我包含的意思?

> 3.2这里的闭包:
> These data structures are often called closures, because they are self-contained: they contain everything the procedure needs in order to be applied.

3.3.1 介绍了procedure的基本原理

3.3.2 开始介绍实现技术, 书中列举了两种 (注意都是实现技术, interface/specification是一样的)

1. interface/specification: 

> proc? : SchemeVal → Bool
> procedure : Var × Exp × Env → Proc
> apply-procedure : Proc × ExpVal → ExpVal

关键都是把env保存下来, 然后在调用的时候拿出来, 把var val pair放入env, 再对body求值即可

2. implementation (precedure的)

* 一种方式是用2.2.3介绍的宿主语言所在的procedural representation. 在本书中宿主语言就是Scheme, 所以实现如下:

```scheme
(define proc?
  (lambda (val) 
    (procedure? val)))
(define procedure
  (lambda (var body env) 
    (lambda (val) 
      (value-of body (extend-env var val env)))))  ; procedure所在的env被放到了lambda里, 即所谓的capture?
(define apply-procedure
  (lambda (proc1 val) 
    (proc1 val)))
```
* 另一种方式是借助define-datatype, 让它帮我们实现, 帮我们选一种representation

```scheme
(define-datatype proc proc?      ; define-datatype这里提供了proc? predicate
  (procedure 
    (var identifier?) 
    (body expression?) 
    (saved-env environment?)))
(define apply-procedure
  (lambda (proc1 val) 
    (cases proc proc1 
      (procedure (var body saved-env)  ; 把需要的值extract出来
        (value-of body 
                  (extend-env var val saved-env))))))
```scheme