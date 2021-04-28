## 4.1 Computational Effects 
理解effect

## 4.2 EXPLICIT-REFS: A Language with Explicit References
用EXPLICIT-REFS举例子. 代码参考ch4/p111_lang_with_explicit_ref
和ch4/p111_explicit_refs

---

4.2 EXPLICIT-REFS的ExpVal和DenVal分别是(相比第三章的PROC语言, 多了Ref(ExpVal) ):
```
ExpVal = Int + Bool + Proc + Ref(ExpVal)
DenVal = Int + Bool + Proc + Ref(ExpVal)
```

## 4.3 IMPLICIT-REFS: A Language with Implicit References

* IMPLICIT-REFS语言在EXPLICIT-REFS的基础上改成了所有的var-exp都是引用.

> 从4.3以后, 参考了 (https://github.com/mwand/eopl3) 代码的组织方式, 还是放到多个文件里更清晰.

### two-level查找
> When a variable appears in an expression, we first look up the identifier in the environment 
> to find the location to which it is bound, and then we look up in the store to find the value 
> at that location. Hence we have a “two-level” system for var-exp.

```C
ident(var) -> ref(location) -> val(expval)
```

---

IMPLICIT-REFS语言的env全是ref, 不再直接存expval, 即
```
ExpVal = Int + Bool + Proc 
DenVal = Ref(ExpVal)
```
指到学会IMPLICIT-REFS语言的设计, 才体会到之前ch3所说的ExpVal可以和DenVal不同是什么意思...
> eopl ch3.2.2 p61 Chapter 4 presents languages in which expressed and denoted values are different. 指的就是这里
```C
既然学到了这里, 再回过头来理解下第三章的
Each language has at least two such sets: the expressed values and the denoted values. 
The expressed values are the possible values of expressions, and the denoted values are 
the values bound to variables.
denoted values代表variables绑定到什么值上面, 体现在实现上就是env的variable value pair的value
在env里varibale binding to value, value的范围即denoted values集合.
```

```C
IMPLICIT-REFS语言所有的var-exp都是引用...
value of var-exp 需要 (deref (apply-env env var))
```

```C
IMPLICIT-REFS的引用有关的这些操作在concrete syntax层看不见...所以隐式, 
而explicit refs语言需要在语法层指明...所以是显式
```
---

#### 一个问题

```C
ch4.3的这句话
This design is called call-by-value, or implicit references.
怎么理解?
主要是This design is called call-by-value不太懂.
```
一些理解如下(猜测为什么叫call-by-value可能有如下原因):
```scheme
; 在ch4/p120_implicit_refs运行以下代码
; 比如这段代码: 会产生新的一份存储吧.
  (instrument_let #t)
  (instrument_newref #t)
  (eopl:pretty-print 
    (run "let aa = 100
          in let f = proc (x)
                      begin
                        set x = -(x,-1); 
                        -(x,10)
                      end
             in (f aa)"))
可以发现x和aa binding到了不同的content, 也即在store处于不同的位置, 它们是不同的两个变量了!
; todo 其他更好的理解待后续再补充
```

---

* 目前的例子里面(ch4.2/ch4.3)的这些, store的实现都非常naive

* 注意一点, 貌似ch4.2/ch4.3目前这些例子的store都是在不断的扩充(利用), 而不回收...(ref数字在不断增大)

### 通过4.22/23/24...这几道题, 理解expression和statement的区别

>  基于4.22这个语言, 设计出来的程序有点像C/C++风格, statement通过side effect实现计算

类似4.3 IMPLICIT-REFS语言
4_22的ExpVal和DenVal也是:
```
ExpVal = Int + Bool + Proc 
DenVal = Ref(ExpVal)
```

---

```C
4.22题目的背后重点是添加了statement!

然后通过做这几道题理解了expression和statement的区别, 之前的例子一直都是在用expression的
expression-oriented风格是之前章节的风格, 这种风格普遍出现在函数式编程语言里. 
* All functional programming languages are expression-oriented(https://en.wikipedia.org/wiki/Expression-oriented_programming_language)
```

```
expression和statement的比较重要的区别是:
"Statements do not return results and are executed solely for their side effects, 
while expressions always return a result and often do not have side effects at all." (https://alvinalexander.com/scala/fp-book/note-about-expression-oriented-programming/)
* statement依赖side effect, 而expression倾向于没有side effect

```

#### 扩展部分(eopl上没有的)

C/C++普遍使用statements, 依赖side effect, 用于表示各种状态.
FP家族的语言, 倾向于没有side effect.
> In functional programming, side effects are rarely used. The lack of side effects makes it easier 
> to do formal verifications of a program. Functional languages such as Standard ML, Scheme and Scala 
> do not restrict side effects, but it is customary for programmers to avoid them. (https://en.wikipedia.org/wiki/Side_effect_(computer_science))

```C
还有一个概念是Referential transparency(https://en.wikipedia.org/wiki/Referential_transparency)
Referential transparency and referential opacity are properties of parts of computer programs. 
An expression is called referentially transparent if it can be replaced with 
its corresponding value (and vice-versa) without changing the program's behavior.
This requires that the expression be pure, that is to say the expression value must be 
the same for the same inputs and its evaluation must have no side effects. 
An expression that is not referentially transparent is called referentially opaque.
```
有Referential transparency才能memoization optimization...

## 4.4 MUTABLE-PAIRS: A Language with Mutable Pairs

ch4.4在ch4.3IMPLICIT-REFS语言的基础上增加mutable-list
关键是需要注意ch4/p125_mutable_pairs/p125示例图解.png中所示的问题, 怎么进行共享的. 其实类似于C/C++传指针...

## 4.5 Parameter-Passing Variations

### natural parameter passing
ch3的语言是Natural parameter passing的, denoted value与expressed value一样.

### call-by-value
从ch4.3 IMPLICIT-REFS开始, 出现call-by-value, denoted value是expressed value的引用

### call-by-reference

有些时候传location可能更好, 不重新分配expressed value, 多个ref(location)共享一个expressed value

ch4.5.1 call-by-reference

> Under call-by-reference, variables still denote references to expressed values, just as they did under call-by-value(ch4 p132):
``` 
ExpVal = Int + Bool + Proc 
DenVal = Ref(ExpVal)
```
> The only thing that changes is the allocation of new locations.

> 后面的习题4.35是道不错的题, 达到的效果是: 语言还是处于call-by-value框架下, 但是在局部范围设计成可以支持call-by-reference

> C/C++传指针就属于call-by-reference

---

前面讨论的parameter-passing mechanisms都是eager的, 4.5.2 开始转向lazy evaluation

> Under lazy evaluation, an operand in a procedure call is not evaluated until it is needed by the procedure body. If the body never refers to the parameter, then there is no need to evaluate it.

### call-by-name

Call by name if it is used several times, it is re-evaluated each time it appears. 

> call-by-name没有做什么习题, 可能理解上还没那么深刻

### call-by-need

Call by need is a [memoized](https://en.wikipedia.org/wiki/Memoization) variant of call by name

If the argument is [pure](https://en.wikipedia.org/wiki/Pure_function) (i.e., free of side effects), this produces the same results as call by name, saving the cost of recomputing the argument.

如果impure, call by need和call by name那么就可能不一样了...

> Haskell (Hudak et al., 1990) was the first practical language to use call-by-need. [附录A]