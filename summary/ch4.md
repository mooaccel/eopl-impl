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