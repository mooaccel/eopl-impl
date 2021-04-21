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

> 从4.3以后, 参考了https://github.com/mwand/eopl3代码的组织方式, 还是放到多个文件里更清晰.

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

```C
IMPLICIT-REFS语言所有的var-exp都是引用...
value of var-exp 需要 (deref (apply-env env var))
```

```C
IMPLICIT-REFS的引用有关的这些操作在concrete syntax层看不见...所以隐式, 
而explicit refs语言需要在语法层指明...所以是显式
```

* 一个问题
```C
ch4.3的这句话
This design is called call-by-value, or implicit references.
怎么理解?
主要是This design is called call-by-value不太懂.
```

---

* 目前的例子里面(ch4.2/ch4.3)的这些, store的实现都非常naive

* 注意一点, 貌似ch4.2/ch4.3目前这些例子的store都是在不断的扩充(利用), 而不回收...(ref数字在不断增大)