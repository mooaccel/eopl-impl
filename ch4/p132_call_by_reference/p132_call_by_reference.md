ch4/p132_call_by_reference基于4_19的IMPLICIT-REFS语言, 在它的基础上改apply-procedure有关的部分

> 同样的例子可以在ch4/p132_call_by_reference和ch4/4_19中都运行一下, 对比加深理解

它们的ExpVal, DenVal是一样的:

```scheme
ExpVal Int + Bool + Proc
DenVal Ref(ExpVal)
```

就一点细微差别(即ch4/p132_call_by_reference关注的重点)