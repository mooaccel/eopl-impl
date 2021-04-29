基于5_06


---

一点分析
在apply-cont if-test-cont的时候
```scheme
              (value-of/k exp2 saved_env saved_cont)
              (value-of/k exp3 saved_env saved_cont)
```
会出现分叉, 如果进入value-of/k raise-exp

> raise-exp的cont是什么? 感觉是一层一层的套在try-cont之上的

得到异常值, 即raise后面的expression得到的expval
然后执行apply-handler, 一层一层剥开cont, 直到发现try-cont, 执行exception handler, 然后将handler_exp的结果送入saved_cont(saved_cont是try-exp的continuation, 然后接着处理上层逻辑, 另外一条路是通过正常路径, 把正常值送入saved_cont, 然后接着处理上层逻辑)