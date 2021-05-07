
在5_08的基础上
1.让letrec具备多参数 
2.增加multiplication-exp

---
然后实现cps fact, 看看cont

看完了6.2之后回头研究的这个ch5/ch5_1_user_pgm_test
这是一个对比分析, 基于5.1这种解释器
```scheme
对比分析以下两段程序:
(eopl:pretty-print 
 (run
   " 
   letrec
       fact(x) = if zero?(x) 
                 then 1 
                 else *(x, (fact -(x, 1)))
    in (fact 6)" 
 ))

(eopl:pretty-print (run
  " 
  letrec factk(n cont) = if zero?(n) 
                         then (cont 1)
                         else (factk -(n, 1)
                                       proc (val) 
                                          (cont *(n, val)))
  in (factk 5 proc (val) 
                val)
"
))
```