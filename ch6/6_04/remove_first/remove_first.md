最初的版本是是section 1.2.3定义的

---

然后想cps变换, 成为Continuation-Passing Style
```C
得到这三个版本
remove_first_data_structure_repre.scm
remove_first_procedural_repre.scm
remove_first_procedual_inline.scm

其实都进行了cps变换, 写成了Continuation-Passing Style. 本质上都是在改变cont的表示
```

--- 

```C
然后还可以运用5.3的registerized. 对上面三种的任意一种进行优化
得到ch6/6_04/remove_first/registerized文件夹下的三种版本

对remove_first_data_structure_repre.scm registerized化最简单
remove_first_procedural_repre.scm需要改变一下lambda为无参数
remove_first_procedual_inline.scm需要引入额外的cont_snapshot变量
```

> 可以在这个基础上继续运用trampoline. 就不继续做了

总之.

```C
remove_first_data_structure_repre.scm
remove_first_procedural_repre.scm
remove_first_procedual_inline.scm
都是cps变换

这是一个维度, 然后ch5.3的registerized又是一个维度, ch5.2的trampoline是一个维度
这三个维度正交, 可以进行组合. registerized, trampoline是优化技术(意味着也可以没有这种优化)

```


> 第六章6.1的重点是学习inline化表示cont