5.3实现Imperative Interpreter

重构ch5/p154_continuation_passing_inter, 本质上背后的计算过程并没有什么变化


--- 

* 很容易重构产生bug, 书上也提到了3个问题, 被第2个问题 名字冲突导致的坑了(已经修复)

* 第三个问题, 先把cont放在前面, 在目前的例子里也可以先不用, 还是这么做比较好
> 最好还是把set! cont这种放前面, 虽然现在没有问题, 但是怕以后基于这个例子修改可能产生bug.