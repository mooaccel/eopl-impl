5.3实现Imperative Interpreter

重构ch5/p154_continuation_passing_inter, 本质上背后的计算过程并没有什么变化


--- 

很容易重构产生bug, 书上也提到了3个问题, 被第2个问题 名字冲突导致的坑了,(已经修复)

第三个问题, 先把cont放在前面, 在目前的例子里也可以先不用, 因为还没有出现问题, todo 待放在前面