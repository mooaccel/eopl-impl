## 6.1 Writing Programs in Continuation-Passing Style

> continuation-passing style不仅可以用于ch5的解释器, 普通程序也能cps

6.1关键是理解continuation的procedural representation的变种inline化

---

注意data structure representation, procedural representation, procedural representation with inline三种方法是**等效**的, 只不过第五章大部分用的data structure representation, 然后第六章开始研究procedural representation with inline