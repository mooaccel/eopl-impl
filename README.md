# EOPL的做题记录

## 笔记总结

eopl-impl/summary/*, 目前只看了前六章

## 环境搭建

macOS系统, IDE: vscode, Code runner插件

### ch2.4之前
在~/Library/Application Support/Code/User/setting.json修改配置, 添加如下配置
```json
    "code-runner.executorMap": {
      "scheme": "pwd && /usr/local/bin/mit-scheme $fullFileName"
    },
```
brew安装mit-scheme. 之前学sicp用的即是mit-scheme
2.4节之前用的mit-scheme没有什么问题

### 注意, 在ch2.4节之后, 由于需要使用define-datatype和cases, 改成了用racket
> 折腾了下其他的, 没搞好, 还是直接用racket省心

brew install --cask racket
相应的, vscode里的配置也要修改
```json
    "code-runner.executorMap": {
      "scheme": "pwd && /usr/local/bin/racket $fullFileName"
    },
```
之后就用#lang eopl, 就已经包含了包括define-datatype和cases在内的一些组件了, 除了些许区别(默认很多东西不输出), 其他的和之前的mit-scheme类似

### 在ch4.3节以后, 代码组织形式改成类似于 (https://github.com/mwand/eopl3) 的形式
原因: 
*. 没有全部放一个文件了, 更加清晰, 可读性强点

> ch3的那些例子因为折腾racket的module比较麻烦, 所以没有改

```C
#lang eopl
等于
(module 自定义模块名 (lib "eopl")

)
```