### EOPL的做题记录

macOS系统, IDE: vscode, Code runner插件
在~/Library/Application Support/Code/User/setting.json修改配置, 添加如下配置
brew安装mit-scheme? 之前sicp用的mit-scheme
```json
    "code-runner.executorMap": {
      "scheme": "pwd && /usr/local/bin/mit-scheme $fullFileName"
    },
```
#### 注意, 在ch2.4节之后, 由于需要使用define-datatype和cases, 改成了用racket
> 折腾了下其他的, 没搞好

brew install --cask racket
相应的, vscode里的配置也要修改
```json
    "code-runner.executorMap": {
      "scheme": "pwd && /usr/local/bin/racket $fullFileName"
    },
```
之后就用#lang eopl, 就已经包含了包括define-datatype和cases在内的一些组件了, 除了些许区别(默认很多东西不输出), 其他的和之前的mit-scheme类似
