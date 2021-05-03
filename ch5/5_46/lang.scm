; 改成(module lang eopl)可以吗?
(module lang (lib "eopl")

  ;; language for EXPLICIT-REFS
  
  (require "drscheme-init.scm")
  
  (provide (all-defined-out))  ; 里面包含expression?, provide应该要在当前文件执行完成之后才能知道哪些需要导出吧?

  ;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;
  
  (define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier
       (letter (arbno (or letter digit "_" "-" "?")))
       symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))
  
  (define the-grammar           ; 经过sllgen处理, 生成define-datatype那些, 然后就有了expression?
    '((program (expression) a-program)

      (expression 
        (identifier) 
        var-exp)

      (expression 
        (number) 
        const-exp)
      
      (expression 
        ("[" (separated-list number ",") "]")
        const-list-exp)

      (expression 
        ("-" "(" expression "," expression ")") 
        diff-exp)

      (expression 
        ("*" "(" expression "," expression ")") 
        multiplication-exp)
      
      ;(expression
      ; ("zero?" "(" expression ")")
      ; zero?-exp)

      (expression
       ("if" expression "then" expression "else" expression)
       if-exp)

      (expression
       ("let" identifier "=" expression "in" expression)
       let-exp)   

      (expression
       ("proc" "(" identifier ")" expression)
       proc-exp)

      (expression
       ("(" expression expression ")")
       call-exp)

      (expression
        ("letrec"
          identifier "(" identifier ")" "=" expression
         "in" expression)
        letrec-exp)
      ;(expression
      ;  ("letrec"
      ;    (arbno identifier "(" identifier ")" "=" expression)
      ;   "in" expression)
      ;  letrec-exp)
    
      ;; new for explicit-refs
      (expression
        ("begin" expression (arbno ";" expression) "end")
        begin-exp)

      (expression
        ("set" identifier "=" expression)
        set-exp)
      
      (expression
        ("spawn" "(" expression ")")
        spawn-exp)

      (expression
        ("spawn" "(" expression ")")
        spawn-exp)
      
      (expression
        ("mutex" "(" ")")
        mutex-exp)
      
      (expression
        ("wait" "(" expression ")")
        wait-exp)

      (expression
        ("signal" "(" expression ")")
        signal-exp)

      (expression 
        ("yield")
        yield-exp)
      
      (expression
        (unop "(" expression ")")
        unop-exp)

      (unop ("car") car-unop)
      (unop ("cdr") cdr-unop)
      (unop ("null?") null?-unop)
      (unop ("zero?") zero?-unop)  ; 需要一个数作为expression
      (unop ("print") print-unop)

      ))

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
  
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))
  
  ;(eopl:pretty-print (show-the-datatypes))
  
  )