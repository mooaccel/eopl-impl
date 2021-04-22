; 改成(module lang eopl)可以吗?
(module lang (lib "eopl")

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
    '((program 
        (statement) 
        a-program)

      (expression 
        (identifier) 
        var-exp)

      (expression 
        (number) 
        const-exp)

      (expression 
        ("-" "(" expression "," expression ")") 
        diff-exp)
      (expression 
        ("+" "(" expression "," expression ")") 
        addition-exp)
      (expression 
        ("*" "(" expression "," expression ")") 
        multiplication-exp)
      
      (expression
       ("zero?" "(" expression ")")
       zero?-exp)

      (expression
       ("not" "(" expression ")")
       not-exp)

      (expression
       ("let" (arbno identifier "=" expression) "in" expression)
       let-exp)

      (expression
       ("proc" "(" (arbno identifier) ")" expression)
       proc-exp)

      (expression
       ("(" expression (arbno expression) ")")
       call-exp)

      (expression
        ("letrec"
          (arbno identifier "(" (arbno identifier) ")" "=" expression )
         "in" expression)
        letrec-exp)
      
      (expression
        ("begin" expression (arbno ";" expression) "end")
        begin-exp)

      ; ==== statement

      (statement
        (identifier "=" expression)
        set-stat)

      (statement
        ("print" expression)
        print-stat)

      (statement
        ("read" identifier)
        read-stat)

      (statement
        ("{" (separated-list statement ";") "}")
        block-stat)

      (statement
        ("if" expression statement statement)
        if-stat)

      (statement
        ("while" expression statement)
        while-stat)

      (statement
        ("var" (separated-list identifier ",") ";" statement)
        declare-stat)

      ))
  ;; 自己写define-datatype, 暂时不用make-define-datatypes生成
  (define (identifier? var)
    (symbol? var))

  (define-datatype program program? 
    (a-program 
      (stat1 statement?)))

  (define-datatype expression expression?
    (const-exp
     (num number?))
    (var-exp
     (id symbol?))
    (zero?-exp
     (expr expression?))
    (not-exp
     (expr expression?))
    (diff-exp
     (exp1 expression?)
     (exp2 expression?))
    (addition-exp
     (exp1 expression?)
     (exp2 expression?))
    (multiplication-exp
     (exp1 expression?)
     (exp2 expression?))
    (let-exp
      (vars (list-of identifier?))
      (vals (list-of expression?))
      (body expression?))
    (proc-exp
     (vars (list-of identifier?))
     (body expression?))
    (call-exp
     (rator expression?)
     (rands (list-of expression?)))
    (letrec-exp
     (listof_proc_name (list-of identifier?))
     (listof_bound_vars (list-of (list-of identifier?)))
     (listof_proc_body (list-of expression?))
     (letrec_body expression?))
    (begin-exp 
     (exp1 expression?)
     (remaining_exps (list-of expression?)))
  )

  (define-datatype statement statement?
   (set-stat 
    (ident identifier?) 
    (exp1 expression?))
   (print-stat 
    (exp1 expression?))
   (read-stat 
    (ident identifier?))
   (block-stat 
    (stats (list-of statement?)))
   (if-stat
    (exp1 expression?)
    (then_stat statement?)
    (else_stat statement?))
   (while-stat 
    (exp1 expression?) 
    (stat1 statement?))
   (declare-stat
    (idents (list-of identifier?))
    (stat1 statement?))
  )

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
  
  ;(sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))
  
  ;(eopl:pretty-print (show-the-datatypes))
)