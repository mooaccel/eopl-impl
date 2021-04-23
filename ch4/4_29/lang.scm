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
        ("-" "(" expression "," expression ")") 
        diff-exp)
      
      (expression
       ("zero?" "(" expression ")")
       zero?-exp)

      (expression
       ("if" expression "then" expression "else" expression)
       if-exp)

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
      
      ;; new for explicit-refs
      (expression
        ("begin" expression (arbno ";" expression) "end")
        begin-exp)

      (expression
        ("set" identifier "=" expression)
        assign-exp)
      
      (expression
        ("newarray" "(" expression "," expression ")")
        newarray-exp)

      (expression
        ("arrayref" "(" identifier "," expression ")")
        arrayref-exp)

      (expression
        ("arrayset" "(" identifier "," expression "," expression ")")
        arrayset-exp)

      ))
  ;; 自己写define-datatype, 暂时不用make-define-datatypes生成
  (define (identifier? var)
    (symbol? var))

(define-datatype program program? 
  (a-program 
    (exp1 expression?)))

(define-datatype expression expression?
  (const-exp
   (num number?))
  (var-exp
   (id symbol?))
  (zero?-exp
   (expr expression?))
  (diff-exp
   (exp1 expression?)
   (exp2 expression?))
  (if-exp 
   (exp1 expression?) 
   (exp2 expression?) 
   (exp3 expression?))
  ; (let-exp
  ;  (var identifier?)
  ;  (val expression?)
  ;  (body expression?))
  ; (proc-exp
  ;   (var identifier?)
  ;   (body expression?))
  ; (call-exp
  ;   (rator expression?)
  ;   (rand expression?))
  ; (letrec-exp
  ;   (listof_proc_name (list-of identifier?))
  ;   (listof_bound_vars (list-of identifier?))
  ;   (listof_proc_body (list-of expression?))
  ;   (letrec_body expression?))
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
  (assign-exp 
   (assign_var identifier?) 
   (assign_exp expression?))
  (newarray-exp
   (exp1 expression?)
   (exp2 expression?))
  (arrayref-exp
   (ident identifier?)
   (exp1 expression?))
  (arrayset-exp
   (ident identifier?)
   (exp1 expression?)
   (exp2 expression?))
)

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
  
  ; (sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))
  
  ; (eopl:pretty-print (show-the-datatypes))
)