(module lang (lib "eopl.ss" "eopl")                

  ;; grammar for the LETREC language

  (require "drscheme-init.scm")
  
  (provide (all-defined-out))

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
  
  (define the-grammar
    '((program (expression) a-program)

      (expression (number) const-exp)

      (expression (identifier) var-exp)

      (expression
        ("-" "(" expression "," expression ")")
        diff-exp)
      (expression
        ("*" "(" expression "," expression ")")
        multiplication-exp)
      
      (expression
       ("zero?" "(" expression ")")
       zero?-exp)

      (expression
       ("if" expression "then" expression "else" expression)
       if-exp)

      ;(expression
      ; ("let" identifier "=" expression "in" expression)
      ; let-exp)   

      (expression ("let" (arbno identifier "=" expression) "in" expression) let-exp)

      ; (expression
      ;  ("proc" "(" identifier ")" expression)
      ;  proc-exp)

      ; (expression
      ;  ("(" expression expression ")")
      ;  call-exp)
      (expression ("proc" "(" (separated-list identifier ",") ")" expression) proc-exp)  ; proc也可以没有参数
      (expression ("(" expression (arbno expression) ")") call-exp)  ; (f)可以没有参数, 只有rator

      (expression
        ("letrec"
          identifier "(" (separated-list identifier ",") ")" "=" expression
          "in" expression)
        letrec-exp)

      (expression ("cons" "(" expression "," expression ")") cons-exp)
      (expression ("car" "(" expression ")") car-exp)
      (expression ("cdr" "(" expression ")") cdr-exp)
      (expression ("emptylist") emptylist-exp)
      (expression ("null?" "(" expression ")") null?-exp)

      (expression ("list" "(" (separated-list expression ",") ")" ) list-exp)
      
      ))
  
  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
  
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))
  
  )
