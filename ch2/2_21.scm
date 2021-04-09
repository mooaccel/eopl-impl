#lang eopl
; Implement the data type of environments, as in section 2.2.2, using define-datatype. 
; Then include has-binding? of exercise 2.9.

; Env-exp ::= (empty-env) 
;         ::= (extend-env Identifier Scheme-value Env-exp)

(define (identifier? var)
  (symbol? var))
; todo
(define (scheme-val? var)
  #t)

(define-datatype env-exp env-exp?
  (empty-env)
  (extend-env
    (ident identifier?)
    (scheme_val scheme-val?)
    (env_exp env-exp?)))

(eopl:pretty-print (empty-env))
(eopl:pretty-print (extend-env 'a 
                               10
                               (empty-env)))
(define test_env_01 (extend-env 'b 
                                20
                                (extend-env 'a 
                                            10
                                            (empty-env))))

(define test_env_02 (extend-env 'd 
                                6 
                                (extend-env 'y 
                                            8 
                                            (extend-env 'x 
                                                        7 
                                                        (extend-env 'y 
                                                                    14 
                                                                    (empty-env))))))
(define (apply-env env s)
  (cases env-exp env
    (empty-env ()
      (eopl:error 'apply-env "~s not found" s))
      ;(error "not find " s))
    (extend-env (ident scheme_val env_exp)
      (if (eqv? ident s)
          scheme_val
          (apply-env env_exp s)))))

(define (has-binding? env s)
  (cases env-exp env
    (empty-env ()
      #f)
    (extend-env (ident scheme_val env_exp)
      (if (eqv? ident s)
          #t
          (has-binding? env_exp s)))))

(display (equal? (has-binding? test_env_01 'mm) #f))
(display (equal? (has-binding? test_env_01 'b) #t))
(display (equal? (has-binding? test_env_01 'a) #t))
(newline)

(display (equal? (apply-env test_env_02 'd) 6))
(display (equal? (apply-env test_env_02 'y) 8))
(display (equal? (apply-env test_env_02 'x) 7))
;(display (apply-env test_env_02 'mm))
(newline)