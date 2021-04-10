#lang eopl

(define (identifier? var)
  (symbol? var))
; todo, 可能需要改变, 目前是什么都能放
(define (scheme-val? var)
  #t)

(define-datatype env-exp env-exp?
  (empty-env)
  (extend-env
    (ident identifier?)
    (scheme_val scheme-val?)
    (env_exp env-exp?)))

(define (apply-env env s)
  (cases env-exp env
    (empty-env ()
      (eopl:error 'apply-env "~s not found" s))
    (extend-env (ident scheme_val env_exp)
      (if (eqv? ident s)
          scheme_val
          (apply-env env_exp s)))))

(provide identifier?)
(provide empty-env)
(provide extend-env)
(provide apply-env)