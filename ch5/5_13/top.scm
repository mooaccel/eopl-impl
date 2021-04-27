(module top (lib "eopl.ss" "eopl")
  
  ;; top level module.  Loads all required pieces.
  ;; Run the test suite with (run-all).

  (require "drscheme-init.scm")
  (require "data-structures.scm")  ; for expval constructors
  (require "lang.scm")             ; for scan&parse
  (require "interp.scm")           ; for value-of-program
  (require "tests.scm")            ; for test-list
  (require "environments.scm")     ; for empty-env
  
  ;; since this is the top-level module, we don't really need to
  ;; provide anything, but we do so just in case.  

  (provide run run-all)
  
  ;;; interface for book test ;;;
  (provide test-all)
  (define (test-all) (run-all))

  ;;;;;;;;;;;;;;;; interface to test harness ;;;;;;;;;;;;;;;;
  
  ;; run : String -> ExpVal
  (define run
    (lambda (string)
      (value-of-program (scan&parse string))))
  
  ;; run-all : () -> Unspecified

  ;; runs all the tests in test-list, comparing the results with
  ;; equal-answer?  

  (define run-all
    (lambda ()
      (run-tests! run equal-answer? test-list)))
  
  (define equal-answer?
    (lambda (ans correct-ans)
      (equal? ans (sloppy->expval correct-ans))))
  
  (define sloppy->expval 
    (lambda (sloppy-val)
      (cond
        ((number? sloppy-val) (num-val sloppy-val))
        ((boolean? sloppy-val) (bool-val sloppy-val))
        (else
         (eopl:error 'sloppy->expval 
                     "Can't convert sloppy value to expval: ~s"
                     sloppy-val)))))
    
  ;; run-one : Sym -> ExpVal

  ;; (run-one sym) runs the test whose name is sym
  
  (define run-one
    (lambda (test-name)
      (let ((the-test (assoc test-name test-list)))
        (cond
          ((assoc test-name test-list)
           => (lambda (test)
                (run (cadr test))))
          (else (eopl:error 'run-one "no such test: ~s" test-name))))))

   ;; ===================
  (instrument_value_of_k #t)
  (instrument_apply_cont #t)
 
  ; let+proc不能递归, 所以得用letrec
  (eopl:pretty-print (run
  "
  letrec fact(x) = if zero?(x)
                   then 1
                   else *(x, (fact -(x, 1)))
  in (fact 4)
  "
  ))

  ; (eopl:pretty-print (run
  ; " 
  ; letrec factiteracc(n, acc) = 
  ;                  if zero?(n) 
  ;                  then acc
  ;                  else (factiteracc -(n, 1) *(acc, n))
  ; in let factiter = proc(n) (factiteracc n 1)
  ;    in (factiter 4)
  ; " 
  ; ))

  )