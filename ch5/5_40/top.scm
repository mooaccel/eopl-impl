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
 
   ;(eopl:pretty-print (run-one
   ;   'simple-letrec-1))
   ;(run-all)
   ;(run-one 'simple-let-1)

   ;(eopl:pretty-print (run
   ;   "let2 x = 5
   ;        y = 10
   ;    in -(y, x)"
   ;))

   ;(eopl:pretty-print (run
   ;   "let2 x = 5
   ;        y = 10
   ;    in -(y, x)"
   ;))

   ; ; cons测试
   ; (eopl:pretty-print (run
   ;    "let a = cons(1000, 200) 
   ;     in a"
   ; ))
   ; (eopl:pretty-print (run
   ;    "let a = cons(cons(1000, 200),300)
   ;     in a"
   ; ))
   ; (eopl:pretty-print (run
   ;    "let a = emptylist
   ;     in a"
   ; ))
   ; (eopl:pretty-print (run
   ;    "let a = cons(1000, emptylist)
   ;     in null?(a)"
   ; ))
   ; (eopl:pretty-print (run
   ;    "let a = emptylist
   ;     in null?(a)"
   ; ))
   ; (eopl:pretty-print (run
   ;    "let a = cons(1000, 200) 
   ;     in -(car(a), cdr(a))"
   ; ))
   ; (eopl:pretty-print (run
   ;    "let a = cons(cons(1000, 200),300)
   ;     in -(car(car(a)), cdr(a))"
   ; ))
   ; (eopl:pretty-print (run
   ;    "let a = list(100, 200, 300)
   ;     in a"
   ; ))
   ; (eopl:pretty-print (run
   ;    "let a = list(100, 200, 300)
   ;     in -(car(a), car(cdr(a)))"
   ; ))
   ; (eopl:pretty-print (run
   ;    "let a = list(100, 200, 300)
   ;     in -(car(a), car(cdr(cdr(a))))"
   ; ))

   ; (eopl:pretty-print (run
   ; "
   ; try raise 3
   ;     catch (x cont) (cont 300)
   ; "
   ; ))
   ; (eopl:pretty-print (run
   ; "
   ; try raise 3
   ;     catch (x cont) (cont 300)
   ; "
   ; ))

   ;(eopl:pretty-print (run
   ;"try try -(3, raise 5)
   ;     catch (x cont) raise 7
   ; catch (y cont) y"
   ;))
   (eopl:pretty-print (run
   "try try -(3, raise 5)
        catch (x c) (c raise 7)
      catch (y c) (c y)"
   ))

   (eopl:pretty-print (run
   "-(try try -(3, raise 5)
        catch (x c) (c raise 7)
      catch (y c) (c y), 100)"
   ))

  
  )
