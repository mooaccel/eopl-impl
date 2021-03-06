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

   ; ; cons??????
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

  ; (instrument_value_of_k #t)
  ; (instrument_apply_cont #t)

  ; -12
  ;(eopl:pretty-print (run 
  ;"
  ;let x = -3
  ;    y = 4
  ;    z = -5
  ;in -(x, -(y, z))
  ;"
  ;))
  ;  ; -20
  ;  (eopl:pretty-print (run 
  ;  "
  ;  let q = 30
  ;      w = 40
  ;      e = 50
  ;      r = 60
  ;  in -(-(q, r), -(w, e))
  ;  "
  ;  ))

  ; (eopl:pretty-print (run
  ; "
  ; let f = proc (x, y) -(x, y)
  ; in (f 3 4)
  ; "
  ; ))
  ;(test-all)
   ; ????????????
   ;(eopl:pretty-print 
   ; (run
   ;   " 
   ;     *(11,12)
   ;   "
   ; ))

   ; ???cont????????????
   ; (eopl:pretty-print 
   ;  (run
   ;    " 
   ;    letrec
   ;        fact(x) = if zero?(x) 
   ;                  then 1 
   ;                  else *(x, (fact -(x, 1)))
   ;     in (fact 6)" 
   ;  ))

  ; letrec???????????????
;  (eopl:pretty-print (run
;  "
; let m = -5 
; in letrec f(x uu) = if zero?(x) 
;                     then 0 
;                     else -((f -(x,1) uu), uu) 
;    in (f 4 -10)
;  "
;  ))




;(define fact
;  (lambda (n)
;    (fact/k n (lambda (val) 
;                val))))
;
;(define fact/k
;  (lambda (n cont) 
;    (if (zero? n)
;        (cont 1)
;        (fact/k (- n 1) (lambda (val) 
;                          (cont (* n val)))))))
;
;(eopl:pretty-print (fact 6))

; ?????????????????????, proc-val...
;  (eopl:pretty-print (run
;  "
; let f = proc (g)
;          -((g 10), 10)
; in (f proc (x) 21)
;  "
;  ))

  (eopl:pretty-print (run
    " 
    letrec factk(n cont) = if zero?(n) 
                           then (cont 1)
                           else (factk -(n, 1)
                                         proc (val) 
                                            (cont *(n, val)))
    in (factk 5 proc (val) 
                  val)
  "
  ))

  )