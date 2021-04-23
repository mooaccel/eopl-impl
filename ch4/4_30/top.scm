(module top (lib "eopl")
  
  ;; top level module.  Loads all required pieces.
  ;; Run the test suite with (run-all).

  (require "drscheme-init.scm")
  (require "data-structures.scm")  ; for expval constructors
  (require "lang.scm")             ; for scan&parse
  (require "interp.scm")           ; for value-of-program
  (require "tests.scm")            ; for test-list
  
   ;; run : String -> ExpVal
   (define run
     (lambda (string)
       (value-of-program (scan&parse string))))
       
;  (provide run run-all)
;  (provide test-all)
  (define (test-all) 
    (run-all))
 
   ;;;;;;;;;;;;;;;; interface to test harness ;;;;;;;;;;;;;;;;
  ;; run-all : () -> Unspecified
  ;; runs all the tests in test-list, comparing the results with equal-answer?  
  (define run-all
    (lambda ()
      (run-tests! run equal-answer? test-list)))  ; run-tests在drscheme-init.scm

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

  ; 控制instrument
   (instrument_let #t)
   (instrument_newref #t)

   ;(-99, -99)
   ;(-99, 0)
   ;(-99, 1)
   ;(-99, 2)
;  (eopl:pretty-print (run
;    "
;    let a = newarray(2, -99)
;        p = proc (x)
;              let v = arrayref(x, 1)
;              in arrayset(x, 1, -(v, -1)) 
;    in begin 
;        arrayset(a, 1, 0); 
;        (p a); 
;        (p a); 
;        arrayref(a, 1)
;       end
;    "
;  ))
  
  (eopl:pretty-print (run
    "
    let a = newarray(8, -99)
    in begin 
        arraylength(a)
       end
    "
  ))

)