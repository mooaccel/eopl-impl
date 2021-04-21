(module top (lib "eopl")
  
  ;; top level module.  Loads all required pieces.
  ;; Run the test suite with (run-all).

  (require "drscheme-init.scm")
  (require "data-structures.scm")  ; for expval constructors
  (require "lang.scm")             ; for scan&parse
  (require "interp.scm")           ; for value-of-program
  ; (require "tests.scm")            ; for test-list
  
;  (provide run run-all)
;  
;  ;;; interface for book test ;;;
;  (provide test-all)
;  (define (test-all) 
;    (run-all))
;
;  ;; run-all : () -> Unspecified
;  ;; runs all the tests in test-list, comparing the results with
;  ;; equal-answer?  
;  (define run-all
;    (lambda ()
;      (run-tests! run equal-answer? test-list)))  ; run-tests在drscheme-init.scm
;
;   ;;;;;;;;;;;;;;;; interface to test harness ;;;;;;;;;;;;;;;;
   ;; run : String -> ExpVal
   (define run
     (lambda (string)
       (value-of-program (scan&parse string))))
 
;   (define equal-answer?
;     (lambda (ans correct-ans)
;       (equal? ans (sloppy->expval correct-ans))))
;   
;   (define sloppy->expval 
;     (lambda (sloppy-val)
;       (cond
;         ((number? sloppy-val) (num-val sloppy-val))
;         ((boolean? sloppy-val) (bool-val sloppy-val))
;         (else
;          (eopl:error 'sloppy->expval 
;                      "Can't convert sloppy value to expval: ~s"
;                      sloppy-val)))))
    
;  ;; run-one : Sym -> ExpVal
;  ;; (run-one sym) runs the test whose name is sym
;  (define run-one
;    (lambda (test-name)
;      (let ((the-test (assoc test-name test-list)))
;        (cond
;          ((assoc test-name test-list)
;           => (lambda (test)
;                (run (cadr test))))
;          (else (eopl:error 'run-one "no such test: ~s" test-name))))))
 
  ; (eopl:pretty-print (run-one 'positive-const))
  ; (run-all)
  ; (if (instrument_newref #t)
  ;     (eopl:pretty-print "t")
  ;     (eopl:pretty-print "f"))

  (instrument_let #t)
  (instrument_newref #t)

  ; (eopl:pretty-print 
  ;   (run "let f = proc (x) 
  ;                   proc (y) 
  ;                     begin 
  ;                       set x = -(x,-1); 
  ;                       -(x,y) 
  ;                     end
  ;         in ((f 44) 33)")
  ; )
  (eopl:pretty-print (run
  "
  let a = 100
      b = 200
      c = 300
  in let f = proc (x)
              begin
                set a = x;
                -(a, c)
              end
     in (f 20)
  "
  ))


  ; (eopl:pretty-print (run
  ; "
  ; let f = proc (x, y) +(x,y)
  ; in (f 3 4)
  ; "
  ; ))
  ; (eopl:pretty-print (run
  ; "
  ; letrec even(x) = if zero?(x) then 1 else (odd -(x,1)) 
  ;        odd(x) = if zero?(x) then 0 else (even -(x,1)) 
  ; in (odd 13)
  ; "
  ; ))

)





; 注意: 目前这个例子store只扩充, 不回收...
; implicit ref语言在explicit refs的基础上改成了所有的var-exp都是ref, 
; 然后注意: 
; ExpVal = Int + Bool + Proc 
; DenVal = Ref(ExpVal)