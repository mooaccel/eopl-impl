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

  ; (eopl:pretty-print (run
  ; "
  ;   let p = proc (x) set x = 4
  ;   in let a = 3
  ;      in begin 
  ;           (p a); 
  ;           a 
  ;         end
  ; "
  ; ))

  ; (eopl:pretty-print (run
  ;   "
  ;   let f = proc (x) set x = 44
  ;     in let g = proc (y) (f y)
  ;        in let z = 55
  ;           in begin (g z); z end
  ;   "
  ; ))

  ;(eopl:pretty-print (run
  ;  "
  ;    let swap = proc (x) proc (y) 
  ;                          let temp = x 
  ;                          in begin
  ;                              set x = y;
  ;                              set y = temp 
  ;                             end            
  ;    in let a = 33            
  ;       in let b = 44            
  ;          in begin            
  ;              ((swap a) b); 
  ;              -(a,b) 
  ;             end
  ;  "))

  ; 或者这样写, 效果是一样的, 参考exer3.20
  ; (eopl:pretty-print (run
  ;   "
  ;     let swap = proc (x y)
  ;                   let temp = x 
  ;                   in begin
  ;                       set x = y;
  ;                       set y = temp 
  ;                      end            
  ;     in let a = 33            
  ;            b = 44            
  ;        in begin            
  ;             (swap a b); 
  ;             -(a,b) 
  ;           end
  ;   "))

  ; (eopl:pretty-print (run
  ;   "
  ;   let b = 3
  ;   in let p = proc (x) proc(y) 
  ;                         begin
  ;                           set x = 4;
  ;                           y
  ;                         end 
  ;      in ((p b) b)
  ;   "
  ; ))

)