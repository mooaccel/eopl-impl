(module top (lib "eopl")
  
  ;; top level module.  Loads all required pieces.
  ;; Run the test suite with (run-all).

  (require "drscheme-init.scm")
  (require "data-structures.scm")  ; for expval constructors
  (require "lang.scm")             ; for scan&parse
  (require "interp.scm")           ; for value-of-program
  (require "tests.scm")            ; for test-list
  
;  (provide run run-all)
;  
;  ;;; interface for book test ;;;
;  (provide test-all)
;  (define (test-all) 
;    (run-all))
;
  (define run
    (lambda (timeslice string)
      (value-of-program timeslice (scan&parse string))))
  
  (define run-all
    (lambda (timeslice)
      (run-tests! 
        (lambda (string) (run timeslice string))
        equal-answer? test-list)))
  
  (define run-one
    (lambda (timeslice test-name)
      (let ((the-test (assoc test-name test-list)))
        (cond
          ((assoc test-name test-list)
           => (lambda (test)
                (run timeslice (cadr test))))
          (else (eopl:error 'run-one "no such test: ~s" test-name))))))
 
  (define equal-answer?
    (lambda (ans correct-ans)
      (equal? ans (sloppy->expval correct-ans))))
  
  ; 转化成内部的值
  (define sloppy->expval 
    (lambda (sloppy-val)
      (cond
        ((number? sloppy-val) (num-val sloppy-val))
        ((boolean? sloppy-val) (bool-val sloppy-val))
        ((list? sloppy-val) (list-val (map sloppy->expval 
                                           sloppy-val)))  ; ?
        (else
         (eopl:error 'sloppy->expval 
                     "Can't convert sloppy value to expval: ~s"
                     sloppy-val)))))
 
  ;(instrument_let #t)
  ;(instrument_newref #t)

  ;(instrument_value_of_k #t)
  ;(instrument_apply_cont #t)

  ;(eopl:pretty-print (run-one 
  ;  'gensym-test
  ;))
  ;(eopl:pretty-print (run-one 
  ;  'ch4_5_1_example_01
  ;))
  ;(eopl:pretty-print (run-one 
  ;  'ch4_5_1_example_02
  ;))

  ; ==========

  (instrument_ready_queue_size #f)
  ;(eopl:pretty-print (run-one 
  ;  3 ; 5, 20, 50 可以都试试
  ;  'two-non-cooperating-threads-01
  ;))
  (run-all 5)


  ;(eopl:pretty-print (run
  ;  20
  ;  "
  ;  let buffer = 0
  ;  in let producer = proc (n)
  ;                      letrec wait(k) = if zero?(k) 
  ;                                       then set buffer = n 
  ;                                       else begin 
  ;                                              print(-(k, -200)); 
  ;                                              (wait -(k, 1)) 
  ;                                            end
  ;                      in (wait 5) 
  ;     in let consumer = proc (d)
  ;                        letrec busywait (k) = if zero?(buffer)
  ;                                              then begin 
  ;                                                    print(-(k, -100)); 
  ;                                                    (busywait -(k, -1)) 
  ;                                                   end 
  ;                                              else buffer
  ;                        in (busywait 0)
  ;        in begin
  ;            spawn(proc (d) (producer 44));
  ;            print(1000);
  ;            (consumer 86)
  ;            end
  ;  "
  ;))

)