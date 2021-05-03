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

  ;(eopl:pretty-print (run-one 
  ;  5
  ;  'producer-consumer-01
  ;))

  ; ; 加上yield之后可以固定输出1,6,2,7,3,8..., 不受timeslice影响
  ; (eopl:pretty-print (run-one 
  ; 10;  20, 50
  ; 'two-non-threads-with-yield-01
  ; ))

)