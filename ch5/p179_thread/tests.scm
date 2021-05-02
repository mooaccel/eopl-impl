(module tests mzscheme
  
  (provide test-list)
  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
  
  (define test-list
    '(
      (proc-test-cases-01
      "let f = proc (x) -(x, 1) 
       in (f 33)"
      32)

      (zero?-test-01 "zero?(10)" #f)
      (zero?-test-02 "zero?(0)" #t)
  
      ;; simple arithmetic
      (positive-const "11" 11)
      (negative-const "-33" -33)
      (simple-arith-1 "-(44,33)" 11)
  
      ;; nested arithmetic
      (nested-arith-left "-(-(44,33),22)" -11)
      (nested-arith-right "-(55, -(22,11))" 44)
  
      ;; simple variables
      (test-var-1 "x" 10)
      (test-var-2 "-(x,1)" 9)
      (test-var-3 "-(1,x)" -9)
      
      ;; simple unbound variables
      (test-unbound-var-1 "foo" error)
      (test-unbound-var-2 "-(x,foo)" error)
  
      ;; simple conditionals
      (if-true "if zero?(0) then 3 else 4" 3)
      (if-false "if zero?(1) then 3 else 4" 4)
      
      ;; test dynamic typechecking
      (no-bool-to-diff-1 "-(zero?(0),1)" error)
      (no-bool-to-diff-2 "-(1,zero?(0))" error)
      (no-int-to-if "if 1 then 2 else 3" error)

      ;; make sure that the test and both arms get evaluated
      ;; properly. 
      (if-eval-test-true "if zero?(-(11,11)) then 3 else 4" 3)
      (if-eval-test-false "if zero?(-(11, 12)) then 3 else 4" 4)
      
      ;; and make sure the other arm doesn't get evaluated.
      (if-eval-test-true-2 "if zero?(-(11, 11)) then 3 else foo" 3)
      (if-eval-test-false-2 "if zero?(-(11,12)) then foo else 4" 4)

      ;; simple let
      (simple-let-1 "let x = 3 in x" 3)

      ;; make sure the body and rhs get evaluated
      (eval-let-body "let x = 3 in -(x,1)" 2)
      (eval-let-rhs "let x = -(4,1) in -(x,1)" 2)

      ;; check nested let and shadowing
      (simple-nested-let "let x = 3 in let y = 4 in -(x,y)" -1)
      (check-shadowing-in-body "let x = 3 in let x = 4 in x" 4)
      (check-shadowing-in-rhs "let x = 3 in let x = -(x,1) in x" 2)

      ;; simple applications
      (apply-proc-in-rator-pos "(proc(x) -(x,1)  30)" 29)
      (apply-simple-proc "let f = proc (x) -(x,1) in (f 30)" 29)
      (let-to-proc-1 "(proc(f)(f 30)  proc(x)-(x,1))" 29)


      (nested-procs "((proc (x) proc (y) -(x,y)  5) 6)" -1)
      (nested-procs2 "let f = proc(x) proc (y) -(x,y) in ((f -(10,5)) 6)"
        -1)
      
       (y-combinator-1 "
let fix =  proc (f)
            let d = proc (x) proc (z) ((f (x x)) z)
            in proc (n) ((f (d d)) n)
in let
    t4m = proc (f) proc(x) if zero?(x) then 0 else -((f -(x,1)),-4)
in let times4 = (fix t4m)
   in (times4 3)" 12)
      
       ;; simple letrecs
      (simple-letrec-1 "letrec f(x) = -(x,1) in (f 33)" 32)
      (simple-letrec-2
        "letrec f(x) = if zero?(x)  then 0 else -((f -(x,1)), -2) in (f 4)"
        8)

      (simple-letrec-3
        "let m = -5 
 in letrec f(x) = if zero?(x) then 0 else -((f -(x,1)), m) in (f 4)"
        20)
      
      (fact-of-6  "letrec
                      fact(x) = if zero?(x) 
                                then 1 
                                else *(x, (fact -(x, 1)))
                   in (fact 6)" 
                  720)
     
      ; 这个测试用例这样也能形成互递归? todo待研究
      (HO-nested-letrecs
"letrec even(odd)  = proc(x) if zero?(x) then 1 else (odd -(x,1))
   in letrec  odd(x)  = if zero?(x) then 0 else ((even odd) -(x,1))
   in (odd 13)" 1)
;
;      
;      (begin-test-1
;        "begin 1; 2; 3 end"
;        3)

      ;; extremely primitive testing for mutable variables

      (assignment-test-0 "let x = 17
                          in set x = 27"
        34)
      ;(assignment-test-1 "let x = 17
      ;                    in begin set x = 27; x end"
      ;  27)

      (ch4_5_1_example_01
      "
        let p = proc (x) set x = 4
        in let a = 3
        in let dummy = (p a)
           in a
      "
      3)

      (ch4_5_1_example_02
      "
        let p = proc (x) set x = 4
        in let a = 3
        in let dummy = (p a)
           in dummy
      "
      34)

      (gensym-test
      "let g = let count = 0 
               in proc(d) 
                    let d = set count = -(count, -1)
                    in count
      in -((g 11), (g 22))"
      -1)

;      (even-odd-via-set "
;let x = 0
;in letrec even(d) = if zero?(x) then 1 
;                                  else let d = set x = -(x,1)
;                                       in (odd d)
;              odd(d)  = if zero?(x) then 0 
;                                  else let d = set x = -(x,1)
;                                       in (even d)
;   in let d = set x = 13 in (odd -99)" 1)
;
;      (example-for-book-1 "
;let f = proc (x) proc (y) 
;                  begin
;                   set x = -(x,-1);
;                   -(x,y)
;                  end
;in ((f 44) 33)"
;	12) 

  (simple-spawn-01 "spawn(proc(d) 3)" 73)
  (simple-spawn-02 "begin spawn(proc(d) 3); 44 end" 44)

  (print-test-cases-01
    "begin 
      spawn(proc(dummy) 3);
      print (30)
     end"
     1)
  (print-test-cases-02
    "begin 
      spawn(proc(dummy) 
                  begin 
                    print (23);
                    31
                  end);
      print (30)
    end
    "
    1)
  ; 目前还不能这么写这种结果的测试用例, todo
  ;(const-list-test-cases-01
  ;  "let l1 = [1, 2, 3, 4, 5, 6]
  ;   in l1"
  ;  (list 1 2 3 4 5 6))
  (const-list-test-cases-02
    "let l1 = [1, 2, 3, 4, 5, 6]
     in car(cdr(cdr(l1)))"
    3)
  ;(const-list-test-cases-03
  ;  "let l1 = [1, 2, 3, 4, 5, 6]
  ;   in cdr(cdr(l1))"
  ;  (list 3 4 5 6))
  (const-list-test-cases-04
    "let l1 = [1, 2]
     in null?(cdr(cdr(l1)))"
    #t)

  ; eopl book p180 example
  (two-non-cooperating-threads-01
  "
  letrec noisy (l) = if null?(l) 
                     then 0 
                     else 
                      begin 
                        print(car(l)); 
                        (noisy cdr(l)) 
                      end 
  in begin
      spawn(proc (dummy) (noisy [1,2,3,4,5]));
      spawn(proc (dummy) (noisy [6,7,8,9,10]));
      print(100); 
      33 
     end
  "
  33)

  ))
      
)