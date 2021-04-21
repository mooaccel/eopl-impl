(module tests mzscheme
  
  (provide test-list)
  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
  
  (define test-list
    '(
      (apply-simple-proc 
        "let f = proc (x) -(x,1) in (f 30)" 
        29)

      (letmutable-test-case01
        "
        let b = 100
            c = 200
        in letmutable a = 300
           in let f = proc (x)
                       begin
                         set a = x;
                         -(a, c)
                       end
              in (f 20)
        "
        -180)

      (proc-multi-arg
        "
        let f = proc (x y) -(x, y)
        in (f 3 -4)
        "
        7)

    ))
)

  ; (eopl:pretty-print 
  ;   (run "let f = proc (x) 
  ;                   proc (y) 
  ;                     begin 
  ;                       set x = -(x,-1); 
  ;                       -(x,y) 
  ;                     end
  ;         in ((f 44) 33)")
  ; )

  ; (run-one 'let-multiarg)

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