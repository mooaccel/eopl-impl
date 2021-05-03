(module store (lib "eopl")
  
  (require "drscheme-init.scm")
   
  (provide initialize-store! 
           reference? 
           newref 
           deref 
           setref!

           instrument_newref 
           get-store-as-list)
  
  (define instrument_newref (make-parameter #f))
  
  ;;;;;;;;;;;;;;;; references and the store ;;;;;;;;;;;;;;;;
  
  ;;; world's dumbest model of the store:  the store is a list and a
  ;;; reference is number which denotes a position in the list.

  ;; the-store: a Scheme variable containing the current state of the
  ;; store.  Initially set to a dummy variable.
  (define the_store 'uninitialized)

  ;; empty-store : () -> Sto
  ;; Page: 111
  (define empty-store
    (lambda () '()))
  
  ;; initialize-store! : () -> Sto
  ;; usage: (initialize-store!) sets the-store to the empty-store
  ;; Page 111
  (define initialize-store!
    (lambda ()
      (set! the_store (empty-store))))

  ;; get-store : () -> Sto
  ;; Page: 111
  ;; This is obsolete.  Replaced by get-store-as-list below (更加可读)
  (define (get-store)
    the_store)

  ;; reference? : SchemeVal -> Bool  最简单的引用是这样而已
  ;; Page: 111
  (define (reference? v)
      (integer? v))

  ;; newref : ExpVal -> Ref
  ;; Page: 111
  ; val是expval
  (define (newref val)
      (let ((next_ref (length the_store)))
        (set! the_store
              (append the_store (list val)))  ; 把val放置到最后面
        (when (instrument_newref)
              (eopl:printf 
                  "newref: allocating location ~s with initial contents ~s~%"
                  next_ref val))
        next_ref))

  ;; deref : Ref -> ExpVal
  ;; Page 111
  (define deref 
    (lambda (ref)
      (list-ref the_store ref)))

  ;; setref! : Ref * ExpVal -> Unspecified
  ;; Page: 112
  (define setref!                       
    (lambda (ref val)
      (set! the_store
        (letrec
          ((setref-inner
             ;; returns a list like store1, except that position ref1
             ;; contains val. 
             (lambda (store1 ref1)
               (cond
                 ((null? store1)
                  (report-invalid-reference ref the_store))
                 ((zero? ref1)
                  (cons val (cdr store1)))
                 (else
                   (cons
                     (car store1)
                     (setref-inner
                       (cdr store1) (- ref1 1))))))))
          (setref-inner the_store ref)))))

  (define report-invalid-reference
    (lambda (ref the_store)
      (eopl:error 'setref
        "illegal reference ~s in store ~s"
        ref the_store)))

  ;; get-store-as-list : () -> Listof(List(Ref,Expval))
  ;; Exports the current state of the store as a scheme list.
  ;; (get-store-as-list '(foo bar baz)) = ((0 foo)(1 bar) (2 baz))
  ;;   where foo, bar, and baz are expvals.
  ;; If the store were represented in a different way, this would be
  ;; replaced by something cleverer.
  ;; Replaces get-store (p. 111)
   (define (get-store-as-list)
      (letrec ((inner-loop (lambda (sto n)               ;; convert sto to list as if its car was location n
                              (if (null? sto)
                                '()
                                (cons
                                  (list n (car sto))
                                  (inner-loop (cdr sto) (+ n 1)))))))
         (inner-loop the_store 0)))
  ; 相当于内部define函数, 然后递归调用...

  )