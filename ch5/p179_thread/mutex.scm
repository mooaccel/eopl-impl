(module mutex (lib "eopl")

  (require "store.scm")                    ; for store ops
  (require "data-structures.scm")          ; for lock, a-lock
  (require "scheduler.scm")                ; for os calls
  (require "queue.scm")

  (provide (all-defined-out))

  ;; implements binary semaphores (mutexes).

  (define instrument-mutexes (make-parameter #f))

  ;; new-mutex () -> Mutex
  ;; Page: 188
  (define new-mutex
    (lambda ()
      (a-mutex
        (newref #f)                  
        (newref (empty-queue)))))                 

  ; #f open, 可以获取
  ; #t close, 可以释放

  (define wait-for-mutex
    (lambda (m th)
      (cases mutex m
        (a-mutex (ref_to_closed? ref_to_wait_queue)
          (cond ((deref ref_to_closed?) ; 目前的是关闭状态 
                    (setref! ref_to_wait_queue
                             (enqueue (deref ref_to_wait_queue) th))  ; 注意th从blocked到ready queue之后不需要重新检测ref_to_closed, 因为signal如果发现还有blocked在mutex上的, 不会设置ref_to_closed
                    (run-next-thread))
                (else  ; 可以获取
                    (setref! ref_to_closed? #t)
                    (th)))))))

  (define signal-mutex
    (lambda (m th)
      (cases mutex m
        (a-mutex (ref_to_closed? ref_to_wait_queue)
          (let ((is_closed (deref ref_to_closed?))
                (wait_queue (deref ref_to_wait_queue)))
            (when is_closed  ; 可以释放
              (if (empty? wait_queue)
                  (setref! ref_to_closed? #f) ; 知道最后wait_queue没有thread的时候才设置为#f
                  (dequeue wait_queue  ; 还有其他的需要运行, 保持is_closed为close
                    (lambda (first_waiting_th other_waiting_ths)
                      (place-on-ready-queue!
                        first_waiting_th)  ; 放入ready queue
                      (setref!
                        ref_to_wait_queue
                        other_waiting_ths)))))
            (th))))))  ; 没有获取过, 直接接下来的处理就行, 忽略signal(exp).

)