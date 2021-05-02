(module lang (lib "eopl")

  (require "queue.scm")

  (define the_ready_queue 'uninitialized)
  (define the_final_answer 'uninitialized)
  (define the_max_time_slice 'uninitialized)
  (define the_time_remaining 'uninitialized)


  (provide
    initialize-scheduler!
    place-on-ready-queue!
    run-next-thread
    set-final-answer!
    time-expired?
    decrement-timer!
  )

  ;; initialize-scheduler! : Int -> Unspecified
  (define initialize-scheduler!
    (lambda (ticks) 
      (set! the_ready_queue (empty-queue)) 
      (set! the_final_answer 'uninitialized) 
      (set! the_max_time_slice ticks) 
      (set! the_time_remaining the_max_time_slice)))

  ;; place-on-ready-queue! : Thread -> Unspecified
  (define place-on-ready-queue!
    (lambda (th)   ; ?
      (set! the_ready_queue (enqueue the_ready_queue th))))

  ;; run-next-thread : () -> FinalAnswer
  (define run-next-thread
    (lambda ()
      (if (empty? the_ready_queue)
          the_final_answer
          (dequeue the_ready_queue
                   (lambda (first_ready_thread other_ready_threads)
                      (set! the_ready_queue other_ready_threads)            
                      (set! the_time_remaining the_max_time_slice) 
                      (first_ready_thread)))
      )))

  ;; set-final-answer! : ExpVal -> Unspecified
  (define set-final-answer!
    (lambda (val)
      (set! the_final_answer val)))

  ;; time-expired? : () -> Bool
  (define time-expired?
    (lambda ()
      (zero? the_time_remaining)))

  ;; decrement-timer! : () -> Unspecified
  (define decrement-timer!
    (lambda ()
      (set! the_time_remaining (- the_time_remaining 1))))

)