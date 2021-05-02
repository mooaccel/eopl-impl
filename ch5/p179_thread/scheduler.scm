(module lang (lib "eopl")

  (require "queue.scm")

  (define the_ready_queue 'uninitialized)
  (define the_final_answer 'uninitialized)
  (define the_max_time_slice 'uninitialized)
  (define the_time_remaining 'uninitialized)

  (define instrument_ready_queue_size (make-parameter #f))

  (provide
    initialize-scheduler!
    place-on-ready-queue!
    run-next-thread
    set-final-answer!
    time-expired?
    decrement-timer!

    instrument_ready_queue_size
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
      (begin
        (set! the_ready_queue (enqueue the_ready_queue th))
      	(when (instrument_ready_queue_size)
          (eopl:printf "after place-on-ready-queue!, ready queue size = ~s~%" (queue-size-scheduler)))
        )))
  
  (define (queue-size-scheduler)
    (queue-size the_ready_queue))

  ;; run-next-thread : () -> FinalAnswer
  (define run-next-thread
    (lambda ()
      (begin 
        (when (instrument_ready_queue_size)
          (eopl:printf "enter run-next-thread, ready queue size = ~s~%" (queue-size-scheduler)))
        (if (empty? the_ready_queue)
            the_final_answer
            (dequeue the_ready_queue
                     (lambda (first_ready_thread other_ready_threads)
                        (set! the_ready_queue other_ready_threads)            
                        (set! the_time_remaining the_max_time_slice) 
                        (first_ready_thread))))
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