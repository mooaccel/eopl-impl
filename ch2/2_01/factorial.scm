(load "./ch2/2_01/2_01.scm")

;(translate-to-scheme-number '(10))
;(translate-to-scheme-number '(1))

(define (aux cur_n cur_res)
  (let ((cur_num_in_scheme (translate-to-scheme-number cur_n)))
    ;(newline)
    ;(display "========")
    ;(display cur_num_in_scheme)))
    (if (< cur_num_in_scheme 2)
        cur_res
        (aux (predecessor cur_n) (* cur_res cur_num_in_scheme)))))

;(trace aux)

(define (factorial n)
    (aux n 1))

(factorial '(3))
(factorial '(10))