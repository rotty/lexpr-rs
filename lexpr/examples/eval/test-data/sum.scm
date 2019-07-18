(define (sum n acc)
  (if (< 0 n)
      (sum (- n 1) (+ acc n))
      acc))

(define (main argv)
  (display (sum 1000000 0))
  (newline))

(main (quote ()))
