(define (foo x)
  (define bar (lambda () y))
  (define y x)
  bar)

(define captured (foo (list 1)))
(display (captured))
(newline)
