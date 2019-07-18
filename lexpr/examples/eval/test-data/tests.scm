(define (foo x)
  (define bar (lambda () y))
  (define y x)
  bar)

(define captured (foo 1))
(display (captured))
(newline)

(define (x y) (y x))
(define (y x) (x y))
(x y)
