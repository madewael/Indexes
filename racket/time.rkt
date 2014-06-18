#lang racket

(require  "orders.rkt")

(define N 1024)
(for ((order (list row-major-order S-order Z-order /-order hilbert-order spiral-order)))
  (display order)(newline)
  (let ((order (order N N)))
  (time (for* ((r (in-range N))
               (c (in-range N)))
          (order r c)))))

(newline)(newline)

(for ((order (list row-major-order S-order Z-order /-order hilbert-order spiral-order)))
  (display order)(newline)
  (time (for* ((r (in-range N))
               (c (in-range N)))
          ((order N N) r c))))







