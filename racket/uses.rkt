#lang racket
;⇅⇉⇊▤▥

(require "orders.rkt" "matrix.rkt")
(define N 16)

(define ⇉ row-major-order)
(define ⇊ col-major-order)

(define-syntax accesses
  (syntax-rules (as)
    ((accesses ((M as order)...) body ...)
     (begin (matrix-change-layout! M order) ...
            body ...))))






(define A (build-matrix N N (λ (r c)(if (= r c) 1.0 0.0))))
(define B (build-matrix N N (λ (r c)(if (= r c) 1.0 0.0))))

(define (matrix-multiply A B)
  (accesses ((A as ⇉) (B as ⇊))
    (let ((C (make-matrix (matrix-rows A) (matrix-cols B) 0)))
      (accesses ((C as ⇉))
        (for ((i (matrix-rows C)))
          (for ((j (matrix-cols C)))
            (for ((k (matrix-cols A)))
              (matrix-set! C i j (+ (matrix-get C i j)
                                    (* (matrix-get A i k)
                                       (matrix-get B k j))))))))
      C)))