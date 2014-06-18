#lang racket

(require "orders.rkt" "matrix.rkt")
(define N 16)





(define (matrix-multiply A B)
  (let ((C (make-matrix (matrix-rows A)
                        (matrix-cols B)
                        0)))
    (for ((i (in-range (matrix-rows A))))
      (for ((j (in-range (matrix-cols B))))
        (for ((k (in-range (matrix-cols A))))
          (matrix-set! C i j (+ (matrix-get C i j)
                                (* (matrix-get A i k)
                                   (matrix-get B k j)))))))
    C))

(define (matrix-check C)
  (for ((i (matrix-rows C)))
    (for ((j (matrix-cols C)))
      (when (not (= (matrix-get C i j)
                    (if (= i j) 1 0)))
        (error "err in MM")))))

(define (row-row)
  (let* ((A (build-matrix N N (λ (r c)(if (= r c) 1.0 0.0))))
         (B (build-matrix N N (λ (r c)(if (= r c) 1.0 0.0)))))
    (matrix-check (time (matrix-multiply A B)))))

(define (row-col)
  (let* ((A (build-matrix N N (λ (r c)(if (= r c) 1.0 0.0))))
         (B (build-matrix N N (λ (r c)(if (= r c) 1.0 0.0))))
         (C (void)))
    (time (begin (matrix-change-layout! B col-major-order)
                 (set! C (matrix-multiply A B))
                 (matrix-change-layout! B row-major-order)))
     (matrix-check C)))

(define (col-row)
  (let* ((A (build-matrix N N (λ (r c)(if (= r c) 1.0 0.0))))
         (B (build-matrix N N (λ (r c)(if (= r c) 1.0 0.0))))
         (C (void)))
    (time (begin (matrix-change-layout! A col-major-order)
                 (set! C (matrix-multiply A B))
                 (matrix-change-layout! A row-major-order)))
     (matrix-check C)))

(display "row-row\n")
(row-row)
(newline)

(display "row-col\n")
(row-col)
(newline)

(display "col-row\n")
(col-row)
(newline)

