#lang racket

(require "orders.rkt")

(provide make-matrix build-matrix
         matrix-size matrix-cols matrix-rows
         matrix-get matrix-set!
         matrix-map!
         matrix-vector
         matrix-change-layout!
         matrix->string)

(struct matrix ((coord->idx #:mutable) rows cols vector))

(define (make-matrix rows cols v)
  (matrix (row-major-order rows cols) rows cols (make-vector (* rows cols))))

(define (build-matrix rows cols proc)
  (let ((m (make-matrix rows cols #f)))
    (for* ((r (in-range rows))
           (c (in-range cols)))
      (matrix-set! m r c (proc r c)))
    m))

(define (matrix-size m)
  (* (matrix-rows m)
     (matrix-cols m)))

(define (matrix-get m r c)
  (let ((i ((matrix-coord->idx m) r c))
        (vector (matrix-vector m)))
    (vector-ref vector i)))


(define (matrix-set! m r c v)
  (let ((i ((matrix-coord->idx m) r c))
        (vector (matrix-vector m)))
    (vector-set! vector i v)))


(define (matrix-map! m proc)
  (vector-map! proc (matrix-vector m))
  (void))

(define (matrix->string m)
  (apply string-append(for/list ((r (in-range (matrix-rows m))))
                        (string-append (apply string-append
                                              (for/list ((c (in-range (matrix-cols m))))
                                                (format "~a\t" (matrix-get m r c))))
                                       "\n"))))

(define (matrix-change-layout! m order)
  (let ((old-vector (for/vector ((e (matrix-vector m))) e))
        (new-vector (matrix-vector m))
        (old-coord->idx (matrix-coord->idx m))
        (new-coord->idx (order (matrix-rows m)(matrix-cols m))))
    (for* ((r (in-range (matrix-rows m)))
           (c (in-range (matrix-cols m))))
      (let ((old-i (old-coord->idx r c))
            (new-i (new-coord->idx r c)))
        (vector-set! new-vector new-i (vector-ref old-vector old-i))))
    (set-matrix-coord->idx! m new-coord->idx))
  (void))




#|
(define m (build-matrix 10 10 (λ (r c)(+ (* 10 r) c))))
(display (matrix->string m))
(matrix-map! m (curry * -1))
(display (matrix->string m))
(matrix-map! m (curry * -1))
(matrix-change-layout! m /-order)
(display (matrix->string m))
|#



