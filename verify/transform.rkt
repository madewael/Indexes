#lang racket

(require "../orders.rkt" "../matrix.rkt" "verify-lang.rkt" "iterations.rkt"
         "symbolic-execution.rkt")

(provide transform)

(define def-mm '(let ((C (make-matrix (matrix-rows A) (matrix-cols B) 0)))
                  (for ((i (matrix-rows C)))
                    (for ((j (matrix-cols C)))
                      (for ((k (matrix-cols A)))
                        (matrix-set! C i j (+ (matrix-get C i j)
                                              (* (matrix-get A i k)
                                                 (matrix-get B k j)))))))
                  C))

(define def-mm2 '(define (matrix-multiply A B)
                   (let ((C (make-matrix (matrix-rows A) (matrix-cols B) 0)))
                     (for ((i (matrix-rows C)))
                       (for ((j (matrix-cols C)))
                         (let ((sum 0))
                           (for ((k (matrix-cols A)))
                             (set! sum (+ sum (* (matrix-get A i k)
                                                 (matrix-get B k j)))))
                           (matrix-set! C i j sum))))
                     C)))

(define sum-λ '(lambda (M)
                 (let ((s 0))
                   (for ((i (matrix-cols M)))
                     (set! s (+ s  (matrix-get M 0 i)))))))

(define iterate-λ '(lambda (M)
                     (let ((s 0))
                       (for ((i (matrix-cols M)))
                         (matrix-get M 0 i)
                         (matrix-get M-p 0 i)))))



(define seq-iteration-var (let ((i -1))
                            (thunk (set! i (+ i 1))
                                   (string->symbol (string-append "seq-"
                                                                  (number->string i))))))
(define (transform-sequence exps)
  (let ((l (length exps)))
    (if (= l 1)
        (transform (car exps))
        (let ((var-name (seq-iteration-var)))
          (list 'for
                (list (list var-name l))
                (let loop ((exps exps)(i 0))
                  (if (null? exps)
                      '(error "out of bounds")
                      (let ((first (car exps)))
                        (list 'if (list '= var-name i)
                              (transform first)
                              (loop (cdr exps) (+ i 1)))))))))))



(define (transform-for exp)
  (list 'for (for-exp->clauses exp)
        (transform-sequence (for-exp->body exp))))


(define (transform-let exp)
  (list 'let
        (let-exp->var-val-pairs exp)
        (transform-sequence (let-exp->body exp))))

(define (transform exp)
  (cond
    ((let? exp)(transform-let exp))
    ((for? exp)(transform-for exp))
    (else exp)))



;(transform '(for ((x 10)) (for ((y 20)) (matrix-get A x y)(matrix-get B x y))))