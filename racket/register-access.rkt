#lang racket

(require (except-in "matrix.rkt" matrix-set! matrix-get)
         (prefix-in  : (only-in "matrix.rkt" matrix-set! matrix-get)))

(provide reset-accesses!
         
         access-entry-M
         access-entry-r
         access-entry-c
         
         get-accesses
         make-matrix build-matrix
         matrix-size matrix-cols matrix-rows
         matrix-get matrix-set!
         matrix-map!
         matrix-vector
         matrix-change-layout!
         matrix->string)

(struct access-entry (M r c type))

(define accesses '())

(define (get-accesses) accesses)

(define (reset-accesses!) (set! accesses '()))

(define (register-access M r c type)
  (set! accesses (cons (access-entry M r c type)
                       accesses)))

(define (matrix-set! M r c v)
  (register-access M r c "!")
  (:matrix-set! M r c v))

(define (matrix-get M r c)
  (register-access M r c "?")
  (:matrix-get M r c))
