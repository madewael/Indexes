#lang racket


(define (log2 n)
  (/ (log n)
     (log 2)))

(struct node (value left right) #:transparent)

(define (make-leaf value)
  (node value #f #f))

(define (leaf? x)
  (and (node? x)
       (not (node-left x))
       (not (node-right x))))

(define make-inner-node node)

(define (∆ a b)(abs (- a b)))
(define (make-tree from to)
  (if (<= (∆ from to) 1)
      (make-leaf from)
      (let ((halve (/ (-(∆ from to)1) 2)))
        (make-inner-node (+ from halve)
                         (make-tree from (+ from halve))
                         (make-tree (- to halve) to)))))

(define T (make-tree 0 15))

(define (pre-order tree)
  (if tree
      (append (list (node-value tree))
              (pre-order (node-left tree))
              (pre-order (node-right tree)))
      '()))

(define (post-order tree)
  (if tree
      (append  (post-order (node-left tree))
               (post-order (node-right tree))
               (list (node-value tree)))
      '()))

(define (in-order tree)
  (if tree
      (append (in-order (node-left tree))
              (list (node-value tree))
              (in-order (node-right tree)))
      '()))

(pre-order  T)
(in-order   T)
(post-order T)



