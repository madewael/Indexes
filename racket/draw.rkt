#lang racket

(require racket/draw "orders.rkt")

(provide draw)

(define F 16)
(define C 1)

(define (ordered-coords rs cs coord->idx)
  (sort (for*/list ((r (in-range rs))
                    (c (in-range cs)))
          (list r c))
        (λ (a b)
          (let ((aa (apply coord->idx a))
                (bb (apply coord->idx b)))
            (when (= aa bb)(error "equal is not allowad" a b))
            (< aa bb)))))

(define (draw rs cs f)
  (define (draw res)
    (define target (make-bitmap (* cs F) (* rs F)))
    (define a-dc (new bitmap-dc% [bitmap target]))
    (define translate (curry map (compose (curry + (/ F 2))(curry * F))))
    (for ((el res))
      (let* ((el (translate el))
             (el-x (cadr el))
             (el-y (car el)))
        (send a-dc draw-ellipse	 (- el-x C)(- el-y C) (+ (* 2 C) 1)(+ (* 2 C) 1))))
    
    (for ((from res)(to (cdr res)))
      (let ((from (translate from))
            (to   (translate to)))
        (let ((f-x (cadr from)) ; !!! x := cadr
              (f-y (car from))  ; !!! y := car
              (t-x (cadr to))
              (t-y (car to)))
          (send a-dc
                draw-line
                f-x f-y
                t-x t-y))))
    target)
  
  (let ((coord->idx (f rs cs)))
    (draw (ordered-coords rs cs coord->idx))))



(define N 8)
(define action draw)

(action N N row-major-order)
(action N N S-order)
(action N N col-major-order)
(action N N Z-order)
(action N N /-order)
(action N N hilbert-order)
(action N N spiral-order)



