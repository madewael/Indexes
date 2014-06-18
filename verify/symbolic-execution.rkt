#lang racket

(provide reduce? :* :+ :∆ :quotient :modulo :IDX)

(define reduce? (make-parameter #t))

(define (:* a b)
  (cond
    ((and (reduce?) (number? a)(number? b) (* a b)))
    ((and (reduce?) (number? a)(= a 1))    b)
    ((and (reduce?) (number? b)(= b 1))    a)
    ((and (reduce?) (pair? b)(eq? (car b) '*)) `(* ,a ,@(cdr b)))
    (else `(* ,a ,b))))

(define (:+ a b)
  (cond
    ((and (reduce?) (number? a)(number? b) (+ a b)))
    ((and (reduce?) (number? a)(= a 0))    b)
    ((and (reduce?) (number? b)(= b 0))    a)
    (else `(+ ,a ,b))))

(define (:∆ a b)
  (cond
    ((and (reduce?) (number? a)(number? b) (abs (- a b))))
    ((and (reduce?) (number? a)(= a 0))    b)
    ((and (reduce?) (number? b)(= b 0))    a)
    (else `(∆ ,a ,b))))

(define (:quotient a b)
  (cond
    ((and (reduce?) (number? a)(number? b) (quotient a b)))
    ((and (reduce?) (number? b)(= b 1))    a)
    ((and (reduce?)(number? b)(pair? a)(eq? (car a) 'quotient)(number? (caddr a)))`(quotient ,(cadr a) ,(* b (caddr a))))
    (else `(quotient ,a ,b))))

(define (:modulo a b)
  (cond
    ((and (reduce?) (number? a)(number? b) (modulo a b)))
    ((and (reduce?) (number? b)(= b 1))    a)
    (else `(modulo ,a ,b))))

(define :IDX      'IDX)