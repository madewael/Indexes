#lang racket
(require "symbolic-execution.rkt")
(provide reduce? iteration iteration-space->f)

(struct iteration (variable start end step))
(struct iteration-space (size fs) #:transparent)



(define (iteration-size iteration)
  (let ((start (iteration-start iteration))
        (end (iteration-end iteration))
        (step (iteration-step iteration)))
    (:quotient (:∆ start end)
               step)))


(define (iteration-space->f idx iterations)
  (iteration-space (foldl :* 1 (map iteration-size iterations))
                   (map (λ (var f)
                          (list var
                                `(λ (IDX) ,f)))
                        (map iteration-variable iterations)
                        (reverse (let loop ((iterations (reverse iterations))
                                            (div 1))
                                   (if (null? iterations)
                                       '()
                                       (let* ((iteration (car iterations))
                                              (start     (iteration-start iteration))
                                              (size      (iteration-size iteration)))
                                         (cons (:+ (:modulo (:quotient :IDX div) size) start)
                                               (loop (cdr iterations)
                                                     (:* div size))))))))))
