#lang racket

(require "../orders.rkt" "../matrix.rkt" "verify-lang.rkt" "iterations.rkt"
         "symbolic-execution.rkt")
(define N 8)


(define def-a '(define A (build-matrix N N (λ (r c)(if (= r c) 1 0)))))
(define def-b '(define B (build-matrix N N (λ (r c)(if (= r c) 1 0)))))

(define def-single-step '(matrix-set! C 3 4 (+ (matrix-get C 3 4)
                                               (* (matrix-get A 3 5)
                                                  (matrix-get B 5 4)))))

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



(define make-matrix?  (curry tagged-list? 'make-matrix))
(define build-matrix? (curry tagged-list? 'build-matrix))
(define matrix-set!?  (curry tagged-list? 'matrix-set!))
(define matrix-get?   (curry tagged-list? 'matrix-get))

(define matrix-access-target cadr)
(define (matrix-access-keys exp)(list (caddr exp)(cadddr exp)))
(define matrix-set!-exp (compose cadddr cdr))

(define matrix-rows?  (curry tagged-list? 'matrix-rows))
(define matrix-cols?  (curry tagged-list? 'matrix-cols))



(define seq-iteration-var (let ((i -1))
                            (thunk (set! i (+ i 1))
                                   (string->symbol (string-append "seq-"
                                                                  (number->string i))))))

(define (analyze-iterations-sequence exps idx iterations)
  (let ((analysisses (map (λ (exp)(analyze-iterations exp idx iterations)) exps)))
    (let ((total-size (foldl :+ 0 (map analysis-size analysisses))))
      (let ((analysisses (map (λ (exp)(analyze-iterations exp (:quotient idx total-size)  iterations)) exps)))
        (make-analysis total-size analysisses)))))


(define (OLD-analyze-iterations-sequence exps iterations)
  (apply append (map (curryr analyze-iterations iterations) exps)))

(define (for-clause->iteration clause)
  (define (get-shorthand iterable type)
    (string->symbol (string-append (symbol->string (cadr iterable))
                                   "-"
                                   (symbol->string type))))
  
  (let ((var        (for-clause->var  clause))
        (iterable (for-clause->iteratable clause)))
    (cond
      ((matrix-rows? iterable)(iteration var 0 (get-shorthand iterable 'rows) 1))
      ((matrix-cols? iterable)(iteration var 0 (get-shorthand iterable 'cols) 1))
      (else (iteration var 0 iterable 1)))))

(struct analysis (size analysisses) #:transparent )

(define (make-analysis size analysisses)
  (when (not (list? analysisses))
    (error 'make-analysis "expects list, given ~a." analysisses))
  (if (= (length analysisses) 1)
      (let* ((first (car analysisses))
             (first-size (analysis-size first)))
        (if (= size first-size)
            first
            (analysis size analysisses)))
      (analysis size analysisses)))



(define (analyze-iterations-for exp idx iterations)
  (let ((clauses (for-exp->clauses exp)))
    (when (not (= (length clauses) 1))
      (error 'analyze-iterations-for "Only for-exps with 1 clause allouwed"))
    (let ((iterations (append iterations (list (for-clause->iteration (car clauses))))))
      (analyze-iterations-sequence (for-exp->body exp) idx iterations))))

(struct access (target access f) #:transparent)

(define (analyze-iterations-access exp idx iterations)
  (let ((target (matrix-access-target exp))
        (keys (matrix-access-keys exp)))
    (analysis 1
              (list (access target keys
                            (iteration-space->f idx iterations))))))

(define (analyze-iterations-matrix-set exp idx iterations)
  (let ((new-value-analysis (analyze-iterations (matrix-set!-exp exp) idx iterations))
        (access-analysis (analyze-iterations-access exp
                                                    idx
                                                    iterations)))
    (make-analysis (:+ (analysis-size new-value-analysis)
                       (analysis-size access-analysis))
                   (list new-value-analysis
                         access-analysis))))

(define (analyze-iterations-application exp idx iterations)
  ;currently the body of an application is ignored!
  (analyze-iterations-sequence (cdr exp) idx iterations))

(define (analyze-iterations exp idx iterations)
  (cond
    ((let? exp)         (analyze-iterations-sequence (let-exp->body exp) idx iterations))
    ((for? exp)         (analyze-iterations-for exp idx iterations))  
    ((matrix-get? exp)  (analyze-iterations-access exp idx iterations))
    ((matrix-set!? exp) (analyze-iterations-matrix-set exp idx iterations))
    ((application? exp) (analyze-iterations-application exp idx iterations))
    (else               (analysis 0 exp))))

;(analyze-iterations def-single-step :IDX '())

;(analyze-iterations def-mm :IDX '())


;(analyze-iterations '(for ((x 10))(matrix-get A x x)(matrix-get B x x)) :IDX '())

(analyze-iterations '(for ((x 10))
                       (for ((y 10))
                         (matrix-get A x y)
                         (matrix-get B y x))) :IDX '())

;(analyze-iterations def-mm2 :IDX '())


