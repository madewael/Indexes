#lang racket

(provide row-major-order col-major-order S-order Z-order /-order hilbert-order spiral-order )


#| AUX |#
(define (power-of-two? x)
  (if (= x 1)
      #t
      (and (= 0 (modulo x 2))
           (power-of-two? (quotient x 2)))))

(define (∆-number n)(/ (* n (+ n 1)) 2))

(define (∆ a b)(abs (- a b)))

#| ORDERS |#
(define (row-major-order ROWS COLS)
  (λ (r c)(+ (* COLS r) c)))

(define (col-major-order ROWS COLS)
  (λ (r c)(+ r (* ROWS c))))

(define (S-order ROWS COLS)
  (λ (r c)(+ (* r COLS)
             (if (odd? r)(- COLS 1 c) c))))

(define (/-order ROWS COLS)
  (λ (r c)
    (let ((sum (+ r c))
          (min (min ROWS COLS))
          (max (max ROWS COLS)))
      (cond
        ((< sum min)(+ (∆-number (+ r c))c))
        ((>= sum max) (+ (∆-number min)
                         (* (- min 1) (- sum min))
                         c -1
                         (- (∆-number(- sum max)))))
        (else (+ (∆-number min)(* (- min 1) (- sum min)) c -1))))))



(define (Z-order ROWS COLS)
  (when (not (= ROWS COLS)) (error 'Z-order "Expecting square matrix, given ~a x ~a." ROWS COLS))
  (when (not (power-of-two? ROWS))(error 'Z-order "Expecting power-of-two matrix, given ~a x ~a." ROWS COLS))
  (λ (r c)
    (let loop ((r r)
               (c c))
      (if (and (= 0 r)(= 0 c))
          0
          (+ (modulo c 2)
             (* 2 (modulo r 2))
             (* 4 (loop (quotient r 2)
                        (quotient c  2))))))))



(define (hilbert-order rs cs)
  (define ords '((N #(#(0 3)
                      #(1 2))
                    #(#(W E)
                      #(N N)))
                 (W #(#(0 1)
                      #(3 2))
                    #(#(N W)
                      #(S W)))
                 (S #(#(2 1)
                      #(3 0))
                    #(#(S S)
                      #(W E)))
                 (E #(#(2 3)
                      #(1 0))
                    #(#(E N)
                      #(E S)))))
  
  (when (not (= rs cs)) (error 'hilbert-order "expects square matrix, given ~a x ~a" rs cs))
  (λ (r c)
    (let loop ((s (/ rs 2))
               (r r)
               (c c)
               (o 'N))
      (let* ((res (assoc o ords))
             (idx (vector-ref (vector-ref (cadr res) (quotient r s)) (quotient c s)))
             (o (vector-ref (vector-ref (caddr res) (quotient r s)) (quotient c s))))
        (if (= s 1)
            idx
            (+ (* idx s s)
               (loop (/ s 2)(modulo r s)(modulo c s) o)))))))




(define (spiral-order rs cs)
  
  
  (define (orde r c)
    (min (∆ (- rs 1) r) r
         (∆ (- cs 1) c) c))
  
  (define (spiral-length orde)
    (* 2 orde (+ rs cs (* -2 orde))))
  
  (define (spiral-index orde r c)
    (let ((r-o (- r orde))
          (c-o (- c orde))
          (rs-o (- rs orde orde))
          (cs-o (- rs orde orde)))
      
      (cond ((= r-o 0) c-o)
            ((= c-o (- cs-o 1))
             (+ cs-o r-o -1))
            ((= r (- rs orde 1))
             (+ cs-o cs-o rs-o (- c-o) -3))
            (else 
             (+ cs-o cs-o rs-o rs-o  (- r-o) -4)))))
  
  (λ (r c)
    (let ((orde (orde r c)))
      (+ (spiral-length orde)(spiral-index orde r c)))))