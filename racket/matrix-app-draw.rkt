#lang racket

(require "orders.rkt" "draw-access.rkt")

(define N 4)

(define (rec-matrix-multiply A B)
  (let ((C (make-matrix (matrix-rows A)
                        (matrix-cols B)
                        0)))
    (define (∆ a b)(abs (-  a b)))
    (define (matrix-multiply A afr atr afc atc
                             B bfr btr bfc btc)
      (let ((∆Ar (∆ afr atr))
            (∆Bc (∆ bfc btc))
            (∆Ac (∆ afc atc)))
        (cond
          ((and (= ∆Ar 1)
                (= ∆Bc 1)
                (= ∆Ac 1))
           (matrix-set! C afr bfc (+ (matrix-get C afr bfc)
                                     (* (matrix-get A afr afc)
                                        (matrix-get B afc bfc)))))
          
          ((>= ∆Ar (max ∆Ac ∆Bc))
           (let ((amr (/ (+ afr atr) 2)))
             (matrix-multiply A afr amr afc atc
                              B bfr btr bfc btc)
             (matrix-multiply A amr atr afc atc
                              B bfr btr bfc btc)))
          ((>= ∆Bc (max ∆Ar ∆Bc))
           (let ((bmc (/ (+ bfc btc) 2)))
             (matrix-multiply A afr atr afc atc
                              B bfr btr bfc bmc)
             (matrix-multiply A afr atr afc atc
                              B bfr btr bmc btc)))
          (else (let ((amc (/ (+ afc atc) 2))
                      (bmr (/ (+ bfr btr) 2)))
                  (matrix-multiply A afr atr afc amc
                                   B bfr bmr bfc btc)
                  (matrix-multiply A afr atr bmr atc
                                   B bmr btr bfc btc))))))
    
    (matrix-multiply A 0 (matrix-rows A) 0 (matrix-cols A)
                     B 0 (matrix-rows B) 0 (matrix-cols B))
    C))


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

(define (block-matrix-multiply A B s)
  (let ((C (make-matrix (matrix-rows A)
                        (matrix-cols B)
                        0)))
    (for ((b-i (in-range (/ (matrix-rows A) s))))
      (for ((b-j (in-range (/ (matrix-cols B) s))))
        (for ((b-k (in-range  (/ (matrix-cols A) s))))
          
          (for ((i (in-range (* b-i s) (* s(+ b-i 1)))))
            (for ((j (in-range (* b-j s) (* s(+ b-j 1)))))
              (for ((k (in-range (* b-k s) (* s(+ b-k 1)))))
                (matrix-set! C i j (+ (matrix-get C i j)
                                      (* (matrix-get A i k)
                                         (matrix-get B k j))))))))))
    C))


(define (draw-bmm)
  (let* ((A (build-matrix N N (λ (r c)(if (= r c) 1.0 0.0))))
         (B (build-matrix N N (λ (r c)(if (= r c) 1.0 0.0)))))
    (block-matrix-multiply A B 2)))

(define (draw-mm)
  (let* ((A (build-matrix N N (λ (r c)(if (= r c) 1.0 0.0))))
         (B (build-matrix N N (λ (r c)(if (= r c) 1.0 0.0)))))
    (matrix-multiply A B)))

(define (discrete-fourier-transform IN)
  (define (get-real M i)(matrix-get M 0 i))
  (define (set-real! M i v)(matrix-set! M 0 i v))
  
  (define (get-imag M i)(matrix-get M 1 i))
  (define (set-imag! M i v)(matrix-set! M 1 i v))
  
  
  (let* ((length (matrix-cols IN))
         (OUT (make-matrix 2 length 0)))
    (for ((k length))
      (define (inner-loop)
        (for/fold ((sum-real 0) (sum-imag 0))
          ((t length))
          (let* ((angle (/ (* 2 pi t k) length))
                 (cos (cos angle))
                 (sin (sin angle)))
            (values (+ sum-real (* (get-real IN t) cos)
                       (* (get-imag IN t) sin))
                    (+ sum-imag (* (get-real IN t) sin -1)
                       (* (get-imag IN t) cos))))))
      (let-values (((sum-real sum-imag) (inner-loop)))
        (set-real! OUT k sum-real)
        (set-imag! OUT k sum-imag)))))


(define (draw-dft)
  (let ((A (build-matrix 2 N (λ (r c)(random)))))
    (discrete-fourier-transform A)))


;(reset-drawing!)
;(draw-mm)
;(as-animated-gif 1 "mm.gif")