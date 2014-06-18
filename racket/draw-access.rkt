#lang racket
(require racket/draw
         mrlib/gif
         "orders.rkt"
         (except-in "register-access.rkt" build-matrix make-matrix)
         (prefix-in  : (only-in "register-access.rkt" build-matrix make-matrix)))

(provide reset-drawing!
         as-animated-gif
         make-matrix build-matrix
         matrix-size matrix-cols matrix-rows
         matrix-get matrix-set!
         matrix-map!
         matrix-vector
         matrix-change-layout!
         matrix->string)

(define CELL-SIZE 10)
(define SPACING   5)

(define (draw-cell dc color offset-x offset-y r c)
  (send dc set-pen "gray" (/ SPACING 3) 'solid)
  (send dc set-brush color 'solid)
  (send dc draw-rectangle
        (+ offset-x (* c CELL-SIZE))
        (+ offset-y (* r CELL-SIZE))
        CELL-SIZE
        CELL-SIZE))

(define (make-basic-bitmap rows cols)
  (let* ((target (make-bitmap (+ SPACING (* cols CELL-SIZE) SPACING)
                              (+ SPACING (* rows CELL-SIZE) SPACING)))
         (dc (new bitmap-dc% [bitmap target])))
    (for* ((r rows) (c cols))
      (draw-cell dc "white" SPACING SPACING r c))
    target))

(struct matrix-entry (M color basic-2D-bitmap basic-1D-bitmap))

(define (make-matrix-entry M)
  (matrix-entry M
                (get-color)
                (make-basic-bitmap (matrix-rows M)
                                   (matrix-cols M))
                (make-basic-bitmap 1
                                   (matrix-size M))))

(define (init-colors)
  (define (name->color color-name)
    (send the-color-database find-color color-name))
  (map name->color '("green" "yellow" "red" "blue")))

(define colors (init-colors))

(define (get-color)
  (if (null? colors)
      (error 'register "Cant register new matrix, add more colors")
      (let ((color (car colors)))
        (set! colors (cdr colors))
        color)))

(define matrixes '())

(define (reset-drawing!)
  (set! matrixes '())
  (set! colors (init-colors))
  (reset-accesses!))

(define (register M)
  (set! matrixes (cons (cons M (make-matrix-entry M)) matrixes)))

(define (draw-2D-accesses-next-to-eachother access-entries)
  (let* ((basic-2D-bitmaps (map (compose matrix-entry-basic-2D-bitmap cdr) matrixes))
         (width (apply +   (map (λ (bm)(send bm get-width))  basic-2D-bitmaps)))
         (height (apply max (map (λ (bm)(send bm get-height)) basic-2D-bitmaps)))
         (target (make-bitmap width height))
         (dc (new bitmap-dc% [bitmap target])))
    
    (let ((m-es (map cdr matrixes)))
      (for/fold ((x-offset 0))
        ((M               (map matrix-entry-M m-es))
         (color           (map matrix-entry-color m-es))
         (basic-2D-bitmap (map matrix-entry-basic-2D-bitmap m-es)))
        (define (filter-pred a-e) (eq? (access-entry-M a-e) M))
        
        (send dc draw-bitmap basic-2D-bitmap x-offset 0)
        
        (for ((a-e (filter filter-pred access-entries)))
          (draw-cell dc color (+ x-offset SPACING) SPACING
                     (access-entry-r a-e)
                     (access-entry-c a-e)))
        (+ x-offset (send basic-2D-bitmap get-width))))
    target))

(define (draw-accesses access-entries)
  (let* ((basic-2D-bitmaps (map (compose matrix-entry-basic-2D-bitmap cdr) matrixes))
         (basic-1D-bitmaps (map (compose matrix-entry-basic-1D-bitmap cdr) matrixes))
         
         (2D-width (apply max (map (λ (bm)(send bm get-width))  basic-2D-bitmaps)))
         (1D-width (apply max (map (λ (bm)(send bm get-width))  basic-1D-bitmaps)))
         (width  (+ 2D-width 1D-width))
         (height (apply + (map (λ (bm)(send bm get-height)) basic-2D-bitmaps)))
         
         (target (make-bitmap width height))
         (dc (new bitmap-dc% [bitmap target])))
    
    (let ((m-es (map cdr matrixes)))
      (for/fold ((y-offset 0))
        ((M               (map matrix-entry-M m-es))
         (color           (map matrix-entry-color m-es))
         (basic-2D-bitmap (map matrix-entry-basic-2D-bitmap m-es))
         (basic-1D-bitmap (map matrix-entry-basic-1D-bitmap m-es)))
        (define (filter-pred a-e) (eq? (access-entry-M a-e) M))
        (define orders (map (λ (order)(order (matrix-rows M)(matrix-cols M)))
                            (list row-major-order col-major-order Z-order hilbert-order)))
        ; draw 2D access
        (send dc draw-bitmap basic-2D-bitmap 0 y-offset)
        ;draw all 1Ds
        (for/fold ((y-offset y-offset)) ((order orders))
          (send dc draw-bitmap basic-1D-bitmap 2D-width y-offset)
          (+ y-offset SPACING CELL-SIZE))
        
        (for ((a-e (filter filter-pred access-entries)))
          (let ((r (access-entry-r a-e))
                (c (access-entry-c a-e)))
            ; draw 2D access
            (draw-cell dc color SPACING  (+ y-offset SPACING) r c)
            
            ;draw all 1Ds access
            (for/fold ((y-offset y-offset))
              ((order orders))
              (draw-cell dc color (+ 2D-width SPACING) (+ y-offset SPACING)  0 (order r c))
              (+ y-offset SPACING CELL-SIZE))))
        (+ y-offset (send basic-2D-bitmap get-height))))
    target))

(define (make-matrix rows cols v)
  (let ((M (:make-matrix rows cols v)))
    (register M)
    M))

(define (build-matrix rows cols proc)
  (let ((M (:build-matrix rows cols proc)))
    (register M)
    M))

(define (as-animated-gif delay-csec filename)
  (define history '())
  (foldl (λ (n r)
           (if (>= (length r) 3)
               (begin (set! history (cons (draw-accesses (cons n r)) history))
                      '())
               (cons n r)))
         '()
         (get-accesses))
  (write-animated-gif history delay-csec filename))


(reset-drawing!)
