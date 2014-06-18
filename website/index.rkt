#lang scribble/manual

@(require "../draw.rkt" "../orders.rkt")

@title{Matrixes and there Indexes}

@image{mm.gif}
@image{bmm.gif}

@(define N 8)

@(draw  N N row-major-order)
@(draw  N N S-order)
@(draw  N N col-major-order)
@(draw  N N Z-order)
@(draw  N N /-order)
@(draw  N N hilbert-order)
@(draw  N N spiral-order)