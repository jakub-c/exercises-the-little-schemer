#lang racket/base

(provide atom?)

(define atom?
  (λ(x)
    (and (not (pair? x))
         (not (null? x)))))
