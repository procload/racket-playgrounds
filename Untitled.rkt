#lang racket
(require 2htdp/universe 2htdp/image)

(define WIDTH 400)
(define HEIGHT 200)

(define (add-3-to-current-state current-state)
  (+ (current-state 3))
  