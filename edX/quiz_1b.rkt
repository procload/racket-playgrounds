;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname quiz_1b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
;; Image, Image -> Boolean
;; Consumes two images and produces true if the first is larger than the second

(check-expect (larger? (rectangle 10 100 "solid" "red")
                      (rectangle 5 100 "solid" "red"))
                      true)
(check-expect (larger? (circle 5 "solid" "red")
                       (circle 100 "solid" "red"))
                      false)
(check-expect (larger? (circle 100 "solid" "red")
                       (circle 5 "solid" "red"))
                      true)
(check-expect (larger? (triangle 100 "solid" "red")
                       (triangle 100 "solid" "red"))
                      false)

; (define (larger? img1 img2) false) ; stub
; (define (larger? img1 img2) ; template
;  (... img1 img2))

(define (larger? img1 img2)
  (> (* (image-height img1) (image-width img1))
     (* (image-height img2) (image-width img2))))