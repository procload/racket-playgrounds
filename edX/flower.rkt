;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname flower) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Grows a flower where a user clicks a mouse

;; =================
;; Constants:

(define WIDTH 600)
(define HEIGHT 400)

(define FLOWER (scale .5
       (overlay
        (star-polygon 17 17 4 "solid" "DarkViolet")
        (star-polygon 21 21 4 "solid" "VioletRed")
        (star-polygon 23 23 3 "solid" "HotPink"))))

(define SCALE .01)

(define SPEED 1)

(define MTS (empty-scene WIDTH HEIGHT))

;; =================
;; Data definitions:

;; Flower is a Natural, Natural, Number
;; interp. (make-flower x y size) is a flower with x coordinate x and y coordinate y

(define-struct flower (x y size))

(define F1 (make-flower 0 10 1)) ;at 0 x and 10 y and 1 scale size
(define F2 (make-flower 100 100 1.5)) ;at 100 x and 100 y and 1.5 scale size

(define (fn-for-flower f)
  (... (flower-x f)      ;; Natural
       (flower-y f)      ;; Natural
       (flower-size f))) ;; Number

;; =================
;; Functions:

;; Flower -> Flower
;; start the world with (main (make-flower x y size))

;; stub is not required for main

(define (main f)
  (big-bang f                             ; Flower
            (on-tick   grow-flower SPEED) ; Flower -> Flower
            (to-draw   render-flower)     ; Flower -> Image
            (on-mouse  handle-mouse)))    ; Flower Integer Integer MouseEvent -> Flower

;; Flower -> Flower
;; produce the next size of the flower at the specified scale and speed

;; (define (grow-flower f) f) ; stub

(check-expect (grow-flower F1) (make-flower 0 10 (+ (flower-size F1) SCALE)))
(check-expect (grow-flower F2) (make-flower 100 100 (+ (flower-size F2) SCALE)))

;; template taken from Flower

(define (grow-flower f)
  (make-flower (flower-x f)
               (flower-y f)
               (+ SCALE (flower-size f))))


;; Flower -> Image
;; render the flower at the appropriate x and y coordinates and scale 
;; !!!

;; (define (render-flower f) MTS) ; stub

(check-expect (render-flower (make-flower 100 100 1))
              (place-image
               (scale 1 FLOWER)
               100 100 MTS))


(check-expect (render-flower (make-flower 100 100 1.5))
              (place-image
               (scale 1.5 FLOWER)
               100 100 MTS))

;; template taken from Flower

(define (render-flower f)
  (place-image
   (scale (flower-size f) FLOWER)
   (flower-x f) (flower-y f) MTS))


;; Flower Integer Integer MouseEvent -> Flower
;; places the flower at the mouse's x and y position

;; (define (handle-mouse f x y me) f) ; stub

(check-expect (handle-mouse (make-flower 100 100 1) 200 200 "button-down")
              (make-flower 200 200 1))

(check-expect (handle-mouse (make-flower 200 300 1.5) 300 200 "button-down")
              (make-flower 300 200 1))

(check-expect (handle-mouse (make-flower 200 300 1.5) 300 200 "move")
              (make-flower 200 300 1.5))


(define (handle-mouse f x y me)
  (cond [(mouse=? me "button-down") (make-flower x y 1)]
        [else
         (make-flower (flower-x f) (flower-y f) (flower-size f))]))
