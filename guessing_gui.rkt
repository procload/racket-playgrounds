#lang racket
(require 2htdp/universe 2htdp/image)

(struct interval_guess (small big guess))

(define TEXT-SIZE 12)

(define HELP-TEXT
  (text "↑ for larger numbers, ↓ for smaller ones"
        TEXT-SIZE
        "blue"))

(define HELP-TEXT2
  (text "Press = when your number is guessed; q to quit."
        TEXT-SIZE
        "blue"))

(define COLOR "red")
(define WIDTH (+ (image-width HELP-TEXT2) 10))
(define HEIGHT 150)
(define SIZE 72)
(define TEXT-X 3)
(define TEXT-UPPER-Y 10)
(define TEXT-LOWER-Y 135)
(define num_guess 1)

(define MT-SC
  (place-image/align
   HELP-TEXT TEXT-X TEXT-UPPER-Y "left" "top"
   (place-image/align
    HELP-TEXT2 TEXT-X TEXT-LOWER-Y "left" "bottom"
    (empty-scene WIDTH HEIGHT))))

(define (start lower upper)
  (big-bang (interval_guess lower upper num_guess)
            (on-key deal-with-guess)
            (to-draw render)
            (stop-when single? render-last-scene)))

(define (deal-with-guess w key)
  (cond [(key=? key "up") (bigger w)]
        [(key=? key "down") (smaller w)]
        [(key=? key "q") (stop-with w)]
        [(key=? key "=") (stop-with w)]
        [else w]))

(define (smaller w)
  (interval_guess (interval_guess-small w)
            (max (interval_guess-small w) (sub1 (guess w)))
            (add1 (interval_guess-guess w))))

(define (bigger w)
  (interval_guess (interval_guess-small w)
            (min (interval_guess-big w) (add1 (guess w)))
            (add1 (interval_guess-guess w))))

(define (guess w)
  (quotient (+ (interval_guess-small w) (interval_guess-big w)) 2))

(define (render w)
  (overlay/align
   "center" "middle" (text (number->string (guess w)) SIZE COLOR) 
   (overlay/align
     "right" "bottom" (text (number->string (interval_guess-guess w)) 20 "blue")
    MT-SC)))


(define (render-last-scene w)
  (overlay (text "End" SIZE COLOR) MT-SC))

(define (single? w)
  (= (interval_guess-small w) (interval_guess-big w)))

