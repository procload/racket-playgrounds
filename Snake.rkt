#lang racket
(require 2htdp/universe 2htdp/image)

;; -----------------------------------------------------------------------------
;; Constants

;; Tick Rate
(define TICK-RATE 1/10)

;; Board Size Constants
(define SIZE 30)

;; Snake Constants
(define SEG-SIZE 15)

;; Goo Constants
(define MAX-GOO (add1 (random 10)))
(define EXPIRATION-TIME 150)

;; GRAPHICAL BOARD
(define WIDTH-PX  (* SEG-SIZE 30))
(define HEIGHT-PX (* SEG-SIZE 30))

;; Visual constants
(define MT-SCENE (empty-scene WIDTH-PX HEIGHT-PX))
(define GOO-IMG (bitmap "graphics/goo.gif"))
(define GOO-SPECIAL-IMG (bitmap "graphics/goo_special.gif"))
(define SEG-IMG  (bitmap "graphics/body.gif"))
(define HEAD-IMG (bitmap "graphics/head.gif"))

(define HEAD-LEFT-IMG HEAD-IMG)
(define HEAD-DOWN-IMG (rotate 90 HEAD-LEFT-IMG))
(define HEAD-RIGHT-IMG (flip-horizontal HEAD-LEFT-IMG))
(define HEAD-UP-IMG (flip-vertical HEAD-DOWN-IMG))

(define ENDGAME-TEXT-SIZE 15)


(struct pit (snake goos))

(struct snake (dir segs))

(struct posn (x y))

(struct goo (loc expire type))

;; Randomly creates new goo structures
(define (fresh-goo)
  (goo (posn (add1 (random (sub1 SIZE)))
             (add1 (random (sub1 SIZE))))
       EXPIRATION-TIME
       "normal"))

;; Creates a random length list of goos
(define goo-list (build-list MAX-GOO (lambda (x) (fresh-goo))))

;; Main game function
(define (start-snake)
  (big-bang (pit (snake "right" (list (posn 1 1)))
                 goo-list)
            (on-tick next-pit TICK-RATE)
            (on-key direct-snake)
            (to-draw render-pit)
            (stop-when dead? render-end)))

(define (next-pit w)
  (define snake (pit-snake w))
  (define goos (pit-goos w))
  (define goo-to-eat (can-eat snake goos))
  (if goo-to-eat
      (pit (grow snake) (age-goo (eat goos goo-to-eat)))
      (pit (slither snake) (age-goo goos))))

(define (can-eat snake goos)
  (define head (snake-head snake))
  ;; Takes a goo structure and returns it if the snake's head is close
  (ormap (lambda (g) (and (close? head g) g)) goos))

;; Does the same segment have the same posn as a goo
(define (close? s g)
  (posn=? s (goo-loc g)))

;; Removes a goo, and places a new fresh goo to the front of the list
(define (eat goos goo-to-eat)
  (cons (fresh-goo) (remove goo-to-eat goos)))

;; Creates a new snake that is one segment longer than the given
(define (grow sn)
  (snake (snake-dir sn)
         (cons (next-head sn) (snake-segs sn))))

;; Creates a new snake by adding a new head and removing last segment of snake
(define (slither sn)
  (snake (snake-dir sn)
         (cons (next-head sn) (all-but-last (snake-segs sn)))))

;; Returns a given list of segments minus the last one
(define (all-but-last segs)
  (cond [(empty? (rest segs)) empty]
        [else (cons (first segs) (all-but-last (rest segs)))]))

(define (next-head sn)
  (define head (snake-head sn))
  (define dir (snake-dir sn))
  (cond [(string=? dir "up") (posn-move head 0 -1)]
        [(string=? dir "down") (posn-move head 0 1)]
        [(string=? dir "left") (posn-move head -1 0)]
        [(string=? dir "right") (posn-move head 1 0)]))

(define (posn-move p dx dy)
  (posn (+ (posn-x p) dx)
        (+ (posn-y p) dy)))

(define (age-goo goos)
  (rot (renew goos)))

(define (rot goos)
  (map decay goos))

;; Takes a good and decreases the expire field by one
;; then returns a goo in the same location with one less on expire
(define (decay g)
  (goo (goo-loc g) (sub1 (goo-expire g)) (goo-type g)))

;; If a goo is rotten, create a fresh one and add it to the list
(define (renew goos)
  (cond [(empty? goos) empty]
        [(rotten? (first goos))
         (cons (fresh-goo) (renew (rest goos)))]
        [else
         (cons (first goos) (renew (rest goos)))]))


;; Checks if the expiration is zero, if it is then return #t
(define (rotten? g)
  (zero? (goo-expire g)))


(define (direct-snake w ke)
  (cond [(dir? ke) (world-change-dir w ke)]
        [else w]))

(define (dir? x)
  (or (key=? x "up")
      (key=? x "down")
      (key=? x "left")
      (key=? x "right")))

(define (world-change-dir w d)
  ;; Creates a new snake from the world passed in
  (define the-snake (pit-snake w))
  ;; Checks if the snake is going the same direction as the one passed in
  ;; and if the snake has at least one segment
  (cond [(and (opposite-dir? (snake-dir the-snake) d)
              ;; consists of the head and at least one segment
              (cons? (rest (snake-segs the-snake))))
         (stop-with w)]
        [else
         (pit (snake-change-dir the-snake d) (pit-goos w))]))

(define (opposite-dir? d1 d2)
  (cond [(string=? d1 "up") (string=? d2 "down")]
        [(string=? d1 "down") (string=? d2 "up")]
        [(string=? d1 "left") (string=? d2 "right")]
        [(string=? d1 "right") (string=? d2 "left")]))

(define (render-pit w)
  (snake+scene (pit-snake w)
               (goo-list+scene (pit-goos w) MT-SCENE)))

(define (snake+scene snake scene)
  (define snake-body-scene
    (img-list+scene (snake-body snake) SEG-IMG scene))
  (define dir (snake-dir snake))
  (img+scene (snake-head snake)
             (cond [(string=? "up" dir) HEAD-UP-IMG]
                   [(string=? "down" dir) HEAD-DOWN-IMG]
                   [(string=? "left" dir) HEAD-LEFT-IMG]
                   [(string=? "right" dir) HEAD-RIGHT-IMG])
             snake-body-scene))

;; Returns a scene
;; Draws the image to each posn in the list
(define (img-list+scene posns img scene)
  (foldr (lambda (p s) (img+scene p img s)) scene posns))

;; Returns a scene
;; Draws the given image onto the scene at the posn.
(define (img+scene posn img scene)
  (place-image img
               (* (posn-x posn) SEG-SIZE)
               (* (posn-y posn) SEG-SIZE)
               scene))

;; Draws all the goo to a scene
(define (goo-list+scene goos scene)
  ;; Takes a list of goo and returns a list of posns
  ;; > (get-posns-from-goo (list (goo (posn 2 2) 1) (goo (posn 3 3) 1))
  ;; (list (posn 2 2) (posn 3 3))
  (define (get-posns-from-goo goos)
    (map goo-loc goos))
  (img-list+scene (get-posns-from-goo goos) GOO-IMG scene))

(define (dead? w)
  (define snake (pit-snake w))
  (or (self-colliding? snake) (wall-colliding? snake)))

(define (render-end w)
  (overlay (text (number->string (length (snake-body (pit-snake w)))) ENDGAME-TEXT-SIZE "black")
           (render-pit w)))

(define (self-colliding? snake)
  (cons? (member (snake-head snake) (snake-body snake))))

(define (wall-colliding? snake)
  (define x (posn-x (snake-head snake)))
  (define y (posn-y (snake-head snake)))
  (or (= 0 x) (= x SIZE)
      (= 0 y) (= y SIZE)))

;; Checks if two posns have the same x and y coordinates
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

(define (snake-head sn)
  (first (snake-segs sn)))

(define (snake-body sn)
  (rest (snake-segs sn)))

(define (snake-tail sn)
  (last (snake-segs sn)))

(define (snake-change-dir sn d)
  (snake d (snake-segs sn)))
