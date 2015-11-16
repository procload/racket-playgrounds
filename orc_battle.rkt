;; Nat Nat -> Nat
;;; a random number between 1 and the (quotient x y)
(define (random-quotient x y)
  (define div (quotient x y))
  (if (> 0 div) 0 (random+ (add1 div))))

;; Nat -> Nat
;; (random+ n) creates a random number in [1,n]
(define (random+ n)
  (add1 (random n)))

;; Nat -> Nat
;; (random+ n) creates a random number in [-n,-1]
(define (random- n)
  (- (add1 (random n))))

;; Nat Nat [Nat] -> Nat
;; subtract n from m but stay in [0,max-value]
(define (interval- n m (max-value 100))
  (min (max 0 (- n m)) max-value))

;; Nat Nat [Nat] -> Nat
;; subtract n from m but stay in [0,max-value]
(define (interval+ n m (max-value 100))
  (interval- n (- m) max-value))


(struct orc-world (player lom attack# target) #:mutable #:transparent)

(struct player (health agility strength) #:mutable #:transparent)

(define MAX-HEALTH 35)
(define MAX-AGILITY 35)
(define MAX-STRENGTH 35)

(define DAMAGE 2)

(define MONSTER-HEALTH0 10)
(define CLUB-STRENGTH 10)


(struct monster ([health #:mutable #:transparent]))

(struct orc monster (club))
(struct hydra monster ())
(struct slime monster (sliminess))
(struct brigand monster ())

(orc MONSTER-HEALTH0 (add1 (random CLUB-STRENGTH)))

(define my-orc (orc MONSTER-HEALTH0 2))
(define my-slime (slime MONSTER-HEALTH0 10))

(slime-sliminess my-slime)
(monster-health my-orc)
(orc-club my-orc)

(define (stab-orc an-orc)
  (set-monster-health! an-orc (- (monster-health an-orc) DAMAGE)))

(stab-orc my-orc)
(monster-health my-orc)

(define player1 (player 1 2 3))
(set-player-health! player1 33)

(define player2 (player 10 20 30))
(set-player-health! player2 66)

(define player3 player1)
(set-player-agility! player3 666)

(define players (list player1 player2 player3))

(eq? (first players) (third players))

(set-player-strength! (second players) 999)

(define (player-update! setter selector mx)
  (lambda (player delta)
    setter player (interval+ (selector player) delta mx)))

(define player-health+
  (player-update! set-player-health! player-health MAX-HEALTH))

(define player-strength+
  (player-update! set-player-strength! player-strength MAX-STRENGTH))

(define player-agility+
  (player-update! set-player-agility! player-agility MAX-AGILITY))

(define p (player 1 2 3))

(player-strength+ p 10)

(define lom (list (orc 9 3) (orc 9 4) (orc 9 1)))
(list-ref lom 0)

(define ow1 (orc-world 'some-player lom 2 0))
(orc-club (list-ref (orc-world-lom ow1) 2))

(define (initialize-orc-world)
  (define player0 (initialize-player))
  (define lom0 (initialize-monsters))
  (orc-world player0 lom0 (random-number-of-attacks player0) 0))

(define (end-of-orc-battle? w)
  (or (win? w) (lose? w)))

(define (render-orc-battle w)
  (render-orc-world w (orc-world-target w) (instructions w)))

(define (render-the-end w)
  (render-orc-world w #f (message (if (lose? w) LOSE WIN))))

(define (player-acts-on-monsters w k)
  (cond
    [(zero? (orc-world-attack# w)) (void)]
    [(key=? "s" k) (stab w)]
    [(key=? "h" k) (heal w)]
    [(key=? "f" k) (flail w)]
    [(key=? "e" k) (end-turn w)]
    [(key=? "n" k) (initialize-orc-world)]
    [(key=? "right" k) (move-target w +1)]
    [(key=? "left" k) (move-target w -1)]
    [(key=? "down" k) (move-target w (+ PER-ROW))]
    [(key=? "up" k) (move-target w (- PER-ROW))])
  (give-monster-turn-if-attack#=0 w)
  w)

(define (start)
  (big-bang (initialize-orc-world)
            (on-key player-acts-on-monsters)
            (to-draw render-orc-battle)
            (stop-when end-of-orc-battle? render-the-end)))

