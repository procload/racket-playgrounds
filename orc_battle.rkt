#lang racket
(require 2htdp/universe 2htdp/image)

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


(struct orc-world (player lom attack# target) #:mutable)

(struct player (health agility strength) #:mutable)

;; -----------------------------------------------------------------------------
;; THE CONSTANTS IN THE WORLD 

;; player attributes 
(define MAX-HEALTH 35)
(define MAX-AGILITY 35)
(define MAX-STRENGTH 35)

;; depending on other player attributes, 
;; the game picks the number of attacks, flailing and stabbing damage 
(define ATTACKS# 4)
(define STAB-DAMAGE 2)
(define FLAIL-DAMAGE 3)
(define HEALING 8)

;; monster attributes 
(define MONSTER# 12)
(define PER-ROW 4)
(unless (zero? (remainder MONSTER# PER-ROW))
  (error 'constraint "PER-ROW must divide MONSTER# evenly into rows"))

(define MONSTER-HEALTH0 9)
(define CLUB-STRENGTH 8)
(define SLIMINESS 5)

(define HEALTH-DAMAGE -2)
(define AGILITY-DAMAGE -3)
(define STRENGTH-DAMAGE -4)

;; string constants 
(define STRENGTH "strength")
(define AGILITY "agility")
(define HEALTH "health")
(define LOSE  "YOU LOSE")
(define WIN "YOU WIN")
(define DEAD "DEAD")
(define REMAINING "Remaining attacks ")
(define INSTRUCTIONS-2 "Select a monster using the arrow keys")
(define INSTRUCTIONS-1
  "Press S to stab a monster | Press F to Flail wildly | Press H to Heal")

;; graphical constants 
(define HEALTH-BAR-HEIGHT 12)
(define HEALTH-BAR-WIDTH  50)

;; compute constants for image frames 
(define ORC     (bitmap "graphics/orc.png"))
(define HYDRA   (bitmap "graphics/hydra.png"))
(define SLIME   (bitmap "graphics/slime.bmp"))
(define BRIGAND (bitmap "graphics/brigand.bmp"))

(define PIC-LIST (list ORC HYDRA SLIME BRIGAND))
(define w (apply max (map image-width PIC-LIST)))
(define h (apply max (map image-height PIC-LIST)))

;; images: player, monsters, constant texts
(define PLAYER-IMAGE  (bitmap "graphics/player.bmp"))

(define FRAME (rectangle w h 'outline 'white))
(define TARGET (circle (- (/ w 2) 2) 'outline 'blue))

(define ORC-IMAGE     (overlay ORC FRAME))
(define HYDRA-IMAGE   (overlay HYDRA FRAME))
(define SLIME-IMAGE   (overlay SLIME FRAME))
(define BRIGAND-IMAGE (overlay BRIGAND FRAME))

(define V-SPACER (rectangle 0 10 "solid" "white"))
(define H-SPACER (rectangle 10 0 "solid" "white"))

;; fonts & texts & colors
(define AGILITY-COLOR "blue")
(define HEALTH-COLOR "crimson")
(define STRENGTH-COLOR "forest green") 
(define MONSTER-COLOR "crimson")
(define MESSAGE-COLOR "black")
(define ATTACK-COLOR "crimson")

(define HEALTH-SIZE (- HEALTH-BAR-HEIGHT 4))
(define DEAD-TEXT-SIZE (- HEALTH-BAR-HEIGHT 2))
(define INSTRUCTION-TEXT-SIZE 16)
(define MESSAGES-SIZE 40)

(define INSTRUCTION-TEXT
  (above 
   (text INSTRUCTIONS-2 (- INSTRUCTION-TEXT-SIZE 2) "blue")
   (text INSTRUCTIONS-1 (- INSTRUCTION-TEXT-SIZE 4) "blue")))

(define DEAD-TEXT (text DEAD DEAD-TEXT-SIZE "crimson"))

(struct monster (image [health #:mutable]))

(struct orc monster (club))
(struct hydra monster ())
(struct slime monster (sliminess))
(struct brigand monster ())

(define (player-update! setter selector mx)
  (lambda (player delta)
    (setter player
            (interval+ (selector player) delta mx))))

(define player-health+
  (player-update! set-player-health! player-health MAX-HEALTH))

(define player-strength+
  (player-update! set-player-strength! player-strength MAX-STRENGTH))

(define player-agility+
  (player-update! set-player-agility! player-agility MAX-AGILITY))

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
    [(zero? (orc-world-attack# w)) w]
    
    [(key=? "s" k) (stab w)]
    [(key=? "h" k) (heal w)]
    [(key=? "f" k) (flail w)]
    
    [(key=? "right" k) (move-target w +1)]
    [(key=? "left" k)  (move-target w -1)]
    [(key=? "down" k)  (move-target w (+ PER-ROW))]
    [(key=? "up" k)    (move-target w (- PER-ROW))]
    
    [(key=? "e" k) (end-turn w)]
    
    [else w])
  (give-monster-turn-if-attack#=0 w)
  w)

(define (start)
  (big-bang (initialize-orc-world)
            (on-key player-acts-on-monsters)
            (to-draw render-orc-battle)
            (stop-when end-of-orc-battle? render-the-end)))

(define (initialize-player)
  (player MAX-HEALTH MAX-AGILITY MAX-STRENGTH))

(define (random-number of attacks p)
  (random-quotient (player-agility p) ATTACKS#))

(define (random-quotient x y)
  (define div (quotient x y))
  (if (> 0 div) 0 (random+ (add1 div))))

(define (random+ n)
  (add1 (random n)))

(define (initialize-monsters)
  (build-list
    MONSTER#
    (lambda (_)
      (define health (random+ MONSTER-HEALTH0))
      (case (random 4)
        [(0) (orc ORC-IMAGE health (random+ CLUB-STRENGTH))]
        [(1) (hydra HYDRA-IMAGE health)]
        [(2) (slime SLIME-IMAGE health (random+ SLIMINESS))]
        [(3) (brigand BRIGAND-IMAGE health)]))))

(define (instructions w)
  (define na (number->string (orc-world-attack# w)))
  (define ra (string-append REMAINING na))
  (define txt (text ra INSTRUCTION-TEXT-SIZE ATTACK-COLOR))
  (above txt INSTRUCTION-TEXT))

(define (message str)
  (text str MESSAGES-SIZE MESSAGE-COLOR))

(define (render-orc-world w t additional-text)
  (define i-player (render-player (orc-world-player w)))
  (define i-monster (render-monsters (orc-world-lom w) t))
  (above V-SPACER
         (beside H-SPACER
                 i-player
                 H-SPACER H-SPACER H-SPACER
                 (above i-monster
                        V-SPACER V-SPACER V-SPACER
                        additional-text)
                 H-SPACER)
         V-SPACER))

(define (render-player p)
  (define s (player-strength p))
  (define a (player-agility p))
  (define h (player-health p))
  (above/align
    "left"
    (status-bar s MAX-STRENGTH STRENGTH-COLOR STRENGTH)
    V-SPACER
    (status-bar a MAX-AGILITY AGILITY-COLOR AGILITY)
    V-SPACER
    (status-bar h MAX-HEALTH HEALTH-COLOR HEALTH)
    V-SPACER V-SPACER V-SPACER
    PLAYER-IMAGE))

(define (random-number-of-attacks p)
  (random-quotient (player-agility p)
                   ATTACKS#))

(define (status-bar v-current v-max color label)
  (define w (* (/ v-current v-max) HEALTH-BAR-WIDTH))
  (define f (rectangle w HEALTH-BAR-HEIGHT 'solid color))
  (define b (rectangle HEALTH-BAR-WIDTH HEALTH-BAR-HEIGHT 'outline color))
  (define bar (overlay/align "left" "top" f b))
  (beside bar H-SPACER (text label HEALTH-SIZE color)))

(define (render-monsters lom with-target)
  ;; the currently targeted monster (if needed)
  (define target
    (if (number? with-target)
        (list-ref lom with-target)
        'a-silly-symbol-that-cannot-be-eq-to-an-orc))

    (define (render-one-monster m)
      (define image
        (if (eq? m target)
            (overlay TARGET (monster-image m))
            (monster-image m)))
      (define health (monster-health m))
      (define health-bar
        (if (= health 0)
            (overlay DEAD-TEXT (status-bar 0 1 'white ""))
            (status-bar health MONSTER-HEALTH0 MONSTER-COLOR "")))
      (above health-bar image))
  
    ;; arrange here is taking a list of images returned from the mapping
    ;; of render-one-monster
    (arrange (map render-one-monster lom)))

(define (arrange lom)
  (cond
    [(empty? lom) empty-image]
    [else (define r (apply beside (take lom PER-ROW)))
          (above r (arrange (drop lom PER-ROW)))]))

(define (win? w)
  (all-dead? (orc-world-lom w)))

(define (lose? w)
  (player-dead? (orc-world-player w)))

(define (player-dead? p)
  (or (= (player-health p) 0)
      (= (player-agility p) 0)
      (= (player-strength p) 0)))

(define (all-dead? lom)
  (not (ormap monster-alive? lom)))

(define (monster-alive? m)
  (> (monster-health m) 0))

(define (end-turn w)
  (set-orc-world-attack#! w 0))

(define (heal w)
  (decrease-attack# w)
  (player-health+ (orc-world-player w) HEALING))

(define (stab w)
  (decrease-attack# w)
  (define target
    (list-ref (orc-world-lom w) (orc-world-target w)))
  (define damage
    (random-quotient (player-strength (orc-world-player w))
                     STAB-DAMAGE))
  (damage-monster target damage))

(define (flail w)
  (decrease-attack# w)
  (define target (current-target w))
  (define alive (filter monster-alive? (orc-world-lom w)))
  (define pick#
    (min
     (random-quotient (player-strength (orc-world-player w))
                      FLAIL-DAMAGE)
     (length alive)))
  (define getem (cons target (take alive pick#)))
  (for-each (lambda (m) (damage-monster m 1)) getem))

(define (decrease-attack# w)
  (set-orc-world-attack#! w (sub1 (orc-world-attack# w))))

(define (damage-monster m delta)
  (set-monster-health! m (interval- (monster-health m) delta)))

(define (current-target w)
  (list-ref (orc-world-lom w) (orc-world-target w)))

(define (move-target w delta)
  (define new (+ (orc-world-target w) delta))
  (set-orc-world-target! w (modulo new MONSTER#)))


(define (give-monster-turn-if-attack#=0 w)
  (when (zero? (orc-world-attack# w))
    (define player (orc-world-player w))
    (all-monsters-attack-player player (orc-world-lom w))
    (set-orc-world-attack#! w (random-number-of-attacks player))))

;; Player [Listof Monster] -> Void 
;; Each monster attacks the player
(define (all-monsters-attack-player player lom) 
  ;; Monster -> Void
  (define (one-monster-attacks-player monster)
    (cond
      [(orc? monster)
       (player-health+ player (random- (orc-club monster)))]
      [(hydra? monster)
       (player-health+ player (random- (monster-health monster)))]
      [(slime? monster) 
       (player-health+ player -1)
       (player-agility+ player (random- (slime-sliminess monster)))]
      [(brigand? monster) 
       (case (random 3)
         [(0) (player-health+ player HEALTH-DAMAGE)]
         [(1) (player-agility+ player AGILITY-DAMAGE)]
         [(2) (player-strength+ player STRENGTH-DAMAGE)])]))
  ;; -- IN -- 
  (for-each one-monster-attacks-player (filter monster-alive? lom)))


