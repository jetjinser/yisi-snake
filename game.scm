;;; Copyright (C) 2024 David Thompson <dave@spritely.institute>
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

;;; Commentary:
;;;
;;; Example game showing off several common game programming things.
;;;
;;; Code:

;; TODO: 
;; - lose
;; - score
;; - grow

(use-modules (dom canvas)
             (dom document)
             (dom element)
             (dom event)
             (dom image)
             (dom media)
             (dom window)
             (hoot ffi)
             (hoot hashtables)
             (ice-9 match)
             (ice-9 q)
             (math)
             (math rect)
             (math vector)
             (srfi srfi-9)
             (srfi srfi-1))

(define-record-type <position>
  (make-position x y)
  position?
  (x position-x set-position-x!)
  (y position-y set-position-y!))

(define (position= p1 p2)
  (and (= (position-x p1) (position-x p2))
       (= (position-y p1) (position-y p2))))

(define-record-type <snake>
  (make-snake head head-icon tails tail-icon direction)
  snake?
  (head snake-head set-snake-head!)
  (head-icon snake-head-icon)
  (tails snake-tails set-snake-tails!)
  (tail-icon snake-tail-icon)
  (direction snake-direction set-snake-direction!)) ; up down left right

(define-record-type <food>
  (make-food icon position)
  food?
  (icon food-icon)
  (position food-position set-food-position!))

(define-record-type <world>
  (make-world state food snake score)
  world?
  (state world-state set-world-state!) ; play, lose
  (food world-food)
  (snake world-snake)
  (score world-score))

(define (make-world-1)
  (make-world 'play
              (make-food "🐭" (make-position 1 24))
              (make-snake (make-position 16 10)
                          "🐍"
                          (make-q)
                          "🟢"
                          'right)
              0))

;; Game state
(define *world*
  (let* ([world (make-world-1)])
    (for-each (λ (pos) (enq! (snake-tails (world-snake world)) pos))
              (reverse (list (make-position 16 9) (make-position 16 8) (make-position 15 8) (make-position 14 8))))
    world))

;; Game data
(define game-size     800.0)
(define line-count    25)
(define grid-count    (- line-count 1))
(define grid-size     (/ game-size line-count))
; (define snake-velocity 2)
(define hz             4.0)

(define (move- n s)
  (if (< n s) grid-count (- n s)))

(define (move+ n s)
  (if (> (+ n s) grid-count) 0 (+ n s)))

(define (absolute-pos position)
  (let ([x (position-x position)]
        [y (position-y position)])
    (make-position (+ (* x grid-size 1) (/ grid-size 2))
                   (+ (* y grid-size 1) (/ grid-size 2)))))

(define (center-text! context)
  (set-text-align! context "center")
  (set-text-baseline! context "middle"))

(define (move-node! direction node distance)
  (match direction
    ['up (set-position-y! node (move- (position-y node) distance))]
    ['down (set-position-y! node (move+ (position-y node) distance))]
    ['left (set-position-x! node (move- (position-x node) distance))]
    ['right (set-position-x! node (move+ (position-x node) distance))]))

(define (move-snake! snake)
 (let* ([head (snake-head snake)]
        [tails (snake-tails snake)]
        [direction (snake-direction snake)])
   (enq! tails (make-position (position-x head) (position-y head)))
   (deq! tails)
   ; (move-node! direction head (/ hz snake-velocity))
   (move-node! direction head 1)))

(define (spawn-food! snake)
  (let* ([pos-x (round (* (random) grid-count))]
         [pos-y (round (* (random) grid-count))]
         [pos (make-position pos-x pos-y)])
    (if (any1 (λ (p) (position= p pos))
              (cons (snake-head snake) (car (snake-tails snake))))
      (spawn-food! snake)
      pos)))

(define (eat-and-spawn-food! snake food)
  (when (position= (snake-head snake) (food-position food))
    (set-food-position! food (spawn-food! snake))))

(define dt (/ 1000.0 hz))
(define (update)
  (match (world-state *world*)
    ['play
     (let ([snake (world-snake *world*)]
           [food (world-food *world*)])
       (move-snake! snake)
       (eat-and-spawn-food! snake food))]
    [_ #t])
  (timeout update-callback dt))
(define update-callback (procedure->external update))

(define (draw prev-time)
  (set-fill-color! context "#222436")
  ;; draw background & grid
  (fill-rect context 0.0 0.0 game-size game-size)
  (do ([g grid-size (+ g grid-size)])
      ([>= g game-size])
    (draw-line! context 0 g game-size g)
    (draw-line! context g 0 g game-size))
  ;; setup font
  (set-font! context "bold 16px monospace")
  (center-text! context)
  ;; draw food
  (let* ([food (world-food *world*)]
         [food-pos (absolute-pos (food-position food))]
         [food-ico (food-icon food)])
    (fill-text context food-ico (position-x food-pos) (position-y food-pos)))
  ;; draw snake
  (let* ([snake (world-snake *world*)]
         [head-pos (absolute-pos (snake-head snake))]
         [head-icon (snake-head-icon snake)]
         [tails (snake-tails snake)]
         [tail-icon (snake-tail-icon snake)])
    (fill-text context head-icon (position-x head-pos) (position-y head-pos))
    (for-each (lambda (tail-pos)
                (let ([pos (absolute-pos tail-pos)])
                  (fill-text context tail-icon (position-x pos) (position-y pos))))
              (car tails)))
  (request-animation-frame draw-callback))
(define draw-callback (procedure->external draw))

;; Input
(define key:up "ArrowUp")
(define key:down "ArrowDown")
(define key:left "ArrowLeft")
(define key:right "ArrowRight")
(define key:confirm "Enter")

(define (opposite-direction direction)
  (match direction
    ['up 'down]
    ['down 'up]
    ['left 'right]
    ['right 'left]))

(define (set-snake-direction-checked! snake direction)
  (let ([curr-direction (snake-direction snake)])
    (or (eq? curr-direction (opposite-direction direction))
        (set-snake-direction! snake direction))))

(define (on-key-down event)
  (let ([key (keyboard-event-code event)]
        [snake (world-snake *world*)])
    (match (world-state *world*)
      ['play
       (cond
         [(string=? key key:up)
          (set-snake-direction-checked! snake 'up)]
         [(string=? key key:down)
          (set-snake-direction-checked! snake 'down)]
         [(string=? key key:left)
          (set-snake-direction-checked! snake 'left)]
         [(string=? key key:right)
          (set-snake-direction-checked! snake 'right)])]
      [(or 'win 'lose)
       (when (string=? key key:confirm)
         (set! *world* (make-world-1)))])))

(define (on-key-up event) #t)

;; Canvas and event loop setup
(define canvas (get-element-by-id "canvas"))
(define context (get-context canvas "2d"))
(set-element-width! canvas (inexact->exact game-size))
(set-element-height! canvas (inexact->exact game-size))
(add-event-listener! (current-document) "keydown"
                     (procedure->external on-key-down))
(add-event-listener! (current-document) "keyup"
                     (procedure->external on-key-up))
(request-animation-frame draw-callback)
(timeout update-callback dt)
