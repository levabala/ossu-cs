#lang racket

(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)

(define (sqf r)
  (square r "outline" "red"))

(define (sqb r)
  (square r "outline" "transparent"))

;; Number -> Image
;; product Sierpinski carpet image of given size

(define CUTOFF 2)

(define (scarpet n)
  (sqf CUTOFF))

(check-expect (scarpet CUTOFF) (sqf CUTOFF))
(check-expect (scarpet (* CUTOFF 3))
              (overlay (sqf (* CUTOFF 3)) (beside (sqf CUTOFF) (sqf CUTOFF) (sqf CUTOFF))))
(test)

(define (draw-carpet filled blank)
  (above (beside filled filled filled) (beside filled blank filled) (beside filled filled filled)))

(define (draw-stuff r)
  (cond
    [(<= r CUTOFF) (sqf r)]
    [else
     (overlay (sqf r)
              (local [(define rn (/ r 3))
                      (define filled (overlay (sqf rn) (draw-stuff rn)))
                      (define blank (sqb rn))]
                     (draw-carpet filled blank)))]))

(big-bang 300 (on-tick identity 999) (to-draw draw-stuff))
