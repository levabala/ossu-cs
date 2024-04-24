#lang racket

(require rackunit)

;; average-starter.rkt

; PROBLEM:
;
; Design a function called average that consumes (listof Number) and produces the
; average of the numbers in the list.

;; listof Number -> Number
(define (average l)
  ;; listof Number -> Number (sum accumulator) -> Number (count accumulator) -> Number
  (local [(define (average-acc l s c)
            (cond
              [(empty? l) (/ s c)]
              [else (average-acc (rest l) (+ s (first l)) (add1 c))]))]
         (average-acc l 0 0)))

; create tests for average via check-equal? 
(check-equal? (average (list 1 2 3 4 5)) 3)
(check-equal? (average (list 1 2 3 4 5 6 7 8 9 10)) 11/2)
(check-equal? (average (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)) 8)
