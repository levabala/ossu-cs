#lang racket

(require rackunit)

; Design a function that consumes a list of elements lox and a natural number
; n and produces the list formed by including the first element of lox, then
; skipping the next n elements, including an element, skipping the next n
; and so on.

;; Natural -> Natural -> Boolean
(define (should-keep n i)
  (zero? (remainder i (+ n 1))))

(check-equal? (should-keep 0 0) true)
(check-equal? (should-keep 0 1) true)
(check-equal? (should-keep 0 2) true)
(check-equal? (should-keep 1 0) true)
(check-equal? (should-keep 1 1) false)
(check-equal? (should-keep 1 2) true)
(check-equal? (should-keep 2 0) true)
(check-equal? (should-keep 2 1) false)
(check-equal? (should-keep 2 2) false)
(check-equal? (should-keep 2 3) true)

(define (skipn-indexed l n i)
  (local [(define (skipn-indexed-next)
            (skipn-indexed (rest l) n (+ i 1)))]
         (cond
           [(empty? l) empty]
           [(should-keep n i) (cons (first l) (skipn-indexed-next))]
           [else (skipn-indexed-next)])))

;; list -> Natural -> list
(define (skipn l n)
  (skipn-indexed l n 0))

(check-equal? (skipn (list "a" "b" "c" "d" "e" "f") 0) (list "a" "b" "c" "d" "e" "f"))
(check-equal? (skipn (list "a" "b" "c" "d" "e" "f") 1) (list "a" "c" "e"))
(check-equal? (skipn (list "a" "b" "c" "d" "e" "f") 2) (list "a" "d"))
