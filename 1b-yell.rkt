#lang racket

(require test-engine/racket-tests)

;; String -> String
;; produces string by adding "!" to the end the passed string

#| (define (yell str) "") |#

(define (yell str) (string-append str "!"))

(check-expect (yell "hello") "hello!")
(check-expect (yell "") "!")
(check-expect (yell "bye") "bye!")

(test)
