(require test-engine/racket-tests)

;; ListOfBoolean is one of:
;; - empty
;; - (cons true ListOfBoolean)
;; - (cons false ListOfBoolean)

(define LOB1 empty)
(define LOB2 (cons true empty))
(define LOB3 (cons false (cons true empty)))

#;
(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]
        [else 
          (... (first lob)
               (fn-for-lob (rest lob)))]))


;; check-if-all-true 
;; ListOfBoolean -> boolean
;; produces true if all values of the list are true, false otherwise
;; empty list produces true
#| (define (check-if-all-true lob) false) |#

(define (check-if-all-true lob)
  (cond [(empty? lob) true]
        [else 
          (and (first lob)
               (check-if-all-true (rest lob)))]))

(check-expect (check-if-all-true empty) true)
(check-expect (check-if-all-true (cons false empty)) false)
(check-expect (check-if-all-true (cons true empty)) true)
(check-expect (check-if-all-true (cons true (cons true empty))) true)
(check-expect (check-if-all-true (cons false (cons true empty))) false)
(test)
