#lang racket

(require rackunit)

;; node field:
;; - key Number
;; - value
;; - left node or empty
;; - right node or empty

;; a node of bst (binary search tree)

(define-struct node (key value left right)
  #:transparent)

(define N1 (make-node 3 "a" empty empty))
(define N2 (make-node 4 "b" N1 empty))
(define N3 (make-node 8 "c" N1 N2))

;; list-to-bst
;; non-empty sorted list -> node
;; empty list -> empty

#| (define (list-to-bst l) |#
#|   (make-node 0 "a" empty empty)) ;stub |#

(define (split-list-at l p)
  (values (take l p) (list-tail l (+ p 1)) (list-ref l p)))

(check-equal? (call-with-values (thunk (split-list-at (list 1 3) 1)) list) (list (list 1) empty 3))
(check-equal? (call-with-values (thunk (split-list-at (list 1 2 3) 1)) list)
              (list (list 1) (list 3) 2))

(define (split-list-in-half l)
  (split-list-at l (- (quotient (length l) 2) 0)))

(define (list-to-bst l)
  (cond
    [(= (length l) 0) empty]
    [(= (length l) 1) (make-node (car (car l)) (car (cdr (car l))) empty empty)]
    [else
     (let-values ([(left right pivot) (split-list-in-half l)])
       (make-node (car pivot) (car (cdr pivot)) (list-to-bst left) (list-to-bst right)))]))

(check-equal? (list-to-bst (list (list 0 "a"))) (make-node 0 "a" empty empty))
(check-equal? (list-to-bst (list (list 1 "a"))) (make-node 1 "a" empty empty))
(check-equal? (list-to-bst (list (list 1 "a"))) (make-node 1 "a" empty empty))
(check-equal? (list-to-bst (list (list 1 "b") (list 2 "a")))
              (make-node 2 "a" (make-node 1 "b" empty empty) empty))
(check-equal? (list-to-bst (list (list 1 "b") (list 2 "a") (list 3 "c")))
              (make-node 2 "a" (make-node 1 "b" empty empty) (make-node 3 "c" empty empty)))
(check-equal?
 (list-to-bst (list (list 1 "c") (list 2 "b") (list 5 "d") (list 7 "a") (list 8 "f") (list 9 "e")))
 (make-node 7
            "a"
            (make-node 2 "b" (make-node 1 "c" empty empty) (make-node 5 "d" empty empty))
            (make-node 9 "e" (make-node 8 "f" empty empty) empty)))

;; find-in-bst
;; bst -> key Number -> value
;; empty if not found

(define (find-in-bst n k)
  (cond
    [(empty? n) empty]
    [(= k (node-key n)) (node-value n)]
    [(>= k (node-key n)) (find-in-bst (node-right n) k)]
    [else (find-in-bst (node-left n) k)]))

(check-equal? (find-in-bst (list-to-bst (list (list 1 "a"))) 1) "a")
(check-equal? (find-in-bst (list-to-bst (list (list 1 "a"))) 0) empty)
(check-equal? (find-in-bst (list-to-bst (list (list 1 "a")
                                              (list 4 "b")
                                              (list 4 "c")
                                              (list 5 "d")
                                              (list 8 "e")
                                              (list 12 "f")
                                              (list 13 "g")
                                              (list 14 "h")))
                           5)
              "d")
(check-equal? (find-in-bst (list-to-bst (list (list 1 "a")
                                              (list 4 "b")
                                              (list 4 "c")
                                              (list 5 "d")
                                              (list 8 "e")
                                              (list 12 "f")
                                              (list 13 "g")
                                              (list 14 "h")))
                           15)
              empty)
