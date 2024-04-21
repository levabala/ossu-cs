#lang racket

(require rackunit)

;; bt-contains-tr-starter.rkt

; Problem:
;
; Starting with the following data definition for a binary tree (not a binary search tree)
; design a tail-recursive function called contains? that consumes a key and a binary tree
; and produces true if the tree contains the key.
;

(define-struct node (k v l r)
  #:transparent)
;; BT is one of:
;;  - false
;;  - (make-node Integer String BT BT)
;; Interp. A binary tree, each node has a key, value and 2 children
(define BT1 false)
(define BT2
  (make-node 1
             "a"
             (make-node 6 "f" (make-node 4 "d" false false) false)
             (make-node 7 "g" false false)))

;; BT -> Integer -> Boolean
;; that's a shitty one (non-tail-recursive)
#;
(define (contains? bt k)
  (cond
    [(false? bt) false]
    [(= k (node-k bt)) true]
    [(contains? (node-l bt) k) true]
    [else (contains? (node-r bt) k)]))

;; the good one (tail-recursive)
(define (contains? bt k)
  (local [(define (fn-for-node n todo)
            (cond
              [(false? n) (fn-for-next-nodes todo)]
              [(= k (node-k n)) true]
              [else (fn-for-next-nodes (append (list (node-l n) (node-r n)) todo))]))
          (define (fn-for-next-nodes todo)
            (cond
              [(empty? todo) false]
              [else (fn-for-node (first todo) (rest todo))]))]
         (fn-for-node bt empty)))

(check-equal? (contains? BT2 1) true)
(check-equal? (contains? BT2 0) false)
(check-equal? (contains? BT2 6) true)
(check-equal? (contains? BT2 7) true)
