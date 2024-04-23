#lang racket

(require rackunit)
;; max-exits-to-starter.rkt

; PROBLEM:
;
; Using the following data definition, design a function that produces the room to which the greatest
; number of other rooms have exits (in the case of a tie you can produce any of the rooms in the tie).

;; Data Definitions:

(define-struct room (name exits)
  #:transparent
  #:mutable)
;; Room is (make-room String (listof Room))
;; interp. the room's name, and list of rooms that the exits lead to

(define H1 (make-room "A" (list (make-room "B" empty))))
(define H2
  (shared ([-0- (make-room "A" (list (make-room "B" (list -0-))))])
    -0-))
(define H3
  (shared ([-A- (make-room "A" (list -B-))]
           [-B- (make-room "B" (list -C-))]
           [-C- (make-room "C" (list -A-))])
    -A-))
(define H4
  (shared ([-A- (make-room "A" (list -B- -D-))]
           [-B- (make-room "B" (list -C- -E-))]
           [-C- (make-room "C" (list -B-))]
           [-D- (make-room "D" (list -E-))]
           [-E- (make-room "E" (list -F- -A-))]
           [-F- (make-room "F" (list))])
    -A-))

;; hash room to enters count -> room -> hash room to enters count
;; increments the amount of enters for the given room r
(define (rweh-add rweh r)
  (if (hash-has-key? rweh r) (hash-update rweh r add1) (hash-set rweh r 1)))

(define (rweh-add-list rweh rl)
  (foldl (lambda (r rweh-curr) (rweh-add rweh-curr r)) rweh rl))

(define R1 (make-room "A" empty))
(define R2 (make-room "B" empty))
(define R3 (make-room "C" empty))

(check-equal? (rweh-add (hash) R1) (hash R1 1))
(check-equal? (rweh-add (hash R1 1) R1) (hash R1 2))
(check-equal? (rweh-add (hash R1 1 R2 1) R2) (hash R1 1 R2 2))
(check-equal? (rweh-add (hash R1 1 R2 2) R3) (hash R1 1 R2 2 R3 1))

(define (room-all-enters r)
  ;; rweh - rooms with enters hash
  (local [(define (fn-for-room r todo visited rweh)
            (local [(define todo-new
                      (append (filter (lambda (r2) (not (member r2 visited))) (room-exits r)) todo))
                    (define visited-new (cons r visited))
                    (define rweh-new (rweh-add-list rweh (room-exits r)))]
                   (fn-for-room-list todo-new visited-new rweh-new)))
          (define (fn-for-room-list todo visited rweh)
            (cond
              [(empty? todo) rweh]
              [else (fn-for-room (first todo) (rest todo) visited rweh)]))]
         (fn-for-room r empty empty (hash))))

;; Room room -> String room name
(define (greatest-enters-room r)
  (local [(define all-enters (hash->list (room-all-enters r)))]
         (sort (map (lambda (entry) (room-name (car entry)))
                    (foldl (lambda (entry entries-max)
                             (cond
                               [(> (cdr entry) (cdr (car entries-max))) (list entry)]
                               [(= (cdr entry) (cdr (car entries-max))) (cons entry entries-max)]
                               [else entries-max]))
                           (list (first all-enters))
                           (rest all-enters)))
               string<?)))

(define (equal-one-of? v1 l)
  (ormap (lambda (v2) (equal? v2 v1)) l))

(check-equal? (greatest-enters-room H1) (list "B"))
(check-equal? (greatest-enters-room H2) (list "A" "B"))
(check-equal? (greatest-enters-room H3) (list "A" "B" "C"))
(check-equal? (greatest-enters-room H4) (list "B" "E"))
