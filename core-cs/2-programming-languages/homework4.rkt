#lang racket

#|
while-less: Macro terminates with #t as result (do: bad syntax   in: do) [error]
while-less: Evaluates e2 the correct number of times (do: bad syntax   in: do) [error]
while-less: Evaluates e1 only once (do: bad syntax   in: do) [error]
cached-assoc: Returns the same thing as assoc 
(In this test, a1 is the result of evaluating (cached-assoc (quote ((5 . 2) (blah . 4))) 1). 
    This test calls (a1 5), and then checks that calling (a1 5) again gives the right answer; 
    Result of (a1 5) was expected to equal '(5 . 2)) [incorrect answer]

Because the auto-grader gave a score above 80, here is the link to a message from a very cute dog: https://drive.google.com/file/d/0B5sUgbs6aDNpSWhSZzVtcktDaTA/view?pref=2&pli=1
|#

(define (sequence low high stride)
  (cond
    [(> low high) empty]
    [else (cons low (sequence (+ low stride) high stride))]))

(define (string-append-map xs suffix)
  (map (lambda (v) (string-append v suffix)) xs))

(define (list-nth-mod xs n)
  (cond
    [(< n 0) (error "list-nth-mod: negative number")]
    [(empty? xs) (error "list-nth-mod: empty list")]
    [else (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (cond
    [(<= n 0) empty]
    [else (local [(define v (s))] (cons (car v) (stream-for-n-steps (cdr v) (- n 1))))]))

(define (funny-number-stream)
  (local [(define (f i)
            (cond
              [(= (remainder i 5) 0) (cons (* -1 i) (lambda () (f (+ i 1))))]
              [else (cons i (lambda () (f (+ i 1))))]))]
         (f 1)))

(define (dog-then-dan)
  (cons "dog.jpg" dan-then-dog))
(define (dan-then-dog)
  (cons "dan.jpg" dog-then-dan))

(define (stream-add-zero s)
  (local [(define (f sc)
            (lambda () (local [(define sr (sc))] (cons (cons 0 (car sr)) (f (cdr sr))))))]
         (f s)))

(define (cycle-list xs)
  (local [(define (f xss)
            (lambda ()
              (cond
                [(empty? (cdr xss)) (cons (car xss) (f xs))]
                [else (cons (car xss) (f (cdr xss)))])))]
         (f xs)))

(define (cycle-lists xs ys)
  (local [(define (f s1 s2)
            (lambda ()
              (local [(define r1 (s1)) (define r2 (s2))]
                     (cons (cons (car r1) (car r2)) (f (cdr r1) (cdr r2))))))]
         (f (cycle-list xs) (cycle-list ys))))

(define (vector-assoc v vec)
  (local [(define (f i)
            (cond
              [(>= i (vector-length vec)) false]
              [else
               (local [(define p (vector-ref vec i))]
                      (cond
                        [(not (pair? p)) (f (+ i 1))]
                        [(equal? (car p) v) p]
                        [else (f (+ i 1))]))]))]
         (f 0)))

(define (cached-assoc xs n)
  (local [(define cache (make-vector n)) (define cache-caret 0)]
         (lambda (v)
           (local [(define cache-result (vector-assoc v cache))]
                  (cond
                    [(pair? cache-result) (cdr cache-result)]
                    [else
                     (local [(define new-result (assoc v xs))]
                            (when (not (false? new-result)) (vector-set! cache cache-caret (cons v new-result)))
                            (set! cache-caret (if (>= (add1 cache-caret) n) 0 (add1 cache-caret)))
                            new-result)])))))

(provide sequence
         string-append-map
         list-nth-mod
         stream-for-n-steps
         funny-number-stream
         dan-then-dog
         stream-add-zero
         cycle-list
         cycle-lists
         vector-assoc
         cached-assoc)
