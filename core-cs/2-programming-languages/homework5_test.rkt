#lang racket
;; Programming Languages Homework 5 Simple Test
;; Save this file to the same directory as your homework file
;; These are basic tests. Passing these tests does not guarantee that your code will pass the actual homework grader

;; Be sure to put your homework file in the same folder as this test file.
;; Uncomment the line below and, if necessary, change the filename
(require "homework5.rkt")

(require rackunit)

(define tests
  (test-suite "Sample tests for Assignment 5"

    ;; check racketlist to mupllist with normal list
    (check-equal? (racketlist->mupllist (list (int 3) (int 4)))
                  (apair (int 3) (apair (int 4) (aunit)))
                  "racketlist->mupllist test")

    (check-equal? (mlast (apair (int 3) (apair (int 4) (aunit)))) (int 4) "mlast")

    ;; check mupllist to racketlist with normal list
    (check-equal? (mupllist->racketlist (apair (int 3) (apair (int 4) (aunit))))
                  (list (int 3) (int 4))
                  "racketlist->mupllist test")
    (check-equal? (mupllist->racketlist (apair (int 3) (aunit)))
                  (list (int 3))
                  "racketlist->mupllist test 2")
    (check-equal? (mupllist->racketlist (apair (int 3) (apair (int 4) (apair (int 5) (aunit)))))
                  (list (int 3) (int 4) (int 5))
                  "racketlist->mupllist test")

    ;; tests if ifgreater returns (int 2)
    (check-equal? (eval-exp (ifgreater (int 3) (int 4) (int 3) (int 2))) (int 2) "ifgreater test")
    (check-equal? (eval-exp (ifgreater (int 3) (int 4) (int 3) (add (int 2) (int 2))))
                  (int 4)
                  "ifgreater test 2")
    (check-equal? (eval-exp (ifgreater (int 4) (int 3) (add (int 3) (int 1)) (int 2)))
                  (int 4)
                  "ifgreater test 3")
    (check-equal? (eval-exp (ifgreater (add (int 3) (int 2)) (int 4) (int 3) (int 2)))
                  (int 3)
                  "ifgreater test 4")

    ;; mlet test
    (check-equal? (eval-exp (mlet "x" (int 1) (add (int 5) (var "x")))) (int 6) "mlet test")

    ;; call test
    (check-equal? (eval-exp (call (closure '() (fun #f "x" (add (var "x") (int 7)))) (int 1)))
                  (int 8)
                  "call test")
    (check-equal?
     (eval-exp
      (call
       (fun "f" "x" (ifgreater (var "x") (int 0) (call (var "f") (add (var "x") (int -1))) (var "x")))
       (int 3)))
     (int 0)
     "call test 2")

    ;;fst test
    (check-equal? (eval-exp (fst (apair (int 1) (int 2)))) (int 1) "fst test")

    ;;snd test
    (check-equal? (eval-exp (snd (apair (int 1) (int 2)))) (int 2) "snd test")

    ;; isaunit test
    (check-equal? (eval-exp (isaunit (closure '() (fun #f "x" (aunit))))) (int 0) "isaunit test")
    (check-equal? (eval-exp (isaunit (int 1))) (int 0) "isaunit test 2")
    (check-equal? (eval-exp (isaunit (aunit))) (int 1) "isaunit test 3")

    ;; ifaunit test
    (check-equal? (eval-exp (ifaunit (int 1) (int 2) (int 3))) (int 3) "ifaunit test")
    (check-equal? (eval-exp (ifaunit (aunit) (int 2) (int 3))) (int 2) "ifaunit test 2")

    ;; mlet* test
    (check-equal? (eval-exp (mlet* (list (cons "x" (int 10))) (var "x"))) (int 10) "mlet* test")
    (check-equal? (eval-exp (mlet* (list (cons "x" (int 10)) (cons "x" (int 12))) (var "x")))
                  (int 12)
                  "mlet* test 2")
    (check-equal? (eval-exp (mlet* (list (cons "y" (int 10)) (cons "x" (int 12))) (var "y")))
                  (int 10)
                  "mlet* test 2")
    (check-equal? (eval-exp (mlet* (list (cons "x" (int 10)) (cons "y" (int 12))) (var "y")))
                  (int 12)
                  "mlet* test 2")

    ;; ifeq test
    (check-equal? (eval-exp (ifeq (int 1) (int 2) (int 3) (int 4))) (int 4) "ifeq test")

    ;; ml-map test
    (check-equal? ((ml-map (lambda (v) (add1 v))) (list 1 2 3 4 5)) (list 2 3 4 5 6) "ml-map test")

    ;; mupl-map test
    (check-equal? (eval-exp (call (call mupl-map (fun #f "x" (add (var "x") (int 7))))
                                  (apair (int 1) (aunit))))
                  (apair (int 8) (aunit))
                  "mupl-map test")
    (check-equal? (eval-exp (call (call mupl-map
                                        (fun "f"
                                             "x"
                                             (ifgreater (var "x")
                                                        (int 0)
                                                        (call (var "f") (add (var "x") (int -1)))
                                                        (var "x"))))
                                  (apair (int 1) (apair (int 2) (aunit)))))
                  (apair (int 0) (apair (int 0) (aunit)))
                  "mupl-map test 2")

    ;; problems 1, 2, and 4 combined test
    (check-equal?
     (mupllist->racketlist (eval-exp (call (call mupl-mapAddN (int 7))
                                           (racketlist->mupllist (list (int 3) (int 4) (int 9))))))
     (list (int 10) (int 11) (int 16))
     "combined test")
    (check-equal?
     (mupllist->racketlist (eval-exp (call (call mupl-mapAddN (int 7))
                                           (racketlist->mupllist (list (int 3))))))
     (list (int 10))
     "combined test")
    ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
