;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var (string) #:transparent) ;; a variable, e.g., (var "foo")
(struct int (num) #:transparent) ;; a constant number, e.g., (int 17)
(struct add (e1 e2) #:transparent) ;; add two expressions
(struct ifgreater (e1 e2 e3 e4) #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual) #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body)
(struct apair (e1 e2) #:transparent) ;; make a new pair
(struct fst (e) #:transparent) ;; get first part of a pair
(struct snd (e) #:transparent) ;; get second part of a pair
(struct aunit () #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent)

;; Problem 1

;; (1 . (2 . (3 . null)))
(define (racketlist->mupllist l)
  (foldr apair (aunit) l))

(define (mlast l)
  (cond
    [(aunit? (apair-e2 l)) (apair-e1 l)]
    [else (mlast (apair-e2 l))]))

(define (mupllist->racketlist l)
  (local [(define (f l)
            (cond
              [(aunit? (apair-e2 l)) (cons (apair-e1 l) null)]
              [else (cons (apair-e1 l) (f (apair-e2 l)))]))]
         (cond
           [(aunit? (apair-e2 l)) (list (apair-e1 l))]
           [else (f l)])))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (display "\nlookup for: ")
  (display str)
  (display "\n")
  (cond
    [(null? env) (error "unbound variable during evaluation" str)]
    [(equal? (car (car env)) str) (cdr (car env))]
    [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (display "\n\ne: ")
  (display e)
  (display "\nenv: ")
  (display env)
  (display "\n")
  (cond
    [(or (int? e) (closure? e) (aunit? e)) e]
    [(apair? e) (apair (eval-under-env (apair-e1 e) env) (eval-under-env (apair-e2 e) env))]
    [(var? e) (envlookup env (var-string e))]
    [(add? e)
     (let ([v1 (eval-under-env (add-e1 e) env)] [v2 (eval-under-env (add-e2 e) env)])
       (if (and (int? v1) (int? v2))
           (int (+ (int-num v1) (int-num v2)))
           (error "MUPL addition applied to non-number")))]
    [(fun? e) (closure env e)]
    [(ifgreater? e)
     (local [(define v1 (eval-under-env (ifgreater-e1 e) env))
             (define v2 (eval-under-env (ifgreater-e2 e) env))]
            (cond
              [(not (and (int? v1) (int? v2))) (error "MUPL ifgreater applied to non-number")]
              [(> (int-num v1) (int-num v2)) (eval-under-env (ifgreater-e3 e) env)]
              [else (eval-under-env (ifgreater-e4 e) env)]))]
    [(mlet? e)
     (eval-under-env (mlet-body e) (cons (cons (mlet-var e) (eval-under-env (mlet-e e) env)) env))]
    [(call? e)
     (display "\n\ncall. e: ")
     (local [(define funexp (eval-under-env (call-funexp e) env))
             (define actual (eval-under-env (call-actual e) env))]
            (cond
              [(not (closure? funexp))
               (display "\n\ne: ")
               (display e)
               (display "\n\nfunexp: ")
               (display funexp)
               (display "\n\nactual: ")
               (display actual)
               (display "\n\n")
               (error "MUPL call applied to non-closure")]
              [else
               (local [(define fun (closure-fun funexp))
                       (define env-new
                         (cons (cons (fun-formal fun) actual)
                               (if (false? (fun-nameopt fun))
                                   (append (closure-env funexp) env)
                                   (cons (cons (fun-nameopt fun) funexp)
                                         (append (closure-env funexp) env)))))]
                      (eval-under-env (fun-body fun) env-new))]))]
    [(fst? e)
     (local [(define p (eval-under-env (fst-e e) env))]
            (cond
              [(not (apair? p))
               (display "\n\np: ")
               (display p)
               (display "\n")
               (error "MUPL fst applied to non-pair")]
              [else (eval-under-env (apair-e1 p) env)]))]
    [(snd? e)
     (local [(define p (eval-under-env (snd-e e) env))]
            (cond
              [(not (apair? p)) (error "MUPL snd applied to non-pair")]
              [else (eval-under-env (apair-e2 p) env)]))]
    [(isaunit? e)
     (local [(define u (eval-under-env (isaunit-e e) env))] (if (aunit? u) (int 1) (int 0)))]

    [else (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (foldr (lambda (p e) (mlet (car p) (cdr p) e)) e2 lstlst))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifgreater (var "_x") (var "_y") e4 (ifgreater (var "_y") (var "_x") e4 e3))))

;; Problem 4

(define (ml-map mapper)
  (lambda (l)
    (local [(define (f ls)
              (cond
                [(empty? ls) empty]
                [else (cons (mapper (car ls)) (f (cdr ls)))]))]
           (f l))))

(define mupl-map
  (fun #f
       "_mapper"
       (fun #f
            "_l"
            (mlet "_iterator"
                  (fun #f
                       "_ls"
                       (ifaunit (var "_ls")
                                (aunit)
                                (apair (call (var "_mapper") (fst (var "_ls")))
                                       (call (var "_iterator") (snd (var "_ls"))))))
                  (call (var "_iterator") (var "_l"))))))

(define mupl-mapAddN
  (mlet "map"
        mupl-map
        (fun #f "_i" (call (var "map") (fun #f "_val" (add (var "_val") (var "_i")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars)
  #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env)
  "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
