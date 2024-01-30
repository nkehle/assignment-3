#lang typed/racket
(require typed/rackunit)

;; OAZO Data Definitions
;;-----------------------------------------------------------------------
(struct numC ([n : Number]) #:transparent)
(struct plusC ([l : ExprC] [r : ExprC])#:transparent)
(struct multC ([l : ExprC] [r : ExprC])#:transparent)
(struct squareC ([a : ExprC])#:transparent)
(define-type ExprC (U numC plusC multC squareC))


;; PARSER
;;-----------------------------------------------------------------------
;; Takes in a Sexp of concrete syntax and outputs the AST for the arith lang
(define (parse [code : ExprC]) : ExprC
  (match code
    [(? real? n) (numC n)]
    [(list '+ l r) (plusC (parse l) (parse r))]
    [(list '* l r) (multC (parse l) (parse r))]
    [other (error 'parse "Syntax error in ~e" other)]))

;; TESTS
(check-equal? (parse 5) (numC 5))
(check-equal? (parse '{+ 2 3}) (plusC (numC 2) (numC 3)))
(check-equal? (parse '{* {+ 2 3} 4}) (multC (plusC (numC 2) (numC 3)) (numC 4)))
(check-exn #rx"Syntax error" (lambda() (parse '{+ 2})))


;; INTERPRETER
;;-----------------------------------------------------------------------
;; Interprets the given expression for the Arith language
(define (interp [a : ExprC]) : Number
  (match a
    [(numC n) n]
    [(plusC l r) (+ (interp l) (interp r))]
    [(multC l r) (* (interp l) (interp r))]
    [(squareC a) (expt (interp a) 2)]))

;; TESTS
(check-equal? (interp (numC 5)) 5)
(check-equal? (interp (plusC (numC 3) (numC 2))) 5)
(check-equal? (interp (plusC (multC (numC 2) (numC 3)) (numC 4))) 10)
(check-equal? (interp (squareC (numC 2))) 4)
(check-equal? (interp (squareC (numC 0))) 0)


;; top-interp
;; accepts an Sexp and calls the parse and interp
(define(top-interp [code : Sexp]) : Number
  (interp (parse code)))