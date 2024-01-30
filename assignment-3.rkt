#lang typed/racket
(require typed/rackunit)

;; OAZO Data Definitions
;;-----------------------------------------------------------------------
(struct numC ([n : Number])              #:transparent)
(struct plusC ([l : ExprC] [r : ExprC])  #:transparent)
(struct multC ([l : ExprC] [r : ExprC])  #:transparent)
(struct idC ([s : Symbol])               #:transparent)
(struct appC ([s : Symbol] [arg : ExprC])#:transparent)
(define-type ExprC (U numC plusC multC idC appC))

#;(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])


;; PARSE FUNCTIONS 
;;-----------------------------------------------------------------------
;; Takes in a Sexp of concrete syntax and outputs the AST for the OAZO language
;; should only be in the form of the above defined data types
(define (parse [code : Sexp]) : ExprC
  (match code
    [(? real? n) (numC n)]
    [(list '+ l r) (plusC (parse l) (parse r))]
    [(list '* l r) (multC (parse l) (parse r))]
    [(? symbol? s) (idC s)]
    [(list (? symbol? s) e) (appC s (parse e))]
    [other (error 'parse "Syntax error in ~e" other)]))

;; Parse Tests
(check-equal? (parse 5) (numC 5))
(check-equal? (parse '{+ 2 3}) (plusC (numC 2) (numC 3)))
(check-equal? (parse '{* {+ 2 3} 4}) (multC (plusC (numC 2) (numC 3)) (numC 4)))
(check-equal? (parse 'a) (idC 'a))
(check-equal? (parse '{f {* 2 1}}) (appC 'f (multC (numC 2) (numC 1))))
;;(check-exn #rx"Syntax error" (lambda() (parse '{+ 2}))) ;; TODO


;; INTERP FUNCTIONS
;;-----------------------------------------------------------------------

;; Inteprets the function named main from the func definitons
(define (interp-fns (funs : (Listof FundefC))) : Real
  0)

;; Interp-fns Tests TODO

;;-----------------------------------------------------------------------

;; Inteprets the given expression using list of funs to resolve appC's
(define (interp [a : ExprC]) : Number
  (match a
    [(numC n) n]
    [(idC s) (error 'interp "Logic Error: Interp of an idC: ~e" s)]
    #;[(appC f arg) ...] ;; TODO
    [(plusC l r) (+ (interp l) (interp r))]
    [(multC l r) (* (interp l) (interp r))]))

;; Interp Tests
(check-equal? (interp (numC 5)) 5)
(check-equal? (interp (plusC (numC 3) (numC 2))) 5)
(check-equal? (interp (plusC (multC (numC 2) (numC 3)) (numC 4))) 10)

;;-----------------------------------------------------------------------

;; Takes a Sexp and runs the interp and parse functions on the input
(define(top-interp [code : Sexp]) : Number
  (interp (parse code)))

;; Top-Interp Tests
(check-equal? (interp (parse 5)) 5)
(check-equal? (interp (parse '{+ 2 3})) 5)
(check-equal? (interp (parse '{* {+ 2 3} 4})) 20)
(check-exn #rx"Logic Error" (lambda() (interp (parse 'a)))) 
;;(check-equal? (interp (parse '{f 12})) )


