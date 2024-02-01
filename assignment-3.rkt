#lang typed/racket
(require typed/rackunit)

;; OAZO Data Definitions
;;-----------------------------------------------------------------------

;; Expr
(struct numC ([n : Number])              #:transparent)
(struct plusC ([l : ExprC] [r : ExprC])  #:transparent)
(struct multC ([l : ExprC] [r : ExprC])  #:transparent)
(struct idC ([s : Symbol])               #:transparent)
(struct appC ([s : Symbol] [arg : ExprC])#:transparent)
(struct ifleq0? ([a : ExprC] [b : ExprC] [c : ExprC]) #:transparent)
(define-type ExprC (U numC plusC multC idC appC ifleq0?))

;; Defn
(struct fdC ([name : idC] [arg : idC] [body : ExprC]) #:transparent)
(define-type FunDefC fdC)


;; PARSE FUNCTIONS 
;;-----------------------------------------------------------------------
;; Helper to determine if the symbol is valid for an idC
(define (symbol-valid [s : Symbol]) : Boolean
  (match s
    ['+ #f] ['- #f] ['* #f] ['/ #f] ['ifleq0? #f] ['else: #f] ['ifleq0? #f] [': #f]
    [other #t]))

; Helper Tests
(check-equal? (symbol-valid '+) #f)
(check-equal? (symbol-valid 'name) #t)

;;-----------------------------------------------------------------------

;; Takes in a Sexp of concrete syntax and outputs the AST for the OAZO language
;; should only be in the form of the above defined data types
(define (parse [code : Sexp]) : ExprC
  (match code
    [(? real? n) (numC n)]
    [(list '+ l r) (plusC (parse l) (parse r))]
    [(list '* l r) (multC (parse l) (parse r))]
    [(list a b c ) (ifleq0? (parse a) (parse b) (parse c))] ;; TODO
    [(and (? symbol? s) (? symbol-valid s)) (idC s)]
    [(list (and (? symbol? s) (? symbol-valid s)) expr) (appC s (parse expr))]
    [other (error 'parse "Syntax error in ~e" other)]))

;; Parse Tests
(check-equal? (parse 5) (numC 5))
(check-equal? (parse '{+ 2 3}) (plusC (numC 2) (numC 3)))
(check-equal? (parse '{* {+ 2 3} 4}) (multC (plusC (numC 2) (numC 3)) (numC 4)))
(check-equal? (parse '{ifleq0? x x {+ x 1}}) (ifleq0? (idC 'x) (idC 'x) (plusC (idC 'x) 1)))
(check-equal? (parse 'a) (idC 'a))
(check-equal? (parse '{f {* 2 1}}) (appC 'f (multC (numC 2) (numC 1))))
(check-exn #rx"Syntax error" (lambda() (parse '{* 2})))
(check-exn #rx"Syntax error" (lambda() (parse '{+ 2 3 4})))


;;-----------------------------------------------------------------------

;; Takes in an Sexp and parses it into and AST for the OAZO language
(define (parse-fundef [code : Sexp]) : FunDefC
  (match code
    [(list 'func (list (? symbol? name) (? symbol? arg)) ': body)
     (fdC (idC name) (idC arg) (parse body))]
    [else (error 'parse-func-def "Syntax error: ~e" code)]))

;; Parse FunDef Tests
(check-equal? (parse-fundef '{func {f x} : {+ x 14}}) (fdC (idC 'f) (idC 'x)
                                                           (plusC (idC 'x) (numC 14))))
;;-----------------------------------------------------------------------

;; Takes in the whole program and parses the function definitions
(define (parse-prog [code: Sexp]) : (Listof FunDefC)
  (match code
    []
    []))
;; Parse-prog Tests
(check-equal? ((parse-prog '{{func {f x} : {+ x 14}}
                             {func {main init} : {f 2}}})) (list (fdC)))

;; INTERP FUNCTIONS
;;-----------------------------------------------------------------------

;; Inteprets the function named main from the func definitons
#;(define (interp-fns (funs : (Listof FundefC))) : Real
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

;; Interprets the entirely parsed program TODO
(define (top-interp [program : Sexp]): Real
  (interp-fns (parse-prog program)))

;; Top-Interp Tests
(check-equal? (top-interp
               '{{func {f x} : {+ x 14}}
                 {func {main init} : {f 2}}}) 16)




