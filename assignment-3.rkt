#lang typed/racket
(require typed/rackunit)

;; OAZO Data Definitions
;;-----------------------------------------------------------------------

;; Expressions
(struct numC ([n : Real])               #:transparent)
(struct idC ([s : Symbol])                #:transparent)
(struct appC ([s : Symbol] [arg : ExprC]) #:transparent)
(struct binopC ([op : Symbol][l : ExprC] [r : ExprC])          #:transparent)
(struct ifleq0? ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(define-type ExprC (U numC idC appC binopC ifleq0?))

;; Function Definitions
(struct fdC ([name : idC] [arg : idC] [body : ExprC]) #:transparent)
(define-type FunDefC fdC)


;; PARSE FUNCTIONS 
;;-----------------------------------------------------------------------

;; Helper to determine if the symbol is valid for an idC
(define (symbol-valid [s : Symbol]) : Boolean
  (match s
    ['+ #f] ['- #f] ['* #f] ['/ #f] ['ifleq0? #f] ['else: #f] ['ifleq0? #f] [': #f]
    [other #t]))

;; Helper to determine if its a valid operand
(define (operand-valid [s : Symbol]) : Boolean
  (match s
    ['+ #t] ['- #t] ['* #t] ['/ #t]
    [ other #f]))

;;-----------------------------------------------------------------------

;; Takes in a Sexp of concrete syntax and outputs the AST for the OAZO language
;; should only be in the form of the above defined data types
(define (parse [code : Sexp]) : ExprC
  (match code
    [(? real? n) (numC n)]
    [(list (and (? symbol? op) (? operand-valid s)) l r) (binopC op (parse l) (parse r))]
    [(list (and (? symbol? s) (? symbol-valid s)) expr) (appC s (parse expr))]
    [(list 'ifleq0? test then else) (ifleq0? (parse test) (parse then) (parse else))]
    [(and (? symbol? s) (? symbol-valid s)) (idC s)]
    [other (error 'parse "Syntax error in ~e" other)]))


;; Parse Tests
(check-equal? (parse 5) (numC 5))
(check-equal? (parse '{+ 2 3}) (binopC '+ (numC 2) (numC 3)))
(check-equal? (parse '{* {+ 2 3} 4}) (binopC '* (binopC '+ (numC 2) (numC 3)) (numC 4)))
(check-equal? (parse '{ifleq0? x x {+ x 1}}) (ifleq0? (idC 'x) (idC 'x) (binopC '+ (idC 'x) (numC 1))))
(check-equal? (parse 'a) (idC 'a))
(check-equal? (parse '{f {* 2 1}}) (appC 'f (binopC '* (numC 2) (numC 1))))
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
                                                           (binopC '+ (idC 'x) (numC 14))))
(check-equal? (parse-fundef '{func {f y} : {* y 2}}) (fdC (idC 'f) (idC 'y)
                                                           (binopC '* (idC 'y) (numC 2))))

;;-----------------------------------------------------------------------

;; Takes in the whole program and parses the function definitions
(define (parse-prog [s : Sexp]) : (Listof FunDefC)
  (match s
    [(list fs ...) (map parse-fundef fs)]
    [else '()]))


;; Parse-prog Tests
#;(check-equal? ((parse-prog '{{func {f x} : {+ x 14}}
                             {func {main init} : {f 2}}})) (list (fdC)))

;; INTERP FUNCTIONS
;;-----------------------------------------------------------------------

;; Helper for searching through the list of funs TODO
(define (get-fundef [n : Symbol] [fds : (Listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "reference to undefined function")]
    [(cons? fds) (cond
                   [(equal? n (fdC-name (first fds))) (first fds)]
                   [else (get-fundef n (rest fds))])]))


;; Helper for subsituting identifiers into expressions TODO
(define (sub [what : ExprC] [for : Symbol] [in : ExprC]) : ExprC
  (match in
    [(numC n) in]
    [(idC s) (if (equal? s for) what in)]
    [(binopC op l r) (binopC op (sub what for l) (sub what for r))]
    [(appC f a) (appC f (sub what for a))]
    [(ifleq0? test then else) (ifleq0? (sub what for test)
                                       (sub what for then)
                                       (sub what for else))]
    [else (error 'sub "Unknown expression: ~a" what)]))


(check-equal? (sub (numC 5) 'x (idC 'x))
              (numC 5))
(check-equal? (sub (numC 5) 'x (binopC '+ (idC 'x) (numC 1)))
              (binopC '+ (numC 5) (numC 1)))


;; Inteprets the function named main from the func definitons
#;(define (interp-fns (funs : (Listof FundefC))) : Real
  0)

;;-----------------------------------------------------------------------

;; Inteprets the given expression using list of funs to resolve appC's
(define (interp [a : ExprC] [fds : (Listof FunDefC)]) : Real
  (match a
    [(numC n) n]
    [(idC s) (error 'interp "Logic Error: Interp of an idC: ~e" s)]
    [(ifleq0? test then else)
     (if (>= (cast (interp test fds) Real) 0)  ;; double check this, weird behavior
         (interp then fds)
         (interp else fds))]
    [(binopC op a b) (match op
                       ['+ (+ (interp a fds) (interp b fds))]
                       ['- (- (interp a fds) (interp b fds))]
                       ['* (* (interp a fds) (interp b fds))]
                       ['/ (/ (interp a fds) (interp b fds))])]
    
    [(appC f a) (local ([define fd (get-fundef f fds)])
                  (interp (sub a (fdC-arg fd) (fdC-body fd)) ;;???
                      fds))]

    [else (error 'interp "Unknown expression: ~e" a)]))

;; Interp Tests
(define fds(list (fdC (idC 'f) (idC 'x) (binopC '+ (idC 'x) (numC 1)))
                 (fdC (idC 'g) (idC 'x) (binopC '* (idC 'x) (numC 2)))))

(check-equal? (interp (numC 5) fds) 5)
(check-equal? (interp (binopC '+ (numC 3) (numC 2)) fds) 5)
(check-equal? (interp (binopC '+ (binopC '* (numC 2) (numC 3)) (numC 4)) fds) 10)
(check-equal? (interp (parse '{ifleq0? 10 3 -1}) fds) 3)
(displayln (interp (parse '{f 12}) fds))

;;-----------------------------------------------------------------------

;; Interprets the entirely parsed program TODO
#;(define (top-interp [program : Sexp]): Real
  (interp-fns (parse-prog program)))

;; Top-Interp Tests
#;(check-equal? (top-interp
               '{{func {f x} : {+ x 14}}
                 {func {main init} : {f 2}}}) 16)




