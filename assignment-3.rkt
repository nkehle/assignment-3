#lang typed/racket
(require typed/rackunit)

;; Assignment 3
;; Full Project Implemented


;; OAZO Data Definitions
;;-----------------------------------------------------------------------------------

;; Expressions
(struct numC ([n : Real])                 #:transparent)
(struct idC ([s : Symbol])                #:transparent)
(struct appC ([s : Symbol] [arg : (U (Listof ExprC) ExprC)]) #:transparent)
(struct binopC ([op : Symbol][l : ExprC] [r : ExprC])          #:transparent)
(struct ifleq0? ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(define-type ExprC (U numC idC appC binopC ifleq0?))

;; Function Definitions
(struct fdC ([name : Symbol] [arg : Symbol] [body : ExprC]) #:transparent)


;; PARSE HELPER FUNCTIONS
;;-----------------------------------------------------------------------------------

;; Helper to determine if the symbol is valid for an idC
(define (symbol-valid [s : Symbol]) : Boolean
  (match s
    [(or '+ '- '* '/ 'ifleq0? 'else: 'ifleq0? ': 'func) #f]
    [other #t]))

;; Helper to determine if its a valid operand
(define (operand-valid [s : Symbol]) : Boolean
  (match s
    [(or '+ '- '* '/) #t]
    [ other #f]))


;; Tests for symbol-valid
(check-equal? (symbol-valid '+) #f)
(check-equal? (symbol-valid '-) #f)
(check-equal? (symbol-valid '*) #f)
(check-equal? (symbol-valid '/) #f)
(check-equal? (symbol-valid 'ifleq0?) #f)
(check-equal? (symbol-valid 'else:) #f)
(check-equal? (symbol-valid ':) #f)
(check-equal? (symbol-valid 'func) #f)
(check-equal? (symbol-valid 'x) #t)

;; Tests for operand-valid
(check-equal? (operand-valid '+) #t)
(check-equal? (operand-valid '-) #t)
(check-equal? (operand-valid '*) #t)
(check-equal? (operand-valid '/) #t)


;; PARSE
;;-----------------------------------------------------------------------------------

;; Takes in a Sexp of concrete syntax and outputs the AST for the OAZO language
;; should only be in the form of the above defined data types
(define (parse [code : Sexp]) : ExprC
  (match code
    [(? real? n) (numC n)]                               ;; numC
    [(list (? real? n)) (numC n)]                        ;; numC in {12}
    [(list (and (? symbol? op) (? operand-valid s)) l r) ;; biopC
     (binopC s (parse l) (parse r))]
    [(list (and (? symbol? s) (? symbol-valid s)) exp ...)  ;; appC
     (appC s (map (lambda ([exp : Sexp])
                    (parse exp))))]
    
    [(list 'ifleq0? test then else)                      ;; ifleq0?
     (ifleq0? (parse test) (parse then) (parse else))]
    [(and (? symbol? s) (? symbol-valid s)) (idC s)]     ;; idC 
    [other (error 'parse "OAZO Syntax error in ~e" other)]))

;; Parse Tests
(check-equal? (parse 5) (numC 5))
(check-equal? (parse '{+ 2 3}) (binopC '+ (numC 2) (numC 3)))
(check-equal? (parse '{* {+ 2 3} 4}) (binopC '* (binopC '+ (numC 2) (numC 3)) (numC 4)))
(check-equal? (parse '{ifleq0? x x {+ x 1}}) (ifleq0? (idC 'x) (idC 'x) (binopC '+ (idC 'x) (numC 1))))
(check-equal? (parse 'a) (idC 'a))
(check-equal? (parse '{f {* 2 1}}) (appC 'f (binopC '* (numC 2) (numC 1))))
(check-exn #rx"Syntax error" (lambda() (parse '{* 2})))
(check-exn #rx"Syntax error" (lambda() (parse '{+ 2 3 4})))
(check-exn #rx"Syntax error" (lambda() (parse '{+ func a})))

;; Parse Tests OAZO4
(check-equal? (parse '{f {1 2 3}}) (appC 'f (list (numC 1) (numC 2)(numC 3))))
;;(check-exn (lambda() parse '{f {}})



;; PARSE-FUNDEF
;;-----------------------------------------------------------------------------------

;; Takes in an Sexp and parses it into and function definition for the OAZO language
(define (parse-fundef [code : Sexp]) : fdC
  (match code
    [(list 'func (list (and (? symbol? name) (? symbol-valid name))
                       (and (? symbol? arg) (? symbol-valid arg))) ': body)
     (fdC name arg (parse body))]
    [else (error 'parse-func-def "OAZO Syntax Error: ~e" code)]))

;; Parse FunDef Tests
(check-equal? (parse-fundef '{func {f x} : {+ x 14}})
              (fdC 'f 'x (binopC '+ (idC 'x) (numC 14))))

(check-equal? (parse-fundef '{func {f y} : {* y 2}})
              (fdC 'f 'y (binopC '* (idC 'y) (numC 2))))

(check-equal? (parse-fundef '{func {f x} : {6}})
              (fdC 'f 'x (numC 6)))

(check-exn #rx"OAZO Syntax Error:" (lambda() (parse-fundef '{func {+ x} : 12})))
(check-exn #rx"OAZO Syntax Error:" (lambda() (parse-fundef '{func {f +} : 12})))


;; PARSE-PROG 
;;-----------------------------------------------------------------------------------

;; Takes in the whole program and parses the function definitions and outputs
;; the list of all fdC's
(define (parse-prog [s : Sexp]) : (Listof fdC)
  (match s
    ['() '()]
    [(cons f r) (cons (parse-fundef f) (parse-prog r))] 
    [other (error 'parse-prog "OAZO Syntax Error: Function with invalid syntax in ~e" other)]))


;; Parse-prog Tests
(check-equal? (parse-prog '{{func {f x} : {+ x 14}}
                            {func {main init} : {f 2}}})
              
              (list (fdC 'f 'x (binopC '+ (idC 'x) (numC 14)))
                    (fdC 'main 'init (appC 'f (numC 2)))))
(check-exn #rx"parse-func-def" (lambda() (parse-prog '{12 {func {main init} : {f 2}}})))
(check-exn #rx"parse-prog" (lambda() (parse-prog '12)))


;; INTERP HELPER FUNCTIONS
;;-----------------------------------------------------------------------------------

;; Helper for searching through the list of funs TODO
(define (get-fundef [n : Symbol] [fds : (Listof fdC)]) : fdC
  (cond
    [(empty? fds) (error 'get-fundef "OAZO Error: Reference to undefined function ~e" n)]
    [(cons? fds) (cond
                   [(equal? n (fdC-name (first fds))) (first fds)]
                   [else (get-fundef n (rest fds))])]))

;; Helper for subsituting identifiers into expressions TODO
(define (sub [what : ExprC] [for : Symbol] [in : ExprC]) : ExprC
  (match in
    [(numC n) in]
    [(idC s) (if (equal? s for) what in)]
    [(binopC op l r) (binopC op (sub what for l) (sub what for r))]
    [(appC f a) (appC f (sub what for (cast a ExprC)))]
    [(ifleq0? test then else) (ifleq0? (sub what for test)
                                       (sub what for then)
                                       (sub what for else))]
    #;[else (error 'sub "OAZO Error: Unknown expression: ~a" what)])) ;; this err occurs elsewhere


;; Sub Tests
(check-equal? (sub (numC 5) 'x (idC 'x))
              (numC 5))
(check-equal? (sub (numC 5) 'x (binopC '+ (idC 'x) (numC 1)))
              (binopC '+ (numC 5) (numC 1)))
(check-equal? (sub (numC 5) 'y (idC 'x))
              (idC 'x))
(check-equal? (sub (numC 5) 'y (binopC '+ (idC 'x) (numC 1)))
              (binopC '+ (idC 'x) (numC 1)))
(check-equal? (sub (numC 5) 'y (appC 'f (idC 'y)))
              (appC 'f (numC 5)))
(check-equal? (sub (numC 5) 'y (ifleq0? (idC 'y) (numC 10) (numC 20)))
              (ifleq0? (numC 5) (numC 10) (numC 20)))


;; INTERP
;;-----------------------------------------------------------------------------------

;; Inteprets the given expression using list of funs to resolve appC's
(define (interp [a : ExprC] [fds : (Listof fdC)]) : Real
  (match a
    [(numC n) n]
    [(idC s) (error 'interp "OAZO Logic Error: Interp of an idC: ~e" s)]
    [(ifleq0? test then else)
     (if (<= (cast (interp test fds) Real) 0)
         (interp then fds)
         (interp else fds))]
    [(binopC op a b) (match op
                       ['+ (+ (interp a fds) (interp b fds))]
                       ['- (- (interp a fds) (interp b fds))]
                       ['* (* (interp a fds) (interp b fds))]
                       ['/ (let ([right-val (interp b fds)])
                            (if (not (= right-val 0))
                                (/ (interp a fds) right-val)
                                (error 'interp "OAZO Arithmetic Error: Division by zero")))])]

    [(appC f a) (define fd (get-fundef f fds))
                (interp (sub (numC (interp (cast a ExprC) fds))
                               (fdC-arg fd)
                               (fdC-body fd))
                        fds)]
    [(appC f (list ? ExprC? ...)) (error 'interp "TEST")]

    #;[else (error 'interp "OAZO Error: Unknown expression: ~e" a)]))   ;; this err occurs elsewhere

;; Interp Tests
(define fds(list (fdC 'f 'x (binopC '+ (idC 'x) (numC 1)))
                 (fdC 'g 'x (binopC '* (idC 'x) (numC 2)))))
(check-equal? (interp (numC 5) fds) 5)
(check-exn #rx"Interp of an idC" (lambda () (interp (idC 'x) fds)))
(check-equal? (interp (binopC '+ (numC 3) (numC 2)) fds) 5)
(check-equal? (interp (binopC '- (numC 3) (numC 2)) fds) 1)
(check-equal? (interp (binopC '* (numC 3) (numC 2)) fds) 6)
(check-equal? (interp (binopC '/ (numC 10) (numC 5)) fds) 2)
(check-equal? (interp (binopC '+ (binopC '* (numC 2) (numC 3)) (numC 4)) fds) 10)
(check-equal? (interp (parse '{ifleq0? 10 3 -1}) fds) -1)
(check-equal? (interp (parse '{ifleq0? -12 3 -1}) fds) 3)
(check-equal? (interp (parse '{f 12}) fds) 13)
(check-exn #rx"OAZO"(lambda() (interp (binopC '/ (numC 10) (numC 0)) fds)))


;; INTERP-FNS
;;-----------------------------------------------------------------------------------

;; Inteprets the function named main from the func definitons
(define (interp-fns [funs : (Listof fdC)]) : Real
  (let ([main-fd (get-fundef 'main funs)])
    (define body (sub (numC 0) 'init (fdC-body main-fd)))
    (interp body funs)))


;; TOP-INTERP
;;-----------------------------------------------------------------------------------

;; Interprets the entirely parsed program TODO
(define (top-interp [program : Sexp]): Real
  (interp-fns (parse-prog program)))

;; Top-Interp Tests
(check-equal? (top-interp
               '{{func {minus-five x} : {+ x {* -1 5}}}
                 {func {main init} : {minus-five {+ 8 init}}}}) 3)

(check-equal? (top-interp
               '{{func {minus-five x} : {+ x {* -1 5}}}
                 {func {main init} : {minus-five {+ 8 init}}}}) 3)

(check-equal? (top-interp
               '{{func {f x} : {+ x 14}}
                 {func {main init} : {f 2}}}) 16)

(check-equal? (top-interp
               '{{func {g x} : {ifleq0? x 0
                                        {+ {g {- x 1}} x}}}
                 {func {main init} : {g 3}}}) 6)


(check-exn #rx"OAZO" (lambda() (top-interp
               '12)))

(check-exn #rx"undefined" (lambda() (top-interp
                                     '{{func {f x} : {+ x 14}}
                                       {func {g y} : {f 2}}})))

(check-exn #rx"OAZO" (lambda() (top-interp
                                '{{func {ignoreit x}: {+ 3 4}}
                                  {func {main init} : {ignoreit {/ 1 {+ 0 0}}}}})))


