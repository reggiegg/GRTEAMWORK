#lang plai


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; LANGUAGE DEFINITION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Now, let's talk about identifiers and substitution.

; WAE ::= <num>
;      | { + <WAE> <WAE> }
;      | { - <WAE> <WAE> }
;      | { with { <id> <WAE> } <WAE> }
;      | <id>

(define-type WAE     
  [num (n number?)]
  [add (lhs WAE?)    
       (rhs WAE?)]
  [sub (lhs WAE?)
       (rhs WAE?)]
  [with (id symbol?) (named-expr WAE?) (body WAE?)]
  [id  (name symbol?)])

;; We'll jump through parsers quickly, starting now, but do note the
;; handy use of "match" for our parser!

(define *reserved-symbols* '(+ - with))

;; valid-identifier? : any -> boolean
;; Determines whether the parameter is valid as an identifier name, i.e.,
;; a symbol that is not reserved.
(define (valid-identifier? sym)
  (and (symbol? sym)
       (not (member sym *reserved-symbols*))))

;; Reserved symbols.
(test (valid-identifier? '+) false)
(test (valid-identifier? '-) false)
(test (valid-identifier? 'with) false)

;; Not a symbol
(test (valid-identifier? '{+ 1 2}) false)
(test (valid-identifier? 3) false)
(test (valid-identifier? "id") false)

;; OK
(test (valid-identifier? 'id) true)
(test (valid-identifier? 'x) true)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PARSING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; parse : any -> AE
;; Consumes an s-expression (in AE's concrete syntax)
;; and generates the corresponding AE program.
(define (parse sexp)
  (match sexp
    [(? valid-identifier?) (id sexp)]
    [(? number?) (num sexp)]
    [(list '+ lexp rexp) (add (parse lexp) (parse rexp))]
    [(list '- lexp rexp) (sub (parse lexp) (parse rexp))]
    [(list 'with (list (and (? valid-identifier?) id) binding-expr) body-expr)
     (with id (parse binding-expr) (parse body-expr))]
    [else (error 'parse "unable to parse the s-expression ~s" sexp)]))


(test (parse 'x) (id 'x))
(test (parse 'ys) (id 'ys))
(test/exn (parse '+) "")

;; Numbers
(test (parse '3) (num 3))
(test (parse '0) (num 0))

;; Plain arithmetic.
(test (parse '{+ 1 2}) (add (num 1) (num 2)))
(test (parse '{- 1 2}) (sub (num 1) (num 2)))
(test (parse '{+ 3 4}) (add (num 3) (num 4)))
(test (parse '{- 3 4}) (sub (num 3) (num 4)))

(test (parse '{+ {- 1 {+ 2 3}} 4})
      (add (sub (num 1) (add (num 2) (num 3)))
           (num 4)))

;; With binding
(test (parse '{with {x 1} x}) (with 'x (num 1) (id 'x)))

(test (parse '{with {x {with {y 2} {+ x y}}} {with {z 3} {+ x z}}})
      (with 'x (with 'y (num 2) (add (id 'x) (id 'y)))
            (with 'z (num 3) (add (id 'x) (id 'z)))))

;; Error checking

; non-lists, reserved symbols (e.g., + and -), strings
(test/exn (parse '"hello") "")
(test/exn (parse '+) "")
(test/exn (parse '-) "")
(test/exn (parse 'with) "")


; lists that start with things besides +, -, or with, esp. numbers
(test/exn (parse '{hello 1 2}) "")
(test/exn (parse '{"abc"}) "")
(test/exn (parse '{1 2 3}) "")

; + with fewer or more than 2 arguments
(test/exn (parse '{+}) "")
(test/exn (parse '{+ 1}) "")
(test/exn (parse '{+ 1 2 3}) "")

; - with fewer or more than 2 arguments
(test/exn (parse '{-}) "")
(test/exn (parse '{- 1}) "")
(test/exn (parse '{- 1 2 3}) "")

; ill-structured with
(test/exn (parse '{with}) "")
(test/exn (parse '{with x}) "")
(test/exn (parse '{with x 2 3}) "")
(test/exn (parse '{with {x 1}}) "")
(test/exn (parse '{with {x 1} 2 3}) "")
(test/exn (parse '{with {x 1 2} 3}) "")
(test/exn (parse '{with {+ 1} 2}) "")

; + (and -/with) with non-AEs as arguments
(test/exn (parse '{+ "a" 3}) "")
(test/exn (parse '{- 1 "b"}) "")
(test/exn (parse '{+ {- 12 #\c} 8}) "")
(test/exn (parse '{with {x "foo"} x}) "")
(test/exn (parse '{with {x 1} "foo"}) "")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; INTERPRETATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; subst : WAE symbol WAE -> WAE
;; substitute the val-expr for all FREE instances of id in the body
(define (subst val-expr i body)
  (type-case WAE body
    [num (n) body]
    [add (lhs rhs) (add (subst val-expr i lhs)
                        (subst val-expr i rhs))]
    [sub (lhs rhs) (sub (subst val-expr i lhs)
                        (subst val-expr i rhs))]
    [with (i-with n-e b-e)
          ;; Check whether there are free instances
          ;; in the body at all (which there aren't
          ;; if i-with matches i!).
          (if (symbol=? i-with i)
              (with i-with
                (subst val-expr i n-e)
                b-e)
              (with i-with
                (subst val-expr i n-e)
                (subst val-expr i b-e)))]
    [id (name) (if (symbol=? name i)
                   val-expr
                   body)]))
  
(test (subst (num 5) 'x (num 0)) (num 0))
(test (subst (num 5) 'x (id 'x)) (num 5))
(test (subst (num 5) 'x (id 'y)) (id 'y))
(test (subst (num 5) 'x (add (id 'x) (id 'x))) (add (num 5) (num 5)))
(test (subst (num 5) 'x (sub (id 'x) (id 'x))) (sub (num 5) (num 5)))
(test (subst (num 5) 'x (with 'y (num 10) (add (id 'x) (id 'y))))
      (with 'y (num 10) (add (num 5) (id 'y))))
(test (subst (num 5) 'x (with 'y (add (id 'x) (num 1)) (add (id 'x) (id 'y))))
      (with 'y (add (num 5) (num 1)) (add (num 5) (id 'y))))

;(test/exn (interp (parse '{with {x x} x})) "")
;(test/exn (interp (parse '{with {x 5} {with {y {+ y 1}} {+ x y}}})) "")
;(test (interp (parse '{with {x 5} {with {x 6} {+ x x}}})) 12)
;(test (interp (parse '{with {x 5} {with {x {+ x 1}} {+ x x}}})) 12)

;; interp : WAE -> number
;; consumes a WAE and computes the corresponding number
(define (interp an-ae)
  (type-case WAE an-ae
    [num (n) n]
    [add (l r) (+ (interp l) (interp r))]
    [sub (l r) (- (interp l) (interp r))]
    [with (id named-e body) 
          (interp (subst (num (interp named-e)) id body))]
    [id (name)
        (error 'interp "Unbound identifier ~s." name)]))



(test (interp (parse '3)) 3)
(test (interp (parse '{+ 3 4})) 7)
(test (interp (parse '{- 3 4})) -1)
(test (interp (parse '{+ {- 3 4} 7})) 6)
(test (interp (parse '{+ 7 {- 3 4}})) 6)


;; Tricky with cases (and non-tricky)
(test (interp (parse '{with {x 5} 0})) 0)
(test (interp (parse '{with {x 5} x})) 5)
(test (interp (parse '{with {x {+ 1 2}} x})) 3)
(test (interp (parse '{with {x 5} {+ x x}})) 10)
(test (interp (parse '{with {x 5} {with {y 10} {+ x y}}})) 15)
(test (interp (parse '{with {x 5} {with {y {+ x 1}} {+ x y}}})) 11)
(test/exn (interp (parse '{with {x x} x})) "")
(test/exn (interp (parse '{with {x 5} {with {y {+ y 1}} {+ x y}}})) "")
(test (interp (parse '{with {x 5} {with {x 6} {+ x x}}})) 12)
(test (interp (parse '{with {x 5} {with {x {+ x 1}} {+ x x}}})) 12)

(test/exn (interp (parse 'x)) "")
(test/exn (interp (parse 'lambda-bound)) "")

;; Does it matter whether we evaluate the named expression in a 
;; "with" right away or later on when the identifier is referenced?
;;
;; Determines the difference between lazy and eager evaluation.
;; Eager produces an error (unbound identifier y).
;; Lazy does not. (Evaluates to 5.)
(test/exn (interp (parse '{with {x y} 5})) "")

























;; EAGER evaluation is when we evaluate an expression BEFORE binding it to an identifier.
;; So, identifiers are bound to values.  In our little language, this only happens in a with,
;; but one could imagine it in function application as well.

;; But, what if we instead wait to evaluate the expression? Could we just bind the expression
;; to the identifier? What would happen then?
;;
;; This is called LAZY evaluation.


































;; CAN WE TELL THE DIFFERENCE between these two in our language?
;; How?
;;
;; What could we add to the language so that the programmer can tell the difference
;; even in very small programs?
