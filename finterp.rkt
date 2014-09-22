#lang plai

(define-type Binding
  [binding (name symbol?) (named-expr WAE?)])

(define-type WAE
  [num (n number?)]
  [add (lhs WAE?)    
       (rhs WAE?)]
  [sub (lhs WAE?)
       (rhs WAE?)]
  #;[binop (op procedure?) (lhs WAE?) (rhs WAE?)]
  [with (b Binding?) (body WAE?)]
  #;[with* (lob (listof Binding?)) (body WAE?)]
  [id (name symbol?)])

;; list of reserved symbols
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

;; parse : s-exp -> WAE
;; Consumes an s-expression and generates the corresponding WAE
(define (parse sexp)
  (match sexp
    [(? valid-identifier?) (id sexp)]
    [(? number?) (num sexp)]
    [(list '+ lexp rexp) (add (parse lexp) (parse rexp))]
    [(list '- lexp rexp) (sub (parse lexp) (parse rexp))]
    [(list 'with (list (and (? valid-identifier?) id) binding-expr) body-expr)
     (with id (parse binding-expr) (parse body-expr))]
    [else (error 'parse "unable to parse the s-expression ~s" sexp)]))


;; interp : WAE -> number
;; Consumes a WAE representation of an expression and computes
;; the corresponding numerical result
(define (interp expr)
  0)




;; Here are some test cases to get you started!


;;; parse tests


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

#|

;; Test each type of expression.
;; Note: no need for complicated recursive expressions to verify that
;; you're descending into the appropriate sub-exprs, since even numbers
;; and symbols must be parsed.  (Unless you believe for some reason you're
;; only descending "one level down".)


(test (parse '1) (num 1))
(test (parse 'x) (id 'x))
(test (parse '{+ 1 1}) (binop + (num 1) (num 1)))
(test (parse '{with {x 1} x}) (with (binding 'x (num 1)) (id 'x)))

; One extra with test, because it might be handy in interp.
(test (parse '{with {x 1} {with {x 2} x}}) 
      (with (binding 'x (num 1)) (with (binding 'x (num 2)) (id 'x))))

; For with*, the same idea but also test with 0, 1, and 2 bindings and
; throw in a test the same identifier bound twice, since it might be
; handy to have around for interp (though should be nothing special for parse).
(test (parse '{with* {} 1})
      (with* '() (num 1)))
(test (parse '{with* {{x 1}} x})
      (with* (list (binding 'x (num 1))) (id 'x)))
(test (parse '{with* {{x 1} {y 1}} {+ x y}}) 
      (with* (list (binding 'x (num 1)) 
                   (binding 'y (num 1))) 
             (binop + (id 'x) (id 'y))))
(test (parse '{with* {{x 1}
                      {x {+ x 1}}}
                     x})
             (with* (list (binding 'x (num 1))
                          (binding 'x (binop + (id 'x) (num 1))))
                    (id 'x)))

;; You might wonder why we don't test all our binops.  That's because we have
;; a helper function that looks up an operator symbol in our operator table,
;; and it already has sufficient test cases for us to be confident about its
;; behaviour.



;;; A few--too few!--interp tests.

(test (interp (num 100)) 100)
(test (interp (parse '{+ 1 2})) 3)
(test (interp (parse '{with {x 1} x})) 1)

|#



;; P.S. Steve's favorite programming language keyword is "catch"
;; because his first programming assignment at the college level was
;; about fishing, and so he and half the class named a variable
;; "catch" and spent hours debugging novice-incomprehensible error
;; messages as a result.
