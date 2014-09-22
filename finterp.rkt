#lang plai

(define-type Binding
  [binding (name symbol?) (named-expr WAE?)])

(define-type WAE
  [num (n number?)]
  [binop (op procedure?) (lhs WAE?) (rhs WAE?)]
  [with (b Binding?) (body WAE?)]
  [with* (lob (listof Binding?)) (body WAE?)]
  [id (name symbol?)])

;; parse : s-exp -> WAE
;; Consumes an s-expression and generates the corresponding WAE
(define (parse sexp)
  (num 0))

;; interp : WAE -> number
;; Consumes a WAE representation of an expression and computes
;; the corresponding numerical result
(define (interp expr)
  0)




;; Here are some test cases to get you started!


;;; parse tests

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





;; P.S. Steve's favorite programming language keyword is "catch"
;; because his first programming assignment at the college level was
;; about fishing, and so he and half the class named a variable
;; "catch" and spent hours debugging novice-incomprehensible error
;; messages as a result.
