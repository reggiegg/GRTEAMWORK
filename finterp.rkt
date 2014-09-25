#lang plai

(define-type Binding
  [binding (name symbol?) (named-expr WAE?)])

(define-type WAE
  [num (n number?)]
;  [add (lhs WAE?)    
;       (rhs WAE?)]
;  [sub (lhs WAE?)
;       (rhs WAE?)]
  [binop (op procedure?) (lhs WAE?) (rhs WAE?)]
  [with (b Binding?) (body WAE?)]
  [with* (lob (listof Binding?)) (body WAE?)]
  [id (name symbol?)])

;; list of reserved symbols
(define *reserved-symbols* '(with))


;; div : number number -> number or error
;; divides first number by the second unless second is 0 where it throws an error
(define (div l r)
  (if (= r 0)
      (error "divide by zero error.")
      (/ l r)))

;; regular divisions
(test (div 2 2) 1)
(test (div 4 2) 2)

;; divide by zero
(test/exn (div 3 0) "")
      
      

;; hash table for procedures 
(define *proctable* (hash '+ + '- - '/ div '* *))

;; op-to-proc : symbol -> procedure
(define (op-to-proc op)
  (hash-ref *proctable* op))


;; valid-identifier? : any -> boolean
;; Determines whether the parameter is valid as an identifier name, i.e.,
;; a symbol that is not reserved.
(define (valid-identifier? sym)
  (and (symbol? sym)
       (and (not (member sym *reserved-symbols*))
            (not (member sym (sequence->list (in-hash-keys *proctable*)))))))

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



;; valid-binop? : any -> boolean
;; Determines whether the operator given is valid as a binary operator
(define (valid-binop? op)
  (and (symbol? op)
       (not (not (member op (sequence->list (in-hash-keys *proctable*)))))))

;; OK
(test (valid-binop? '+) true)
(test (valid-binop? '-) true)
(test (valid-binop? '/) true)

;; Not a symbol
(test (valid-binop? '8) false)
(test (valid-binop? '{+ 1 2}) false)

;; Other operators
(test (valid-binop? 'with) false)

;; parse : s-exp -> WAE
;; Consumes an s-expression and generates the corresponding WAE
(define (parse sexp)
  (match sexp
    [(? valid-identifier?) (id sexp)]
    [(? number?) (num sexp)]
;    [(list '+ lexp rexp) (add (parse lexp) (parse rexp))]
;    [(list '- lexp rexp) (sub (parse lexp) (parse rexp))]
    [(list (and (? valid-binop?) op) lexp rexp)
     (binop (op-to-proc op) (parse lexp)(parse rexp))]
    [(list 'with bind body-expr)
     (if (valid-bind? bind)
         (with (binding (first bind)(parse (second bind)))(parse body-expr))
         (error 'parse "wrong number of binding args"))]
    [(list 'with* bindings body-expr)
     (if (andmap valid-bind? bindings)
         (with* (map (λ (b)
                       (binding (first b)
                                (parse (second b)))) bindings)
                (parse body-expr))
         (error 'parse "binding list is erroneous."))]
    [else (error 'parse "unable to parse the s-expression ~s" sexp)]))

;; valid-bind? : listOfSymbol -> Boolean
;; produce true if correctly formed binding
(define (valid-bind? bind)
  (and (= 2 (length bind))
       (valid-identifier? (first bind))))



; subst : WAE symbol WAE -> WAE
;; substitute the val-expr for all FREE instances of id in the body
(define (subst val-expr i body)
  (type-case WAE body
    [num (n) body]
    [binop (op lhs rhs)
           (binop op (subst val-expr i lhs)
                  (subst val-expr i rhs))]
;    [add (lhs rhs) (add (subst val-expr i lhs)
;                        (subst val-expr i rhs))]
;    [sub (lhs rhs) (sub (subst val-expr i lhs)
;                        (subst val-expr i rhs))]
    [with (bind b-e)
          ;; Check whether there are free instances
          ;; in the body at all (which there aren't
          ;; if i-with matches i!).
          (if (symbol=? (binding-name bind) i)
              (with (binding (binding-name bind)
                             (subst val-expr i (binding-named-expr bind)))
                    b-e)
              (with (binding (binding-name bind)
                             (subst val-expr i (binding-named-expr bind)))
                    (subst val-expr i b-e)))]
    [id (name) (if (symbol=? name i)
                   val-expr
                   body)]))

(test (subst (num 5) 'x (num 0)) (num 0))
(test (subst (num 5) 'x (id 'x)) (num 5))
(test (subst (num 5) 'x (id 'y)) (id 'y))
(test (subst (num 5) 'x (binop + (id 'x) (id 'x))) (binop + (num 5) (num 5)))
(test (subst (num 5) 'x (binop - (id 'x) (id 'x))) (binop - (num 5) (num 5)))
(test (subst (num 5) 'x (with (binding 'y (num 10)) (binop + (id 'x)(id 'y))))
      (with (binding 'y (num 10)) (binop + (num 5) (id 'y))))
(test (subst (num 5) 'x (with (binding 'y (binop + (id 'x) (num 1))) (binop + (id 'x) (id 'y))))
      (with (binding 'y (binop + (num 5) (num 1))) (binop + (num 5) (id 'y))))


#|

;; interp : WAE -> number
;; Consumes a WAE representation of an expression and computes
;; the corresponding numerical result
(define (interp expr)
  (type-case WAE expr
    [num (n) n]
    [binop (op l r) (op (interp l) (interp r))]
;    [add (l r) (+ (interp l) (interp r))]
;    [sub (l r) (- (interp l) (interp r))]
    [with (bind body)
          (interp (subst (num (interp (binding-named-expr bind)))
                         (binding-name bind)
                         body))]
    [id (name)
        (error 'interp "Unbound identifier ~s." name)]))

|#


#|
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
(test (parse '{with {x 1} x}) (with (binding 'x (num 1)) (id 'x)))

(test (parse '{with {x {with {y 2} {+ x y}}} {with {z 3} {+ x z}}})
      (with (binding 'x (with (binding 'y (num 2)) (add (id 'x) (id 'y)))) (with (binding 'z (num 3)) (add (id 'x) (id 'z)))))

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



;; Test each type of expression.
;; Note: no need for complicated recursive expressions to verify that
;; you're descending into the appropriate sub-exprs, since even numbers
;; and symbols must be parsed.  (Unless you believe for some reason you're
;; only descending "one level down".)
|#

(test (parse '1) (num 1))
(test (parse 'x) (id 'x))
(test (parse '{+ 1 1}) (binop + (num 1) (num 1)))
(test (parse '{- 2 1}) (binop - (num 2) (num 1)))
(test (parse '{* 1 5}) (binop * (num 1) (num 5)))
(test/exn (parse '{- 2 1 4}) "")
(test (parse '{with {x 1} x}) (with (binding 'x (num 1)) (id 'x)))
(test (parse '{with* {} 4}) (with* '() (num 4)))
(test (parse '{with* {{x 1}} 4}) (with* (list (binding 'x (num 1))) (num 4)))
(test (parse '{with* {{x 1} {y 2}} x}) (with* (list (binding 'x (num 1))(binding 'y (num 2))) (id 'x)))


#|
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
(test (interp (parse '{/ 4 2})) 2)
(test/exn (interp (parse '{/ 1 0})) "")
(test (interp (parse '{with {x 1} x})) 1)
(test (interp (parse '{with {x 5} 0})) 0)
(test (interp (parse '{with {x 5} x})) 5)
(test (interp (parse '{with {x {+ 1 2}} x})) 3)
(test (interp (parse '{with {x 5} {+ x x}})) 10)
(test (interp (parse '{with {x 5} {* x x}})) 25)
(test (interp (parse '{with {x 5} {with {y 10} {+ x y}}})) 15)
(test (interp (parse '{with {x 5} {with {y {+ x 1}} {+ x y}}})) 11)
(test/exn (interp (parse '{with {x x} x})) "")
(test/exn (interp (parse '{with {x 5} {with {y {+ y 1}} {+ x y}}})) "")
(test (interp (parse '{with {x 5} {with {x 6} {+ x x}}})) 12)
(test (interp (parse '{with {x 5} {with {x {+ x 1}} {+ x x}}})) 12)

(test/exn (interp (parse 'x)) "")
(test/exn (interp (parse 'lambda-bound)) "")






;; P.S. Steve's favorite programming language keyword is "catch"
;; because his first programming assignment at the college level was
;; about fishing, and so he and half the class named a variable
;; "catch" and spent hours debugging novice-incomprehensible error
;; messages as a result.

|#
