#lang plai

(define-type Binding
  [binding (name symbol?) (named-expr CFWAE?)])

(define-type CFWAE
  [num (n number?)]
  [binop (op procedure?) (lhs CFWAE?) (rhs CFWAE?)]
  [with (b Binding?) (body CFWAE?)]
  [id (name symbol?)]
  [if0 (c CFWAE?) (t CFWAE?) (e CFWAE?)]
  [fun (args (listof symbol?)) (body CFWAE?)]
  [app (f CFWAE?) (args (listof CFWAE?))])

(define-type Env
  [mtEnv]
  [anEnv (name symbol?) (value CFWAE-Value?) (env Env?)])

(define-type CFWAE-Value
  [numV (n number?)]
  [thunkV (body CFWAE?) (env Env?)]
  [closureV (param symbol?)
            (body CFWAE?)
            (env Env?)])

;; list of reserved symbols
(define *reserved-symbols* '(with fun if0)) ;; try binop

;; div : number number -> number or error
;; divides first number by the second unless second is 0 where it throws an error
(define (div l r)
  (if (= r 0)
      (error 'div "divide by zero error.")
      (/ l r)))

;; regular divisions
(test (div 2 2) 1)
(test (div 4 2) 2)

;; divide by zero
(test/exn (div 3 0) "")

;; hash table for procedures 
(define *proctable* (hash '+ + '- - '/ div '* *))

;; op-to-proc : symbol -> procedure
;; converts an operator symbol to its associated procedure
(define (op-to-proc op)
  (if (hash-has-key? *proctable* op)
      (hash-ref *proctable* op)
      (error 'op-to-proc "Not a valid operator")))

;; recognized op
(test (op-to-proc '+) +)
;; invalid op
(test/exn (op-to-proc '&) "")

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

;; valid-bind? : listOfSymbol -> Boolean
;; produce true if correctly formed binding
(define (valid-bind? bind)
  (and (= 2 (length bind))
       (valid-identifier? (first bind))))

;;;; REMEMBER TO DISALLOW FUNCTIONS WITH NO ARGS LIKE {fun {} ...} 
;;;; AND APPLICATIONS OF FUNCTIONS TO NO ARGUMENTS LIKE {f}.
;; parse : expression -> CFWAE
;; This procedure parses an expression into a CFWAE
(define (parse sexp)
  (match sexp
    [(? valid-identifier?) (id sexp)]
    [(? number?) (num sexp)]
    [(list (and (? valid-binop?) op) lexp rexp)
     (binop (op-to-proc op) (parse lexp)(parse rexp))]
    [(list 'with bind body-expr)
     (if (valid-bind? bind)
         (with (binding (first bind)(parse (second bind)))(parse body-expr))
         (error 'parse "wrong number of binding args"))]
    [(list 'if0 test-expr then-expr else-expr)
     (if0 (parse test-expr)
          (parse then-expr)
          (parse else-expr))]
    [(list 'fun ids body-expr)
     (if (and (andmap valid-identifier? ids) (not (empty? ids)))
         (fun ids (parse body-expr))
         (error 'parse "invalid function ids or no ids in: ~s" sexp))]
    [(list fun-expr arg-exprs ...)
     (app (parse fun-expr) (map parse arg-exprs))]
    [else (error 'parse "unable to parse the s-expression ~s" sexp)]))

(test (parse '{if0 0 1 2}) (if0 (num 0) (num 1) (num 2)))
(test (parse '{if0 {/ 1 2} 1 2}) (if0 (binop div (num 1) (num 2)) (num 1) (num 2)))
(test (parse '{if0 0 {if0 0 1 2} {if0 2 1 0}}) (if0 (num 0) 
                                                    (if0 (num 0) (num 1) (num 2))
                                                    (if0 (num 2) (num 1) (num 0))))

(test (parse '{fun (x) 0}) (fun '(x) (num 0)))
(test (parse '{fun (x y) 0}) (fun '(x y) (num 0)))
(test (parse '{fun (x) (+ 1 2)}) (fun '(x) (binop + (num 1) (num 2))))

(test (parse '{some-fun 3}) (app (id 'some-fun) (list (num 3))))
(test (parse '{some-fun 1 2 3}) (app (id 'some-fun) (list (num 1) (num 2) (num 3))))
(test (parse '{some-fun 1 {with {x 10} x}}) (app (id 'some-fun) (list (num 1) (with (binding 'x (num 10)) (id 'x)))))

(test (parse '{x {with {x {fun {y} 0}} x} 10}) (app (id 'x) (list (with (binding 'x (fun '(y) (num 0))) (id 'x)) (num 10))))

#|
;; Symbols
(test (parse 'x) (id 'x))
(test (parse 'ys) (id 'ys))

;; Reserved Symbol
(test/exn (parse '+) "")

;; Numbers
(test (parse '3) (num 3))
(test (parse '0) (num 0))

;; Plain arithmetic.
(test (parse '{+ 1 2}) (binop + (num 1) (num 2)))
(test (parse '{- 1 2}) (binop - (num 1) (num 2)))
(test (parse '{* 3 4}) (binop * (num 3) (num 4)))
(test (parse '{/ 3 4}) (binop div (num 3) (num 4)))
(test (parse '{+ {- 1 {/ 2 3}} 4})
      (binop + (binop - (num 1) (binop div (num 2) (num 3)))
             (num 4)))

;; With binding
(test (parse '{with {x 1} x}) (with (binding 'x (num 1)) (id 'x)))
(test (parse '{with {x {with {y 2} {+ x y}}} {with {z 3} {+ x z}}})
      (with (binding 'x (with (binding 'y (num 2)) (binop + (id 'x) (id 'y)))) (with (binding 'z (num 3)) (binop + (id 'x) (id 'z)))))

; non-lists, reserved symbols (e.g., + and -), strings
(test/exn (parse '"hello") "")
(test/exn (parse '+) "")
(test/exn (parse '-) "")
(test/exn (parse 'with) "")

; lists that start with things besides +, -, with or with*, esp. numbers
(test/exn (parse '{hello 1 2}) "")
(test/exn (parse '{"abc"}) "")
(test/exn (parse '{1 2 3}) "")
 definition for lookup, we’d have a full interpreter. So here’s one:
; binop with fewer or more than 2 arguments
(test/exn (parse '{+}) "")
(test/exn (parse '{+ 1}) "")
(test/exn (parse '{+ 1 2 3}) "")

; ill-structured with
(test/exn (parse '{with}) "")
(test/exn (parse '{with x}) "")
(test/exn (parse '{with x 2 3}) "")
(test/exn (parse '{with {x 1}}) "")
(test/exn (parse '{with {x 1} 2 3}) "")
(test/exn (parse '{with {x 1 2} 3}) "")
(test/exn (parse '{with {+ 1} 2}) "")


|#

;; pre-process : CFWAE -> CFWAE
;; Consumes a CFWAE and constructs a corresponding CFWAE without
;; with expressions (which are replaced by function application) 
;; and with no functions or applications of more than one argument.
;; (Assumes the input was successfully produced by parse.)
(define (pre-process expr)
  (type-case CFWAE expr
    [num (n) expr]
    [id (id) expr]
    [binop (op lhs rhs)
           (binop op                  
                  (pre-process lhs)
                  (pre-process rhs))]
    [with (bind body-expr)
          (app (fun (list (binding-name bind))
                    (pre-process body-expr))
               (list (pre-process (binding-named-expr bind))))]
    [if0 (test-expr then-expr else-expr)
         (if0 (pre-process test-expr)
              (pre-process then-expr)
              (pre-process else-expr))]
    [fun (args body-expr) 
         (if (<= (length args) 1)
             (fun args (pre-process body-expr))
             (pre-process (fun (rest args)
                               (fun (list (first args)) body-expr))))]
    [app (fun-expr arg-exprs)
         (cond
           [(empty? arg-exprs) (app (pre-process fun-expr) arg-exprs)]
           [(= 1 (length arg-exprs)) (app (pre-process fun-expr) 
                                          (list (pre-process (first arg-exprs))))]
           [else (app (pre-process (app fun-expr (rest arg-exprs)))
                      (list (pre-process (first arg-exprs))))])]))



(test (pre-process (parse 1)) (num 1))
(test (pre-process (parse 'x)) (id 'x))
(test (pre-process (parse '{+ 3 4})) (binop + (num 3) (num 4)))

(test (pre-process (parse '{if0 0 1 2})) (if0 (num 0) (num 1) (num 2)))

(test (pre-process (parse '{fun (x) 0})) (fun '(x) (num 0)))
(test (pre-process (parse '{fun (x y) 0})) (fun '(y) (fun '(x) (num 0))))

(test (pre-process (app (fun '() (num 0)) '()))
      (app (fun '() (num 0)) '()))
(test (pre-process (app (fun '(x) (num 0)) (list (num 1))))
      (app (fun '(x) (num 0)) (list (num 1))))
; a test to show that we don't care if the fun and the args don't agree
(test (pre-process (app (fun '() (num 0)) (list (num 1))))
      (app (fun '() (num 0)) (list (num 1))))
(test (pre-process 
       (app (fun '(x y z) (if0 (id 'x) (id 'y) (id 'z)))
            (list (num 0) (num 1) (num 2))))
      (app (app (app (fun '(z)
                          (fun '(y)
                               (fun '(x)
                                    (if0 (id 'x) (id 'y) (id 'z)))))
                     (list (num 2)))
                (list (num 1)))
           (list (num 0))))

(test (pre-process (with (binding 'x (num 1)) (binop + (id 'x) (num 2))))
      (app (fun '(x) (binop + (id 'x) (num 2))) (list (num 1))))
#;
(test (pre-process 
       (app (fun '(x y z) (if0 (id 'x) (id 'y) (id 'z)))
            (list (with (binding 'x (num 1)) (binop + (id 'x) (num 2)))
                  (fun '(x) (binop + (id 'x) (num 2)))
                  (num 14))))
      (app (app (app (fun '(x) (fun '(y) (fun '(z) (if0 (id 'x) (id 'y) (id 'z)))))
                     (list (num 14)))
                (list (fun '(x) (binop + (id 'x) (num 2)))))
           (list (with (binding 'x (num 1)) (binop + (id 'x) (num 2))))))

;; lookup : symbol Env -> CFWAE-Value
;; produces the CFWAE-Value bound to the given symbol in the given enviroment
;; produces an error if it does not exist

(define (lookup id env)
  (type-case Env env
    [mtEnv () (error "unbound identifier, ~s!" id)]
    [anEnv (name value rest-env) 
           (if (symbol=? id name)
               value
               (lookup id rest-env))]))

(test/exn (lookup 'x (mtEnv)) "")
(test (lookup 'x (anEnv 'x (numV 5) (mtEnv))) (numV 5))
(test (lookup 'x (anEnv 'y (numV 5) (anEnv 'x (numV 0) (mtEnv)))) (numV 0))

;; run : sexp -> CFWAE-Value
;; Consumes an sexp and passes it through parsing, pre-processing,
;; and then interpretation to produce a result.
(define (run sexp)
  (interp (pre-process (parse sexp))))

;; interp : CFWAE -> CFWAE-Value
;; This procedure interprets the given CFWAE and produces a result 
;; in the form of a CFWAE-Value (either a closureV, thunkV, or numV).
;; (Assumes the input was successfully produced by pre-process.)
(define (interp expr)
  (local [(define (helper expr env)
            (type-case CFWAE expr
              [num (n) (numV n)]
              [binop (op lhs rhs)
                     (local [(define helped-lhs (helper lhs env))
                             (define helped-rhs (helper rhs env))]
                       (if (and (numV? helped-lhs) (numV? helped-rhs))
                           (numV (op (numV-n helped-lhs)
                                     (numV-n helped-rhs)))
                           (error "trying to perform a binary operation on non-numeric values")))]
              [if0 (cond-expr then-expr else-expr)
                   (local [(define cond-val (helper cond-expr env))]
                     (if (numV? cond-val)
                         (if (= 0 (numV-n (helper cond-expr env)))
                             (helper then-expr env)
                             (helper else-expr env))
                         (error "non-numeric condition value")))]
              [fun (args body-expr)
                   (if (empty? args)
                       (thunkV body-expr env)
                       (closureV (first args) body-expr env))]
              [app (fun-expr arg-exprs)
                   (local [(define fun-val (helper fun-expr env))]
                     (type-case CFWAE-Value fun-val
                       [thunkV (body thunk-env) (if (empty? arg-exprs)
                                                    (helper body thunk-env)
                                                    (error "too many arguments"))]
                       [closureV (param body closure-env)
                                 (if (empty? arg-exprs)
                                     (error "too few arguments")
                                     (helper body
                                             (anEnv param
                                                    (helper (first arg-exprs) env)
                                                    closure-env)))]
                       [numV (n) (error 'interp "invalid function expression: ~s" n)]))]
              [else (error "interpretor error")]))]
    (helper expr (mtEnv))))

;(numV-n (interp (pre-process
;                 (with (binding (quote apply)
;                             (first arg-exprs   (fun (quote (f x y)) (app (id (quote f)) (list (id (quote x)) (id (quote y))))))
;                       (app (id (quote apply))
;                            (list (fun (quote (a b)) (binop + (id (quote a)) (id (quote b))))
;                                  (num 3) 
;                                  (num 4)))))))
;
;(run '(with (apply (fun (f x y) (f x y)))
;      (apply (fun (a b) (+ a b))
;             3
;             4)))
;
;(numV-n (interp (pre-process (with
;                              (binding 'apply (fun '(f x y)
;                                                   (app (id 'f) (list (id 'x) (id 'y)))))
;                              (app (id 'apply) (list (fun '(a b) (binop + (id 'a) (id 'b))) (num 3) (num 4)))))))
;
;(run '{with {apply {fun {f x y}
;                  {f x y}}}
;      {apply {fun {a b} {+ a b}}
;             3 4}})

;(run '(+ ((fun (x y) (- x y)) 4 3)
;         ((fun (x y) (* x y)) 2 3)))

;; testing num
(test (run '3) (numV 3))

;; testing binop
(test (run '(+ 3 4)) (numV 7))

(test/exn (run '(3 4)) "")
(test (run '(with (x (+ 3 4)) x)) (numV 7))
(test (run '(with (add (fun (x y) (+ x y))) (add 3 4))) (numV 7))

(test (interp (if0 (num 0) (num 1) (num 2))) (numV 1))
;(test (interp (if0 (thunkV 'x) (num 1) (num 2))) (numV 2)) <-- Thunks? Closures? 
(test (interp (if0 (num 0) (binop + (num 1) (num 2)) (binop + (num 3) (num 4)))) (numV 3))
(test (interp (if0 (binop + (num 1) (num 2)) (num 1) (num 2))) (numV 2))

(test (interp (fun (list 'x) (id 'x))) (closureV 'x (id 'x) (mtEnv)))
(test (interp (fun empty (id 'x))) (thunkV (id 'x) (mtEnv)))




;; Possibly useful additional functions:

;; failed-tests : -> (listof plai-test-result)
;; Generates a list of only the failed (non-good) tests from plai-all-test-results.
(define (failed-tests)
  (reverse (filter (compose not (curry symbol=? 'good) first) plai-all-test-results)))

;; CFWAE-pre-fold : (CFWAE -> CFWAE) CFWAE -> CFWAE
;; Takes a function and applies it to each expression node in the 
;; given CFWAE.  Note that the function is applied pre-order; so
;; it is applied to a node before its sub-trees.  WARNING: if
;; your function generates a new node that itself needs to be
;; re-processed through the function, CFWAE-pre-fold will not do
;; so.  (It calls f on a node and then recurses into any sub-nodes
;; of whatever node f returns.  It does not reprocess the node 
;; itself.)
(define (CFWAE-pre-fold f expr)
  (local ([define (ffold expr)
            (type-case CFWAE (f expr)
              [num (n) (num n)]
              [binop (op lhs rhs) (binop op (ffold lhs) (ffold rhs))]
              [with (b body) (with (binding (binding-name b)
                                            (ffold (binding-named-expr b)))
                                   (ffold body))]
              [id (name) (id name)]
              [if0 (c t e) (if0 (ffold c) (ffold t) (ffold e))]
              [fun (args body) (fun args (ffold body))]
              [app (f args) (app (ffold f) (map ffold args))])])
    (ffold expr)))


; swap-op-args : CFWAE -> CFWAE
; Consumes a program and generates the corresponding program in which
; each instance of a binop has had its lhs and rhs swapped.
(define (swap-op-args program)
  (CFWAE-pre-fold (lambda (exp)
                    (type-case CFWAE exp
                      [binop (op lhs rhs) (binop op rhs lhs)]
                      [else exp]))
                  program))

(test (swap-op-args (parse '{+ 1 2})) (parse '{+ 2 1}))
(test (swap-op-args (parse '{+ 3 {- {* 1 2} {/ 3 4}}}))
      (parse '{+ {- {/ 4 3} {* 2 1}} 3}))
(test (swap-op-args (parse '{fun {x} {+ x {if0 0 {+ 1 2} 3}}}))
      (parse '{fun {x} {+ {if0 0 {+ 2 1} 3} x}}))


