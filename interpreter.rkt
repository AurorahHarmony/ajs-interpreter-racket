#lang eopl
;;;
;;;========== Interpreter for AJS-lang ==========
;;;

(require "ajslang.rkt")
(require "data-structures.rkt")

(provide value-of-program)

;;; The initial environment (currently unused since no variables)
(define init-env empty-env)

;;; value-of-program : Program -> ExpVal
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (let ([result (value-of-stmt-list body (init-env))])
                   (cases inter-result result
                     [return-result (env ret-val)
                                    (cases ret-option ret-val
                                      [return-some (val) val]
                                      [return-none (val) val]
                                      [return-end-of-stmts (undefined-val)])]
                     )
                   )))))

;;; value-of-stmt-list : StmtList Env -> ExpVal
(define value-of-stmt-list
  (lambda (statements env)
    (cases statement-list statements
      [stmt-list-empty () (return-result env (return-end-of-stmts))]
      [stmt-list (stmt rest)
                 (let ([result (value-of-stmt stmt env)])
                   (cases inter-result result
                     [return-result (new-env ret-val)
                                    (cases ret-option ret-val
                                      [return-some (expval) result]
                                      [return-none (expval)
                                                   (let ([rest-result (value-of-stmt-list rest new-env)])
                                                     (cases inter-result rest-result
                                                       [return-result (rest-env rest-ret-val)
                                                                      (cases ret-option rest-ret-val
                                                                        [return-some (rest-expval) rest-result]
                                                                        [return-none (rest-expval) rest-result]
                                                                        [return-end-of-stmts () result])]))]
                                      [return-end-of-stmts () result]) ;; Should not occur, generally
                                    ]
                     ))]
      )))

;;; value-of-stmt : Stmt Env -> ExpVal
;;; Returns an updated env
(define value-of-stmt
  (lambda (stmt env)
    (cases statement stmt
      [const-declaration (id expr)
                         (let ([val (value-of-expr-stmt expr env)])
                           (return-result (extended-env id val env) (return-none (undefined-val))))]
      [func-declaration (id params block)
                        (let* (
                               [param-list (id-list->symbols params)]
                               [stmt-list (unwrap-block block)]
                               [func (proc-val (procedure param-list stmt-list env))])
                          (return-result (extended-env id func env) (return-none (undefined-val))))]
      [return-stmt (expr)
                   (let ([val (value-of-expr-stmt expr env)])
                     (return-result env (return-some val)))]
      [expr-stmt (expr)
                 (let ([val (value-of-expr-stmt expr env)])
                   (return-result env (return-none val)))]
      )))

;; Extracts a list of sybols from an ID list
(define id-list->symbols
  (lambda (ids)
    (cases identifier-list ids
      [id-list-empty () '()]
      [id-list (id tail) (cons id (id-list-tail->symbols tail))])))

(define id-list-tail->symbols
  (lambda (ids)
    (cases identifier-list-tail ids
      [id-list-tail-empty () '()]
      [id-list-tail (id tail) (cons id (id-list-tail->symbols tail))])))

;; Unwraps a statement-list from within a block, for use in storing function definitions
(define unwrap-block
  (lambda (blk)
    (cases block blk
      [block-stmt (statement-list) statement-list])))

;;; value-of-expr : Expr Env -> ExpVal
(define value-of-expr-stmt
  (lambda (expr env)
    (cases expression expr
      ; (binop-expr (lhs rhs) (value-of-add-tail (value-of-mul-expr lhs env) rhs env)))))
      [cond-expr (expr) (value-of-conditional-expr expr env)]
      )))

(define value-of-conditional-expr
  (lambda (expr env)
    (cases conditional-expression expr
      (conditional-expr (logical-or-expr cond-tail)
                        (let ([test-val (value-of-logical-or-expr logical-or-expr env)])
                          (value-of-conditional-tail test-val cond-tail env))))))

(define value-of-conditional-tail
  (lambda (test-val cond-tail env)
    (cases conditional-expression-tail cond-tail
      (cond-expr-tail (true-expr false-expr)
                      (if (expval->boolean test-val)
                          (value-of-expr-stmt true-expr env)
                          (value-of-conditional-expr false-expr env)))
      (cond-expr-tail-empty ()
                            test-val))))

(define value-of-logical-or-expr
  (lambda (expr env)
    (cases logical-or-expression expr
      (logical-or (lhs rhs)
                  (value-of-logical-or-tail (value-of-logical-and-expr lhs env) rhs env)))))

(define value-of-logical-or-tail
  (lambda (lhs-val tail env)
    (cases logical-or-tail tail
      (or-expr-tail (rhs tail-rest)
                    (if (expval->boolean lhs-val)
                        lhs-val  ; Short-circuit evaluation
                        (value-of-logical-or-tail (value-of-logical-and-expr rhs env) tail-rest env)))
      (logical-or-tail-empty ()
                             lhs-val))))

(define value-of-logical-and-expr
  (lambda (expr env)
    (cases logical-and-expression expr
      (logical-and-expr (lhs rhs)
                        (value-of-logical-and-tail (value-of-equality-expr lhs env) rhs env)))))

(define value-of-logical-and-tail
  (lambda (lhs-val tail env)
    (cases logical-and-tail tail
      (and-expr-tail (rhs tail-rest)
                     (if (not (expval->boolean lhs-val))
                         lhs-val  ; Short-circuit evaluation
                         (value-of-logical-and-tail (value-of-equality-expr rhs env) tail-rest env)))
      (logical-and-tail-empty ()
                              lhs-val))))

(define value-of-equality-expr
  (lambda (expr env)
    (cases equality-expression expr
      (equality-expr (lhs tail)
                     (value-of-equality-tail (value-of-relational-expr lhs env) tail env)))))

(define value-of-equality-tail
  (lambda (lhs-val tail env)
    (cases equality-tail tail
      (seq-expr-tail (rhs tail-rest)
                     (let* ([rhs-val (value-of-relational-expr rhs env)]
                            [result (bool-val (expval-strict-equals? lhs-val rhs-val))])
                       (value-of-equality-tail result tail-rest env)))
      (sneq-expr-tail (rhs tail-rest)
                      (let* ([rhs-val (value-of-relational-expr rhs env)]
                             [result (bool-val (not (expval-strict-equals? lhs-val rhs-val)))])
                        (value-of-equality-tail result tail-rest env)))
      (eq-expr-tail (rhs tail-rest)
                    (let* ([rhs-val (value-of-relational-expr rhs env)]
                           [result (bool-val (expval-equals? lhs-val rhs-val))])
                      (value-of-equality-tail result tail-rest env)))
      (neq-expr-tail (rhs tail-rest)
                     (let* ([rhs-val (value-of-relational-expr rhs env)]
                            [result (bool-val (not (expval-equals? lhs-val rhs-val)))])
                       (value-of-equality-tail result tail-rest env)))
      (equality-tail-empty ()
                           lhs-val))))

(define value-of-relational-expr
  (lambda (expr env)
    (cases relational-expression expr
      (relational-expr (lhs tail)
                       (value-of-relational-tail (value-of-additive-expr lhs env) tail env)))))

(define value-of-relational-tail
  (lambda (lhs-val tail env)
    (cases relational-tail tail
      (lt-expr-tail (rhs tail-rest)
                    (let* ([rhs-val (value-of-additive-expr rhs env)]
                           [lhs-num (expval->num lhs-val)]
                           [rhs-num (expval->num rhs-val)]
                           [result (bool-val (< lhs-num rhs-num))])
                      (value-of-relational-tail result tail-rest env)))
      (gt-expr-tail (rhs tail-rest)
                    (let* ([rhs-val (value-of-additive-expr rhs env)]
                           [lhs-num (expval->num lhs-val)]
                           [rhs-num (expval->num rhs-val)]
                           [result (bool-val (> lhs-num rhs-num))])
                      (value-of-relational-tail result tail-rest env)))
      (le-expr-tail (rhs tail-rest)
                    (let* ([rhs-val (value-of-additive-expr rhs env)]
                           [lhs-num (expval->num lhs-val)]
                           [rhs-num (expval->num rhs-val)]
                           [result (bool-val (<= lhs-num rhs-num))])
                      (value-of-relational-tail result tail-rest env)))
      (ge-expr-tail (rhs tail-rest)
                    (let* ([rhs-val (value-of-additive-expr rhs env)]
                           [lhs-num (expval->num lhs-val)]
                           [rhs-num (expval->num rhs-val)]
                           [result (bool-val (>= lhs-num rhs-num))])
                      (value-of-relational-tail result tail-rest env)))
      (relational-tail-empty ()
                             lhs-val))))

(define value-of-additive-expr
  (lambda (expr env)
    (cases additive-expression expr
      (additive-expr (lhs tail)
                     (value-of-add-tail (value-of-mul-expr lhs env) tail env)))))

(define value-of-add-tail
  (lambda (lhs-val tail env)
    (cases additive-tail tail
      [add-tail-empty () lhs-val]
      [add-expr-tail (exp0 exp1)
                     (let* ([rhs-val (value-of-mul-expr exp0 env)]
                            [lhs-num (expval->num lhs-val)]
                            [rhs-num (expval->num rhs-val)]
                            [result (num-val (+ lhs-num rhs-num))])
                       (value-of-add-tail result exp1 env))]
      [sub-expr-tail (exp0 exp1)
                     (let* ([rhs-val (value-of-mul-expr exp0 env)]
                            [lhs-num (expval->num lhs-val)]
                            [rhs-num (expval->num rhs-val)]
                            [result (num-val (- lhs-num rhs-num))])
                       (value-of-add-tail result exp1 env))])))

(define value-of-mul-expr
  (lambda (mult env)
    (cases multiplicative-expression mult
      [mul-expr (lhs-val rhs) (value-of-multiplicative-tail (value-of-unary lhs-val env) rhs env)]
      )))

(define value-of-multiplicative-tail
  (lambda (lhs-val tail env)
    (cases multiplicative-tail tail
      [mul-tail-empty () lhs-val]
      [mul-expr-tail (exp0 exp1)
                     (let* ([rhs-val (value-of-unary exp0 env)]
                            [lhs-num (expval->num lhs-val)]
                            [rhs-num (expval->num rhs-val)]
                            [result (num-val (* lhs-num rhs-num))])
                       (value-of-multiplicative-tail result exp1 env))]
      [div-expr-tail (exp0 exp1)
                     (let* ([rhs-val (value-of-unary exp0 env)]
                            [lhs-num (expval->num lhs-val)]
                            [rhs-num (expval->num rhs-val)]
                            [result (num-val (/ lhs-num rhs-num))])
                       (value-of-multiplicative-tail result exp1 env))]
      [mod-expr-tail (exp0 exp1)
                     (let* ([rhs-val (value-of-unary exp0 env)]
                            [lhs-num (expval->num lhs-val)]
                            [rhs-num (expval->num rhs-val)]
                            [result (num-val (modulo lhs-num rhs-num))])
                       (value-of-multiplicative-tail result exp1 env))])))


(define value-of-unary
  (lambda (unary env)
    (cases unary-expression unary
      [not-expr (expr)
                (let ([val (value-of-unary expr env)])
                  (bool-val (not (expval->boolean val))))]
      [neg-expr (expr)
                (let ([val (value-of-unary expr env)])
                  (num-val (- 0 (expval->num val))))]
      [primary-expr (expr) (value-of-primary expr env)]
      )))

(define value-of-primary
  (lambda (expr env)
    (cases primary-expression expr
      [paren-expr (exp0)
                  (value-of-expr-stmt exp0 env)]
      [true-expr ()
                 (bool-val #t)]
      [false-expr ()
                  (bool-val #f)]
      [num-expr (n)
                (num-val n)]
      [postfix-expr (id tail)
                    (let ([primary-val (lookup-env env id)])
                      (value-of-postfix-tail primary-val tail env))]
      )))

(define value-of-postfix-tail
  (lambda (primary-val postfix-tail env)
    (cases postfix-expression-tail postfix-tail
      [postfix-tail-empty () primary-val] ;; No tail means we aren't making a function call, so this must be a constant
      [func-call-tail (args) (cases expval primary-val
                               [proc-val (the-proc)
                                         (cases proc the-proc
                                           [procedure (params body saved-env)
                                                      (let* ([arg-values (value-of-arguments args env)]
                                                             [new-env (extend-env-for-func params arg-values saved-env)]
                                                             [result (value-of-stmt-list body new-env)])
                                                        (cases inter-result result
                                                          [return-result (final-env ret-val)
                                                                         (cases ret-option ret-val
                                                                           [return-some (val) val]
                                                                           [return-none (val) val]
                                                                           [return-end-of-stmts () (undefined-val)]
                                                                           )]))])]
                               [else (eopl:error 'value-of-postfix-tail "Not a function")]
                               )]
      ; TODO!!!
      )))

(define value-of-arguments
  (lambda (args env)
    (cases argument-list args
      [arg-list-empty () '()]
      [arg-list (expr tail) (cons (value-of-expr-stmt expr env) (value-of-argument-tail tail env))]
      )))

(define value-of-argument-tail
  (lambda (args env)
    (cases argument-list-tail args
      [arg-list-tail-empty () '()]
      [arg-list-tail (expr tail) (cons (value-of-expr-stmt expr env) (value-of-argument-tail tail env))]
      )))

(define extend-env-for-func
  (lambda (params args env)
    (if (null? params)
        (if (null? args)
            env
            (eopl:error 'extend-env-for-func "Too many arguments"))
        (if (null? args)
            (eopl:error 'extend-env-for-func "Not enough arguments")
            (extend-env-for-func (cdr params) (cdr args)
                                 (extended-env (car params) (car args) env))
            ))))

(eopl:pretty-print (value-of-program (scan&parse "
function abs(x) {
    return x >= 0 ? x : -x;
}
function close_enough(x, y) {
    return abs(x - y) < 0.001;
}

// close_enough(12.0003, 12);
// close_enough(12.1, 12);
 ")))
