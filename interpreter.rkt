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
      (binop-expr (lhs rhs) (value-of-add-tail (value-of-mul-expr lhs env) rhs env)))))

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

(define value-of-unary
  (lambda (unary env)
    (cases unary-expression unary
      [paren-expr (exp0) (value-of-expr-stmt exp0 env)]
      [num-expr (n) (num-val n)]
      ; [name-expr (id) (lookup-env env id)]
      [postfix-expr (id tail) (let ([primary-val (lookup-env env id)])
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
function square(x) {
    return x * x;
}

square(square(3));

function sum_of_squares(x, y) {
    return square(x) + square(y);
}

function f(a) {
    return( sum_of_squares(a + 1, a * 2));
}

f(5);
 ")))
