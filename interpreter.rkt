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
                 (value-of-stmt-list body init-env)))))

;;; value-of-stmt-list : StmtList Env -> ExpVal
(define value-of-stmt-list
  (lambda (statements env)
    (cases statement-list statements
      (stmt-list-empty ()
                       ;;TODO: Have it return null
                       (num-val 0))  ;; Base case: If empty, return a default value (e.g., 0)
      (stmt-list (stmt rest)
                 (cases statement-list rest
                   (stmt-list-empty ()
                                    (value-of-stmt stmt env))
                   (stmt-list (stmt rest)
                              (begin
                                (value-of-stmt stmt env)
                                (value-of-stmt-list rest env))))))))

;;; value-of-stmt : Stmt Env -> ExpVal
(define value-of-stmt
  (lambda (stmt env)
    (cases statement stmt
      (expr-stmt (expr)
                 (value-of-expr-stmt expr env))))) ;; Evaluate the expression in the statement

;;; value-of-expr : Expr Env -> ExpVal
(define value-of-expr-stmt
  (lambda (expr env)
    (cases expression expr
      ; (num-expr (n) (num-val n))))) ;; Simply return the number as an ExpVal
      ; (binop-expr (expr rest) (display rest)))))
      (binop-expr (lhs rhs) (num-val (value-of-add-tail (value-of-mul-expr lhs env) rhs env))))))

(define value-of-mul-expr
  ; (lambda (mul-expr env) (begin (display  mul-expr) (newline) 1)))
  (lambda (mult env)
    (cases multiplicative-expression mult
      ; [mul-expr (lhs rhs) (begin (display (value-of-unary lhs env)) (newline) 1)]
      [mul-expr (lhs-val rhs) (value-of-multiplicative-tail (value-of-unary lhs-val env) rhs env)]
      )))

(define value-of-multiplicative-tail
  (lambda (lhs-val tail env)
    (cases multiplicative-tail tail
      [mul-tail-empty () lhs-val]
      [mul-expr-tail (exp0 exp1) (* lhs-val (value-of-multiplicative-tail (value-of-unary exp0 env) exp1 env))]
      [div-expr-tail (exp0 exp1) (/ lhs-val (value-of-multiplicative-tail (value-of-unary exp0 env) exp1 env))]
      [mod-expr-tail (exp0 exp1) (modulo lhs-val (value-of-multiplicative-tail (value-of-unary exp0 env) exp1 env))]
      )
    ))

(define value-of-add-tail
  (lambda (lhs-val tail env)
    (cases additive-tail tail
      [add-tail-empty () lhs-val]
      [add-expr-tail (exp0 exp1) (+ lhs-val (value-of-add-tail (value-of-mul-expr exp0 env) exp1 env))]
      [sub-expr-tail (exp0 exp1) (- lhs-val (value-of-add-tail (value-of-mul-expr exp0 env) exp1 env))]
      )
    ))

(define value-of-unary
  (lambda (unary env)
    (cases unary-expression unary
      [paren-expr (exp0) (expval->num (value-of-expr-stmt exp0 env))]
      [num-expr (n) n]
      )))


(eopl:pretty-print (value-of-program (scan&parse "2 * (3 + 2);")))
; (display (modulo 101 2))