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
      (expr-stmt (exp)
                 ;  (value-of-expr exp env))))) ;; Evaluate the expression in the statement
                 (value-of-expr exp env))))) ;; Evaluate the expression in the statement

;;; value-of-expr : Expr Env -> ExpVal
(define value-of-expr
  (lambda (exp env)
    (cases expression exp
      (num-expr (n) (num-val n))))) ;; Simply return the number as an ExpVal


(eopl:pretty-print (value-of-program (scan&parse "137;")))