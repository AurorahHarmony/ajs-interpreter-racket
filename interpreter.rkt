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
                 (value-of-stmt-list body (init-env))))))

;;; value-of-stmt-list : StmtList Env -> ExpVal
(define value-of-stmt-list
  (lambda (statements env)
    (cases statement-list statements
      [stmt-list-empty ()
                       #f] ;; Null as base case
      [stmt-list (stmt rest)
                 (let ([new-env (value-of-stmt stmt env)])
                   (let ([rest-val (value-of-stmt-list rest new-env)])
                     (if (eq? rest-val #f)
                         (value-of-stmt-return stmt env)
                         rest-val)))])))

;;; value-of-stmt : Stmt Env -> ExpVal
;;; Returns an updated env
(define value-of-stmt
  (lambda (stmt env)
    (cases statement stmt
      [const-declaration (id expr)
                         (let [(val (value-of-expr-stmt expr env))]
                           (extended-env id val env))]
      [func-declaration (id id-list block) 0] ;; TODO!!!
      [return-stmt (expr) 0] ;; TODO!!!
      [expr-stmt (expr)
                 (begin (value-of-expr-stmt expr env) env)]))) ;; Evaluate the expression in the statement

;;; value-of-stmt-return : Stmt Env -> ExpVal
;; Returns a value (mainly for use in the REPL)
(define value-of-stmt-return
  (lambda (stmt env)
    (cases statement stmt
      [const-declaration (id expr) #f]  ;; Declarations do not produce a value
      [func-declaration (id id-list block) 0] ;; TODO!!!
      [return-stmt (expr) 0] ;; TODO!!!
      [expr-stmt (expr) (value-of-expr-stmt expr env)]))) ;; Return the value of the expression

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
      [func-call-tail (args) 0] ; TODO!!!
      [postfix-tail-empty () primary-val]
      )))

; (eopl:pretty-print (value-of-program (scan&parse "
; const cat = 7;
;  const dog = cat + cat; 
;  dog;
;  ")))