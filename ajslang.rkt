#lang eopl
(provide (all-defined-out))

(define lex0
  '((whitespace (whitespace) skip) ;; Skip all whitespace
    (comment ("//" (arbno (not #\newline))) skip) ;; Ignore single line comments
    (identifier ((or letter "_" "$") (arbno (or letter digit "_" "$"))) symbol) ; Identifiers / Variable names
    (number (digit (arbno digit)) number)
    (number (digit (arbno digit) "." (arbno digit)) number)
    ))

(define ajs-grammar
  '((program (statement-list) a-program) ;; A program is a sequence of statements

    ;; Statements
    (statement-list (statement statement-list) stmt-list)
    (statement-list () stmt-list-empty)

    (statement ("const" identifier "=" expression ";") const-declaration)
    (statement ("function" identifier "(" identifier-list ")" block) func-declaration)
    (statement ("return" expression ";") return-stmt)
    (statement (expression ";") expr-stmt)

    (identifier-list (identifier identifier-list-tail) id-list)
    (identifier-list () id-list-empty)
    (identifier-list-tail ("," identifier identifier-list-tail) id-list-tail)
    (identifier-list-tail () id-list-tail-empty)

    (block ("{" statement-list "}") block-stmt)

    ;; AdditiveExpression
    (expression (conditional-expression) cond-expr)

    ;; Conditional Expressions
    (conditional-expression (logical-or-expression conditional-expression-tail) conditional-expr)

    (conditional-expression-tail ("?" expression ":" conditional-expression) cond-expr-tail)
    (conditional-expression-tail () cond-expr-tail-empty)

    ;; Logical Expressions
    (logical-or-expression (logical-and-expression logical-or-tail) logical-or)

    (logical-or-tail ("||" logical-and-expression logical-or-tail) or-expr-tail)
    (logical-or-tail () logical-or-tail-empty)

    (logical-and-expression (equality-expression logical-and-tail) logical-and-expr)

    (logical-and-tail ("&&" equality-expression logical-and-tail) and-expr-tail)
    (logical-and-tail () logical-and-tail-empty)

    ;; Equality Expressions
    (equality-expression (relational-expression equality-tail) equality-expr)

    (equality-tail ("===" relational-expression equality-tail) seq-expr-tail)
    (equality-tail ("!==" relational-expression equality-tail) sneq-expr-tail)
    (equality-tail ("==" relational-expression equality-tail) eq-expr-tail)
    (equality-tail ("!=" relational-expression equality-tail) neq-expr-tail)
    (equality-tail () equality-tail-empty)

    ;; Relational Expressions
    (relational-expression (additive-expression relational-tail) relational-expr)

    (relational-tail ("<" additive-expression relational-tail) lt-expr-tail)
    (relational-tail (">" additive-expression relational-tail) gt-expr-tail)
    (relational-tail ("<=" additive-expression relational-tail) le-expr-tail)
    (relational-tail (">=" additive-expression relational-tail) ge-expr-tail)
    (relational-tail () relational-tail-empty)

    (additive-expression (multiplicative-expression additive-tail) additive-expr)

    ;; Tail for handling + and -
    (additive-tail ("+" multiplicative-expression additive-tail) add-expr-tail)
    (additive-tail ("-" multiplicative-expression additive-tail) sub-expr-tail)
    (additive-tail () add-tail-empty)

    ;; MultiplicativeExpression
    (multiplicative-expression (unary-expression multiplicative-tail) mul-expr)

    ;; Tail for handling *, /, and %
    (multiplicative-tail ("*" unary-expression multiplicative-tail) mul-expr-tail)
    (multiplicative-tail ("/" unary-expression multiplicative-tail) div-expr-tail)
    (multiplicative-tail ("%" unary-expression multiplicative-tail) mod-expr-tail)
    (multiplicative-tail () mul-tail-empty)

    ;; Unary Expression
    (unary-expression ("!" unary-expression) not-expr)
    (unary-expression ("-" unary-expression) neg-expr)
    (unary-expression (primary-expression) primary-expr)

    ;; Primary Expression
    (primary-expression ("(" expression ")") paren-expr)
    (primary-expression ("true") true-expr)
    (primary-expression ("false") false-expr)
    (primary-expression (number) num-expr)
    (primary-expression (identifier postfix-expression-tail) postfix-expr)

    ;; Postfix (for function calls)
    (postfix-expression-tail ("(" argument-list ")") func-call-tail)
    (postfix-expression-tail () postfix-tail-empty)

    (argument-list (expression argument-list-tail) arg-list)
    (argument-list () arg-list-empty)
    (argument-list-tail ("," expression argument-list-tail) arg-list-tail)
    (argument-list-tail () arg-list-tail-empty)

    ))

(sllgen:make-define-datatypes lex0 ajs-grammar)

(define scan&parse
  (sllgen:make-string-parser lex0 ajs-grammar))

(define just-scan
  (sllgen:make-string-scanner lex0 ajs-grammar))