#lang eopl
(provide (all-defined-out))

(define lex0
  '((whitespace (whitespace) skip) ;; Skip all whitespace
    (comment ("//" (arbno (not #\newline))) skip) ;; Ignore single line comments
    (identifier ((or letter "_" "$") (arbno (or letter digit "_" "$"))) symbol) ; Identifiers / Variable names
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    ))

(define ajs-grammar
  '((program (statement-list) a-program) ;; A program is a sequence of statements

    (statement-list (statement statement-list) stmt-list)
    (statement-list () stmt-list-empty)

    ;(statement ("const" identifier "=" expression ";") const-declaration)

    (statement (expression ";") expr-stmt)

    (expression (number) num-expr)

    ))

(sllgen:make-define-datatypes lex0 ajs-grammar)

(define scan&parse
  (sllgen:make-string-parser lex0 ajs-grammar))

(define just-scan
  (sllgen:make-string-scanner lex0 ajs-grammar))

; #(struct:a-program
;   #(struct:stmt-list
;     #(struct:expr-stmt #(struct:num-expr 123))
;     #(struct:stmt-list-empty)))