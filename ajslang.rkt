#lang eopl
(provide (all-defined-out))

(define lex0
  '((whitespace (whitespace) skip) ;; Skip all whitespace
    (comment ("//" (arbno (not #\newline))) skip) ;; Ignore single line comments
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    (open-paren ("(") symbol) ;; Opening parenthesis
    (close-paren (")") symbol) ;; Closing parenthsis
    ))

(define ajs-grammar
   '((program (statement-list) a-program) ;; A program is a sequence of statements

     (statement-list (statement statement-list) stmt-list)
     (statement-list () stmt-list-empty)

     (statement (expression ";") expr-stmt)

     (expression (number expression-rest) num-expr)
     (expression-rest (binop expression) expr-rest)
     (expression-rest () expr-rest-empty)

     (binop ("+") add-op)
     (binop ("-") sub-op)

    ))

(sllgen:make-define-datatypes lex0 ajs-grammar)

(define scan&parse-ajs
  (sllgen:make-string-parser lex0 ajs-grammar))

(define scan&parse
  (sllgen:make-string-parser lex0 ajs-grammar))

(define just-scan
  (sllgen:make-string-scanner lex0 ajs-grammar))
