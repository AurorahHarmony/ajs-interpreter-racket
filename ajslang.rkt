#lang eopl
(provide (all-defined-out))

(define lex0
  '((whitespace (whitespace) skip) ;; Skip all whitespace
    (comment ("//" (arbno (not #\newline))) skip) ;; Ignore single line comments
    (identifier ((or letter "_" "$") (arbno (or letter digit "_" "$"))) symbol) ; Identifiers / Variable names
    (number (digit (arbno digit)) number)
    (number (digit (arbno digit) "." (arbno digit)) number)
    ; (number ("-" digit (arbno digit)) number)
    ))

(define ajs-grammar
  '((program (statement-list) a-program) ;; A program is a sequence of statements

    (statement-list (statement statement-list) stmt-list)
    (statement-list () stmt-list-empty)

    ;(statement ("const" identifier "=" expression ";") const-declaration)

    (statement ("const" identifier "=" expression ";") const-declaration)
    (statement (expression ";") expr-stmt)

    ;(expression (number) num-expr)
    ;; AdditiveExpression
    (expression (multiplicative-expression additive-tail) binop-expr)

    ;; MultiplicativeExpression
    (multiplicative-expression (unary-expression multiplicative-tail) mul-expr)

    ;; Tail for handling *, /, and %
    (multiplicative-tail ("*" unary-expression multiplicative-tail) mul-expr-tail)
    (multiplicative-tail ("/" unary-expression multiplicative-tail) div-expr-tail)
    (multiplicative-tail ("%" unary-expression multiplicative-tail) mod-expr-tail)
    (multiplicative-tail () mul-tail-empty)

    ;; Tail for handling + and -
    (additive-tail ("+" multiplicative-expression additive-tail) add-expr-tail)
    (additive-tail ("-" multiplicative-expression additive-tail) sub-expr-tail)
    (additive-tail () add-tail-empty)

    ;; UnaryExpression
    (unary-expression ("(" expression ")") paren-expr)
    (unary-expression (number) num-expr)
    (unary-expression (identifier) name-expr)

    ))

(sllgen:make-define-datatypes lex0 ajs-grammar)

(define scan&parse
  (sllgen:make-string-parser lex0 ajs-grammar))

(define just-scan
  (sllgen:make-string-scanner lex0 ajs-grammar))


; #(struct:a-program
;   #(struct:stmt-list
;     #(struct:const-declaration
;       cat
;       #(struct:binop-expr
;         #(struct:mul-expr #(struct:num-expr 2) #(struct:mul-tail-empty))
;         #(struct:add-tail-empty)))
;     #(struct:stmt-list
;       #(struct:expr-stmt
;         #(struct:binop-expr
;           #(struct:mul-expr #(struct:num-expr 6) #(struct:mul-tail-empty))
;           #(struct:add-tail-empty)))
;       #(struct:stmt-list
;         #(struct:expr-stmt
;           #(struct:binop-expr
;             #(struct:mul-expr #(struct:name-expr cat) #(struct:mul-tail-empty))
;             #(struct:add-tail-empty)))
;         #(struct:stmt-list-empty)))))
; 1+2*3;
; #(struct:a-program
;   #(struct:stmt-list
;     #(struct:expr-stmt
;       #(struct:binop-expr
;         #(struct:mul-expr #(struct:num-expr 1) #(struct:mul-tail-empty))
;         #(struct:add-expr-tail
;           #(struct:mul-expr
;             #(struct:num-expr 2)
;             #(struct:mul-expr-tail
;               #(struct:num-expr 3)
;               #(struct:mul-tail-empty)))
;           #(struct:add-tail-empty))))
;     #(struct:stmt-list-empty)))

; 1*2+3;
; #(struct:a-program
;   #(struct:stmt-list
;     #(struct:expr-stmt
;       #(struct:binop-expr
;         #(struct:mul-expr
;           #(struct:num-expr 1)
;           #(struct:mul-expr-tail
;             #(struct:num-expr 2)
;             #(struct:mul-tail-empty)))
;         #(struct:add-expr-tail
;           #(struct:mul-expr #(struct:num-expr 3) #(struct:mul-tail-empty))
;           #(struct:add-tail-empty))))
;     #(struct:stmt-list-empty)))

; 137;
; #(struct:a-program
;   #(struct:stmt-list
;     #(struct:expr-stmt
;       #(struct:binop-expr
;         #(struct:mul-expr #(struct:num-expr 137) #(struct:mul-tail-empty))
;         #(struct:add-tail-empty)))
;     #(struct:stmt-list-empty)))


; #(struct:a-program
;   #(struct:stmt-list
;     #(struct:expr-stmt
;       #(struct:binop-expr
;         #(struct:mul-expr #(struct:num-expr 1) #(struct:mul-tail-empty))
;         #(struct:add-tail-empty)))
;     #(struct:stmt-list
;       #(struct:expr-stmt
;         #(struct:binop-expr
;           #(struct:mul-expr #(struct:num-expr 5) #(struct:mul-tail-empty))
;           #(struct:add-tail-empty)))
;       #(struct:stmt-list-empty))))