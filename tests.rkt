#lang eopl
;;;
;;; Tests the interpreter for PROC-lang
;;;
(require "test-utils.rkt")
(require "data-structures.rkt")
(require "ajslang.rkt")
(require "interpreter.rkt")


;;; run : string -> expval
(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(define equal-answer?
  (lambda (ans correct-ans)
    (equal? ans (sloppy->expval correct-ans))))

(define sloppy->expval
  (lambda (sloppy-val)
    (cond
      ((number? sloppy-val) (num-val sloppy-val))
      ((boolean? sloppy-val) (bool-val sloppy-val))
      (else
       (eopl:error 'sloppy->expval
                   "Can't convert sloppy value to expval: ~s"
                   sloppy-val)))))

(define-syntax-rule (check-run (name str res) ...)
  (begin
    (cond [(eqv? 'res 'error)
           (check-exn always? (lambda () (run str)))]
          [else
           (check equal-answer? (run str) 'res (symbol->string 'name))])
    ...))


;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
(check-run
 ;; Arithmetic
 (positive-const "345;" 345)
 (addition "123 + 45;" 168)
 (substraction "1000 - 482;" 518)
 (multiplication "5 * 35;" 175)
 (division "10 / 4;" 5/2) ;; TODO: Show this as a float
 (addition-decimal "9.5 + 22;" 31.5) ;; TODO: Add decimal support
 (paren-addition "(3 * 5) + (10 - 4);" 21)
 (bedmas "1 - 5 / 2 * 4 + 3;" -6) ;; TODO: Show this as a negative number
 (paren-multiplication "3 * 2 * (5 + 1 - 2);" 24)
 (last-statement-return "1; 2; 3;" 3)

 ;; Constant Declarations
 (const-declaration-retrieval "const size = 5; size;" 5)
 (const-declaration-retrieval2 "const size = 5; 5 * size;" 25)
 (const-times-const "const pi = 3.14159;
                     const radius = 10;
                     pi * radius * radius;" 314.159) ;; TODO: Add decimal support
 (const-times-const-retrieval "const pi = 3.14159; 
                               const radius = 10;
                               const circumference = 2 * pi * radius;
                               circumference;" 62.8318)
 )