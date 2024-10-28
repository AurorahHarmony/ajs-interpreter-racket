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
 ;; ====== Arithmetic ======
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

 ;; ====== Constant Declarations ======
 (const-declaration-retrieval "const size = 5; size;" 5)
 (const-declaration-retrieval2 "const size = 5; 5 * size;" 25)
 (const-times-const "const pi = 3.14159;
                     const radius = 10;
                     pi * radius * radius;" 314.159) ;; TODO: Add decimal support
 (const-times-const-retrieval "const pi = 3.14159; 
                               const radius = 10;
                               const circumference = 2 * pi * radius;
                               circumference;" 62.8318)

 ;; ====== Function Declarations ======
 (func-call "function square(x) {
              return x * x;
             }
             square(21);" 441)
 (func-call-param-expression "function square(x) {
                                return x * x;
                              }
                              square(4 + 2);" 36)
 (func-call-as-expression "function square(x) {
                            return x * x;
                           }
                           square(3) + square(4);" 25)

 ;; ====== Compound Functions ======
 (simple-compound "
  function square(x) {
    return x * x;
  }

  square(square(3));" 81)

 (complex-compound "
  function square(x) {
    return x * x;
  }

  function sum_of_squares(x, y) {
      return square(x) + square(y);
  }

  function f(a) {
      return( sum_of_squares(a + 1, a * 2));
  }

  f(5);" 136)

 ;; ====== Booleans and Conditionals ======
 (and-true "(3 > 2) && (5 < 9);" #t)
 (and-false "(3 > 2) && (5 > 9);" #f)
 (or-true "(3 > 2) || (5 > 9);" #t)
 (or-true-2 "(3 < 2) || (5 < 9);" #t)
 (or-false "(3 < 2) || (5 > 9);" #f)

 ;; Absolutes
 (abs-pos "
  function abs(x) {
      return x >= 0 ? x : -x;
    }

  abs(6);" 6)

 (abs-neg "
  function abs(x) {
      return x >= 0 ? x : -x;
    }

  abs(-5);" 5)

 (abs-zero "
  function abs(x) {
      return x >= 0 ? x : -x;
    }

  abs(0);" 0)

 ;; Absolutes 2
 (abs2-pos "
  function abs2(x) {
      return x > 0
                ? x
                    : x === 0
                    ? 0
                    : -x;
  }

  abs2(10);" 10)
 (abs2-neg "
  function abs2(x) {
      return x > 0
                ? x
                    : x === 0
                    ? 0
                    : -x;
  }

  abs2(-33);" 33)

 (abs2-zero "
  function abs2(x) {
      return x > 0
                ? x
                    : x === 0
                    ? 0
                    : -x;
  }

  abs2(0);" 0)

 ;; Close enough
 (close-enough "
  function abs(x) {
      return x >= 0 ? x : -x;
  }
  function close_enough(x, y) {
      return abs(x - y) < 0.001;
  }

  close_enough(12.0003, 12);" #t)

 (not-close-enough "
  function abs(x) {
      return x >= 0 ? x : -x;
  }
  function close_enough(x, y) {
      return abs(x - y) < 0.001;
  }

  close_enough(12.1, 12);" #f)
 )