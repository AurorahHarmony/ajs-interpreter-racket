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
 )