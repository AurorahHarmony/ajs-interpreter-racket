#lang racket
(require racket/port)


(require "ajslang.rkt")
(require "interpreter.rkt")
(require "data-structures.rkt")

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(define (process-input input)
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (displayln (exn-message e)))])
    (let ([result (run input)])
      (pretty-display (print-ready-value result)))))

;; REPL function
(define (ajs-repl)
  (displayln "AJS Console - Type 'exit' to quit")
  (let loop ()
    (display "> ")
    (flush-output)
    (let ([input (read-line)])
      (cond
        [(equal? input "exit")]
        [else
         (process-input input)
         (loop)]))))

(ajs-repl)