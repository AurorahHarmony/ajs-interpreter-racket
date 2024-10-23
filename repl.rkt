#lang racket
(require racket/port)


(require "ajslang.rkt")

(define (process-input input)
  (let ([parsed (scan&parse-ajs input)])
    (displayln parsed)))

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