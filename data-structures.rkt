#lang eopl
;;;
;;; Data structures for AJS-lang
;;;
(require "ajslang.rkt")
(provide (all-defined-out))

;;;========= Expressed Values ===========

;;; Expressed values -- either a number or a boolean (if needed later)
(define-datatype expval expval?
  (num-val (value number?))
  (bool-val (boolean boolean?))
  (proc-val (proc proc?))
  (null-val)
  (undefined-val))

;;; extractors

;;; expval->num : ExpVal -> Int
(define (expval->num v)
  (cases expval v
    (num-val (num) num)
    (else (expval-extractor-error 'num v))))

(define expval->boolean
  (lambda (v)
    (cases expval v
      [bool-val (b) b]
      [num-val (n) (not (zero? n))]
      [else (expval-extractor-error 'bool v)])))

;;; expval-extractor-error : Symbol * ExpVal -> Error
(define (expval-extractor-error variant value)
  (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
              variant value))

;;; Boolean comparison
(define expval-equals?
  (lambda (val1 val2)
    (cases expval val1
      (num-val (n1)
               (cases expval val2
                 (num-val (n2) (= n1 n2))
                 (else #f)))
      (bool-val (b1)
                (cases expval val2
                  (bool-val (b2) (eq? b1 b2))
                  (else #f)))
      (else #f))))

(define expval-strict-equals?
  (lambda (val1 val2)
    (cases expval val1
      (num-val (n1)
               (cases expval val2
                 (num-val (n2) (= n1 n2))
                 (else #f)))
      (bool-val (b1)
                (cases expval val2
                  (bool-val (b2) (eq? b1 b2))
                  (else #f)))
      (else #f))))

;;;========= Return Values ===========
(define-datatype inter-result inter-result?
  (return-result
   (env environment?)
   (return-val ret-option?)))

(define-datatype ret-option ret-option?
  (return-some (val expval?)) ;; a return statement was reached
  (return-none (val expval?)) ;; no return statement was reached
  (return-end-of-stmts))

;;;========= Procedures ===========
(define-datatype proc proc?
  (procedure
   (params (list-of symbol?))
   (body statement-list?)
   (env environment?)))

;;;========= Environment Structures ===========
(define-datatype environment environment?
  (empty-env)
  (extended-env
   (sym symbol?)
   (val anything?)
   (env environment?)))

(define anything? (lambda (v) #t))

(define (lookup-env env search-sym)
  (cases environment env
    (empty-env ()
               (eopl:error 'lookup-env "No binding for ~s" search-sym))
    (extended-env (sym val oldenv)
                  (if (eqv? sym search-sym)
                      val
                      (lookup-env oldenv search-sym)))))