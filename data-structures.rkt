#lang eopl
;;;
;;; Data structures for AJS-lang
;;;
(provide (all-defined-out))

;;;========= Expressed Values ===========

;;; Expressed values -- either a number or a boolean (if needed later)
(define-datatype expval expval?
  (num-val (value number?))
  (bool-val (boolean boolean?)))

;;; extractors

;;; expval->num : ExpVal -> Int
(define (expval->num v)
  (cases expval v
    (num-val (num) num)
    (else (expval-extractor-error 'num v))))

;;; expval-extractor-error : Symbol * ExpVal -> Error
(define (expval-extractor-error variant value)
  (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
              variant value))

;;;========= Environment Structures ===========

;;; The environment is currently unused since AJS-lang doesn't include variables
; (define-datatype environment environment?
;   (empty-env))

; (define (lookup-env env search-sym)
;   (eopl:error 'lookup-env "No environment or bindings in AJS-lang."))

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