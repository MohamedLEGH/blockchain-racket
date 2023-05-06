#lang racket
(require racket/list)

(define code_string
  "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz")
(define base (string-length code_string)) ; should give 58

; input: value, type = int
; output, type = base58 encoded string
(define (base58encode value)
  (define (ref->string str k)
    (string (string-ref str k)))
  (define (base58enc acc val)
    (if (= val 0)
        acc
        (base58enc
         (string-append (ref->string code_string (modulo val base)) acc)
         (quotient val base))))
  (base58enc "" value))

; input: value, type = base58 encoded string
; output, type = int
(define (base58decode value)
  (define code_list (string->list code_string))
  (define l (string->list value))
  (define reverse_l (reverse l))
  (define (base58dec acc power val)
    (if (equal? val '())
        acc
        (base58dec (+ (* (index-of code_list (car val)) (expt base power)) acc)
                   (+ power 1)
                   (cdr val))))
  (base58dec 0 0 reverse_l))

(provide base58encode
         base58decode)
