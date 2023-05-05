#lang racket
(require "base58.rkt")

; input: value, type = hexastring
; output, type = base58 encoded string
(define (base58check_encode value)
  (define regex_val (regexp-match #rx"^[0]*" value))
  (define nb_0 (string-length (car regex_val)))
  (define nb_1 (quotient nb_0 2))
  (define prefix (make-string nb_1 #\1))
  (string-append prefix (base58encode (string->number value 16))))

; input: value, type = base58 encoded string
; output, type = hexastring
(define (base58check_decode value)
  (define regex_val (regexp-match #rx"^[1]*" value))
  (define nb_1 (string-length (car regex_val)))
  (define nb_0 (* nb_1 2))
  (define prefix (make-string nb_0 #\0))
  (string-append prefix (number->string (base58decode value) 16)))

(provide base58check_encode
         base58check_decode)
