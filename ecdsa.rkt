#lang racket
(require math/number-theory)
(require crypto)
(require crypto/libcrypto)
(crypto-factories (list libcrypto-factory))
(require "field.rkt")
(require "curve.rkt")
(require "crypto-utils.rkt")

(define (sign_ecdsa pk msg) ; should check if siging is the same in bitcoin
  (define k (generate_random))
  (define R (rmul_point G k))
  (define r_val (field-element-value (point-x R)))
  (define s_val (with-modulus N (mod/ (+ msg (* r_val pk)) k)))
  (signature r_val s_val))

(define (verify_ecdsa
         sig
         pub
         msg) ; should check if verify signature is the same in bitcoin
  ; sig is type signature
  ; pub is a public point
  ; msg is an int
  (define s_val (signature-s sig))
  (define r_val (signature-r sig))
  (define u (with-modulus N (mod/ msg s_val)))
  (define v (with-modulus N (mod/ r_val s_val)))
  (define r_compute
    (field-element-value
     (point-x (add_point (rmul_point G u) (rmul_point pub v)))))
  (equal? r_compute r_val))

(provide sign_ecdsa
         verify_ecdsa)
