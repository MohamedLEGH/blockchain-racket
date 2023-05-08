#lang racket
(require math/number-theory
         secp256k1)
(require "crypto-utils.rkt")

(define (sign-ecdsa pk msg) ; should check if siging is the same in bitcoin
  (define k (generate-random))
  (define R (rmul-point G k))
  (define r-val (field-element-value (point-x R)))
  (define s-val (with-modulus N (mod/ (+ msg (* r-val pk)) k)))
  (signature r-val s-val))

(define (verify-ecdsa
         sig
         pub
         msg) ; should check if verify signature is the same in bitcoin
  ; sig is type signature
  ; pub is a public point
  ; msg is an int
  (define s-val (signature-s sig))
  (define r-val (signature-r sig))
  (define u (with-modulus N (mod/ msg s-val)))
  (define v (with-modulus N (mod/ r-val s-val)))
  (define r-compute
    (field-element-value
     (point-x (add-point (rmul-point G u) (rmul-point pub v)))))
  (equal? r-compute r-val))

(provide sign-ecdsa
         verify-ecdsa)
