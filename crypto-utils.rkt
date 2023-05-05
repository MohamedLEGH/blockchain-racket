#lang racket
(require crypto)
(require crypto/libcrypto)
(crypto-factories (list libcrypto-factory))
(require "curve.rkt")

(define (generate_random)
  (define p
    (for/fold ([result 0]) ([byte (in-bytes (crypto-random-bytes 32))])
      (+ byte (* result 256))))
  ; the random number need to be inferior to N (the order of the curve)
  ; if it's not the case (small probability), recompute a random number
  (if (and (< p N) (> p 0)) p (generate_random)))

(define (priv_to_pub pk) ; pk in hexa string format
  (point_to_string (rmul_point G (string->number pk 16))))

(struct signature (r s)) ; should check if signature is the same in bitcoin

(define (signature->string tx_val)
  (string-append (number->string (signature-r tx_val))
                 (number->string (signature-s tx_val))))

(define (signature->jsexpr sig_val)
  (hash 'r (signature-r sig_val) 's (signature-s sig_val)))

(define (sha256_hex value)
  (bytes->hex-string (digest 'sha256 (hex->bytes value))))

(define (ripemd160_hex value)
  (bytes->hex-string (digest 'ripemd160 (hex->bytes value))))

(define (doublesha256 value)
  (sha256_hex (sha256_hex value)))

(define (hash160 value)
  (ripemd160_hex (sha256_hex value)))

(define (tagged_hash tag value)
  (define tag_digest
    (bytes->hex-string (digest 'sha256
                               (string->bytes/utf-8
                                tag)))) ; the tag is a utf-8 string value
  (define preimage (string-append tag_digest tag_digest value))
  (sha256_hex preimage))

(provide (struct-out signature)
         signature->jsexpr
         signature->string
         generate_random
         priv_to_pub
         sha256_hex
         ripemd160_hex
         doublesha256
         hash160
         tagged_hash)
