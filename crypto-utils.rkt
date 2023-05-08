#lang racket
(require crypto
         crypto/libcrypto
         secp256k1)
(crypto-factories (list libcrypto-factory))

(define (generate-random)
  (define p
    (for/fold ([result 0]) ([byte (in-bytes (crypto-random-bytes 32))])
      (+ byte (* result 256))))
  ; the random number need to be inferior to N (the order of the curve)
  ; if it's not the case (small probability), recompute a random number
  (if (and (< p N) (> p 0)) p (generate-random)))

(define (priv-to-pub pk) ; pk in hexa string format
  (point-to-string (rmul-point G (string->number pk 16))))

(struct signature (r s)) ; should check if signature is the same in bitcoin

(define (signature->string tx-val)
  (string-append (number->string (signature-r tx-val))
                 (number->string (signature-s tx-val))))

(define (signature->jsexpr sig-val)
  (hash 'r (signature-r sig-val) 's (signature-s sig-val)))

(define (sha256-hex value)
  (bytes->hex-string (digest 'sha256 (hex->bytes value))))

(define (ripemd160-hex value)
  (bytes->hex-string (digest 'ripemd160 (hex->bytes value))))

(define (doublesha256 value)
  (sha256-hex (sha256-hex value)))

(define (hash160 value)
  (ripemd160-hex (sha256-hex value)))

(define (tagged-hash tag value)
  (define tag-digest
    (bytes->hex-string (digest 'sha256
                               (string->bytes/utf-8
                                tag)))) ; the tag is a utf-8 string value
  (define preimage (string-append tag-digest tag-digest value))
  (sha256-hex preimage))

(provide (struct-out signature)
         signature->jsexpr
         signature->string
         generate-random
         priv-to-pub
         sha256-hex
         ripemd160-hex
         doublesha256
         hash160
         tagged-hash)
