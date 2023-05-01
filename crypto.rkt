#lang racket
(require racket/random)
(require math/number-theory)
(require crypto)
(require crypto/libcrypto)(require "field.rkt")
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

(define (signature->jsexpr sig_val)
  (hash 'r
        (signature-r sig_val)
        's
        (signature-s sig_val)))

(define (signature->string tx_val)
  (string-append (number->string (signature-r tx_val))
                 (number->string (signature-s tx_val))))


(define (sign pk msg) ; should check if siging is the same in bitcoin
  (define k (generate_random))
  (define R (rmul_point G k))
  (define r_val (field-element-value (point-x R)))
  (define s_val (with-modulus N (mod/ (+ msg (* r_val pk)) k)))
  (signature r_val s_val))

(define (verify sig pub msg) ; should check if verify signature is the same in bitcoin
  (define s_val (signature-s sig))
  (define r_val (signature-r sig))
  (define u (with-modulus N (mod/ msg s_val)))
  (define v (with-modulus N (mod/ r_val s_val)))
  (define r_compute
    (field-element-value
     (point-x (add_point (rmul_point G u) (rmul_point pub v)))))
  (equal? r_compute r_val))

(define (sha256_hex value)
  (bytes->hex-string (digest 'sha256 (hex->bytes value))))

(define (ripemd160_hex value)
  (bytes->hex-string (digest 'ripemd160 (hex->bytes value))))

(define (doublesha256 value)
  (sha256_hex (sha256_hex value)))

(define (hash160 value)
  (ripemd160_hex (sha256_hex value)))


;;;;;;;;;;;;;;;;; test

(define pub1
  (point (field-element
          #x887387E452B8EACC4ACFDE10D9AAF7F6D9A0F975AABB10D006E4DA568744D06C
          P)
         (field-element
          #x61DE6D95231CD89026E286DF3B6AE4A894A3378E393E93A0F45B666329A0AE34
          P)
         secp256k1))

;# Test case 1: verify authenticity
;z = 0xEC208BAA0FC1C19F708A9CA96FDEFF3AC3F230BB4A7BA4AEDE4942AD003C0F60
;r = 0xAC8D1C87E51D0D441BE8B3DD5B05C8795B48875DFFE00B7FFCFAC23010D3A395
;s = 0x68342CEFF8935EDEDD102DD876FFD6BA72D6A427A3EDB13D26EB0781CB423C4

(define z1 #xEC208BAA0FC1C19F708A9CA96FDEFF3AC3F230BB4A7BA4AEDE4942AD003C0F60)
(define r1 #xAC8D1C87E51D0D441BE8B3DD5B05C8795B48875DFFE00B7FFCFAC23010D3A395)
(define s1 #x68342CEFF8935EDEDD102DD876FFD6BA72D6A427A3EDB13D26EB0781CB423C4)

(define sig1 (signature r1 s1))
(verify sig1 pub1 z1)

;# Test case 1.1: false signature r

(define r1bis
  #xAB8D1C87E51D0D441BE8B3DD5B05C8795B48875DFFE00B7FFCFAC23010D3A395)

(define sig11 (signature r1bis s1))
(equal? (verify sig11 pub1 z1) #f)

;# Test case 1.2: false signature s

(define s1bis #x68242CEFF8935EDEDD102DD876FFD6BA72D6A427A3EDB13D26EB0781CB423C4)

(define sig12 (signature r1 s1bis))
(equal? (verify sig12 pub1 z1) #f)

;# Test case 1.3: false message

(define z1bis
  #xEC208BAA0FC1C29F708A9CA96FDEFF3AC3F230BB4A7BA4AEDE4942AD003C0F60)

(equal? (verify sig1 pub1 z1bis) #f)

;# Test case 1.4: false pub key x

(define pub1bis
  (point (field-element
          #x887386E452B8EACC4ACFDE10D9AAF7F6D9A0F975AABB10D006E4DA568744D06C
          P)
         (field-element
          #x61DE6D95231CD89026E286DF3B6AE4A894A3378E393E93A0F45B666329A0AE34
          P)
         secp256k1))

(equal? (verify sig1 pub1bis z1) #f)

;# Test case 1.4: false pub key y

(define pub1bisbis
  (point (field-element
          #x887387E452B8EACC4ACFDE10D9AAF7F6D9A0F975AABB10D006E4DA568744D06C
          P)
         (field-element
          #x61DE6D94231CD89026E286DF3B6AE4A894A3378E393E93A0F45B666329A0AE34
          P)
         secp256k1))

(equal? (verify sig1 pub1bisbis z1) #f)

;# Test case 2: verify authenticity for different signature w/ same P
;z = 0x7C076FF316692A3D7EB3C3BB0F8B1488CF72E1AFCD929E29307032997A838A3D
;r = 0xEFF69EF2B1BD93A66ED5219ADD4FB51E11A840F404876325A1E8FFE0529A2C
;s = 0xC7207FEE197D27C618AEA621406F6BF5EF6FCA38681D82B2F06FDDBDCE6FEAB6
;assert Signature(r, s).verify(z, pub)

(define z2 #x7C076FF316692A3D7EB3C3BB0F8B1488CF72E1AFCD929E29307032997A838A3D)
(define r2 #xEFF69EF2B1BD93A66ED5219ADD4FB51E11A840F404876325A1E8FFE0529A2C)
(define s2 #xC7207FEE197D27C618AEA621406F6BF5EF6FCA38681D82B2F06FDDBDCE6FEAB6)

(define sig2 (signature r2 s2))
(verify sig2 pub1 z2)

;# Test case 3: sign and verify
;e = PrivateKey(randint(0, N))  # generate a private key
;pub = e.secret * G  # public point corresponding to e
;z = randint(0, 2 ** 256)  # generate a random message for testing
;signature: Signature = e.sign(z)
;assert signature.verify(z, pub)
(define e3 (generate_random)) ; a private key is just a random number
(define pub3 (rmul_point G e3)) ; G*e to get the public key
(define z3 (generate_random)) ; just a random message
(define sig3 (sign e3 z3))
(verify sig3 pub3 z3)

(provide (struct-out signature)
         signature->jsexpr
         signature->string
         generate_random
         priv_to_pub
         sign
         verify
         sha256_hex ripemd160_hex doublesha256 hash160)
