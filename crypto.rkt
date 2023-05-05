#lang racket
(require math/number-theory)
(require crypto)
(require crypto/libcrypto)
(crypto-factories (list libcrypto-factory))
(require "field.rkt")
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
  (hash 'r (signature-r sig_val) 's (signature-s sig_val)))

(define (signature->string tx_val)
  (string-append (number->string (signature-r tx_val))
                 (number->string (signature-s tx_val))))

(define (sign pk msg) ; should check if siging is the same in bitcoin
  (define k (generate_random))
  (define R (rmul_point G k))
  (define r_val (field-element-value (point-x R)))
  (define s_val (with-modulus N (mod/ (+ msg (* r_val pk)) k)))
  (signature r_val s_val))

(define (verify sig
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

(define (sha256_hex value)
  (bytes->hex-string (digest 'sha256 (hex->bytes value))))

(define (ripemd160_hex value)
  (bytes->hex-string (digest 'ripemd160 (hex->bytes value))))

(define (doublesha256 value)
  (sha256_hex (sha256_hex value)))

(define (hash160 value)
  (ripemd160_hex (sha256_hex value)))

;; for Schnorr signatures

(define (tagged_hash tag value)
  (define tag_digest
    (bytes->hex-string (digest 'sha256
                               (string->bytes/utf-8
                                tag)))) ; the tag is a utf-8 string value
  (define preimage (string-append tag_digest tag_digest value))
  (sha256_hex preimage))

(define (sign_schnorr pk msg) ; pk and msg should be integers
  (define msg_hex (~r msg #:base 16 #:min-width 64 #:pad-string "0"))
  (define a (generate_random)) ; Auxiliary random data
  ; Let P = d'⋅G
  (define Point (rmul_point G pk))
  ; Let d = d' if has_even_y(P), otherwise let d = n - d'
  (define d
    (if (= (modulo (field-element-value (point-y Point)) 2) 0) pk (- N pk)))
  ; Let t be the byte-wise xor of bytes(d) and hashBIP0340/aux(a)
  (define preimage_aux (~r a #:base 16 #:min-width 64 #:pad-string "0"))
  (define tag_hex (tagged_hash "BIP0340/aux" preimage_aux))
  (define tag_val (string->number tag_hex 16))
  (define t (bitwise-xor d tag_val))
  ; Let rand = hashBIP0340/nonce(t || bytes(P) || m)
  (define t_hex (~r t #:base 16 #:min-width 64 #:pad-string "0"))
  (define Px_hex
    (~r (field-element-value (point-x Point))
        #:base 16
        #:min-width 64
        #:pad-string "0"))
  (define preimage_nonce (string-append t_hex Px_hex msg_hex))
  (define rand_val (tagged_hash "BIP0340/nonce" preimage_nonce))
  ; Let k' = int(rand) mod n
  (define k_prime (modulo (string->number rand_val 16) N))
  ; Fail if k' = 0.
  (when (= k_prime 0)
    (error "fail, kprime cannot be zero"))
  ; Let R = k'⋅G.
  (define R (rmul_point G k_prime))
  ; Let k = k' if has_even_y(R), otherwise let k = n - k' .
  (define k
    (if (= (modulo (field-element-value (point-y R)) 2) 0)
        k_prime
        (- N k_prime)))
  ; Let e = int(hashBIP0340/challenge(bytes(R) || bytes(P) || m)) mod n.
  (define Rx_hex
    (~r (field-element-value (point-x R))
        #:base 16
        #:min-width 64
        #:pad-string "0"))
  (define preimage_challenge (string-append Rx_hex Px_hex msg_hex))
  (define e_hex (tagged_hash "BIP0340/challenge" preimage_challenge))
  (define e (modulo (string->number e_hex 16) N))
  ; Let sig = bytes(R) || bytes((k + ed) mod n).
  (define k_plus_ed (with-modulus N (mod+ k (* e d))))
  (define k_plus_ed_hex (number->string k_plus_ed 16))
  (define sig (string-append Rx_hex k_plus_ed_hex))
  sig)

(define (lift_x x) ; x is a 256-bit unsigned integer
  ;;    Fail if x ≥ p.
  (when (>= x P)
    (error "the point x cannot be >= P"))
  ;;    Let c = x^3 + 7 mod p.
  (define c (with-modulus P (mod+ (modexpt x 3) 7)))
  ;;    Let y = c^((p+1)/4) mod p.
  (define y (with-modulus P (modexpt c (mod/ (mod+ P 1) 4))))
  ;;    Fail if c ≠ y^2 mod p.
  (when (not (= c (with-modulus P (modexpt y 2))))
    (error "c should equal y^2"))
  ;;    Return the unique point P such that x(P) = x and y(P) = y if y mod 2 = 0 or y(P) = p-y otherwise.
  (define y_val (if (= (modulo y 2) 0) y (- P y)))
  (point (field-element x P) (field-element y_val P) secp256k1))

(define (verify_schnorr
         sig
         pub
         msg) ; pub is an hexval, msg is an int, sig is an hex string
  ;;    Let P = lift_x(int(pk)); fail if that fails.
  (define Point (lift_x (string->number pub 16)))
  ;;    Let r = int(sig[0:32]); fail if r ≥ p.
  (define r_hex (substring sig 0 64))
  (define r (string->number r_hex 16))
  ;;    Let s = int(sig[32:64]); fail if s ≥ n.
  (define s_hex (substring sig 64))
  (define s (string->number s_hex 16))
  ;;    Let e = int(hashBIP0340/challenge(bytes(r) || bytes(P) || m)) mod n.
  (define msg_hex (~r msg #:base 16 #:min-width 64 #:pad-string "0"))
  (define e_hex
    (tagged_hash "BIP0340/challenge" (string-append r_hex pub msg_hex)))
  (define e (modulo (string->number e_hex 16) N))
  ;;    Let R = s⋅G - e⋅P.
  ;;     R = point_add(point_mul(G, s), point_mul(P, n - e))
  (define R (add_point (rmul_point G s) (rmul_point Point (- N e))))
  ;;    Fail if is_infinite(R).
  ;;    Fail if not has_even_y(R).
  ;;    Fail if x(R) ≠ r.
  ;(when (equal? R I) (error "R is infinite"))
  ;(when (= (modulo (field-element-value (point-y R)) 2) 1) (error "y(R) is odd"))
  ;(when (not (equal? r_compute r)) (error "x(R) is not equal to r"))
  ;;   Return success iff no failure occurred before reaching this point.
  (define r_compute (field-element-value (point-x R)))
  (cond
    [(equal? R I) #f]
    [(= (modulo (field-element-value (point-y R)) 2) 1) #f]
    [(not (equal? r_compute r)) #f]
    [else #t]))

(define (pub_to_pubschnorr
         pub) ; take a public point and return a hexstring of the x value
  (~r (field-element-value (point-x pub))
      #:base 16
      #:min-width 64
      #:pad-string "0"))

(provide (struct-out signature)
         signature->jsexpr
         signature->string
         generate_random
         priv_to_pub
         sign
         verify
         sha256_hex
         ripemd160_hex
         doublesha256
         hash160
         tagged_hash
         sign_schnorr
         lift_x
         verify_schnorr
         pub_to_pubschnorr)
