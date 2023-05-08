#lang racket
(require math/number-theory)
(require secp256k1)
(require "crypto-utils.rkt")

;; for Schnorr signatures

(define (sign-schnorr pk msg) ; pk and msg should be integers
  (define msg-hex (~r msg #:base 16 #:min-width 64 #:pad-string "0"))
  (define a (generate-random)) ; Auxiliary random data
  ; Let P = d'⋅G
  (define Point (rmul-point G pk))
  ; Let d = d' if has-even-y(P), otherwise let d = n - d'
  (define d
    (if (= (modulo (field-element-value (point-y Point)) 2) 0) pk (- N pk)))
  ; Let t be the byte-wise xor of bytes(d) and hashBIP0340/aux(a)
  (define preimage-aux (~r a #:base 16 #:min-width 64 #:pad-string "0"))
  (define tag-hex (tagged-hash "BIP0340/aux" preimage-aux))
  (define tag-val (string->number tag-hex 16))
  (define t (bitwise-xor d tag-val))
  ; Let rand = hashBIP0340/nonce(t || bytes(P) || m)
  (define t-hex (~r t #:base 16 #:min-width 64 #:pad-string "0"))
  (define Px-hex
    (~r (field-element-value (point-x Point))
        #:base 16
        #:min-width 64
        #:pad-string "0"))
  (define preimage-nonce (string-append t-hex Px-hex msg-hex))
  (define rand-val (tagged-hash "BIP0340/nonce" preimage-nonce))
  ; Let k' = int(rand) mod n
  (define k-prime (modulo (string->number rand-val 16) N))
  ; Fail if k' = 0.
  (when (= k-prime 0)
    (error "fail, kprime cannot be zero"))
  ; Let R = k'⋅G.
  (define R (rmul-point G k-prime))
  ; Let k = k' if has-even-y(R), otherwise let k = n - k' .
  (define k
    (if (= (modulo (field-element-value (point-y R)) 2) 0)
        k-prime
        (- N k-prime)))
  ; Let e = int(hashBIP0340/challenge(bytes(R) || bytes(P) || m)) mod n.
  (define Rx-hex
    (~r (field-element-value (point-x R))
        #:base 16
        #:min-width 64
        #:pad-string "0"))
  (define preimage-challenge (string-append Rx-hex Px-hex msg-hex))
  (define e-hex (tagged-hash "BIP0340/challenge" preimage-challenge))
  (define e (modulo (string->number e-hex 16) N))
  ; Let sig = bytes(R) || bytes((k + ed) mod n).
  (define k-plus-ed (with-modulus N (mod+ k (* e d))))
  (define k-plus-ed-hex (number->string k-plus-ed 16))
  (define sig (string-append Rx-hex k-plus-ed-hex))
  sig)

(define (lift-x x) ; x is a 256-bit unsigned integer
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
  (define y-val (if (= (modulo y 2) 0) y (- P y)))
  (point (field-element x P) (field-element y-val P) secp256k1))

(define (verify-schnorr
         sig
         pub
         msg) ; pub is an hexval, msg is an int, sig is an hex string
  ;;    Let P = lift-x(int(pk)); fail if that fails.
  (define Point (lift-x (string->number pub 16)))
  ;;    Let r = int(sig[0:32]); fail if r ≥ p.
  (define r-hex (substring sig 0 64))
  (define r (string->number r-hex 16))
  ;;    Let s = int(sig[32:64]); fail if s ≥ n.
  (define s-hex (substring sig 64))
  (define s (string->number s-hex 16))
  ;;    Let e = int(hashBIP0340/challenge(bytes(r) || bytes(P) || m)) mod n.
  (define msg-hex (~r msg #:base 16 #:min-width 64 #:pad-string "0"))
  (define e-hex
    (tagged-hash "BIP0340/challenge" (string-append r-hex pub msg-hex)))
  (define e (modulo (string->number e-hex 16) N))
  ;;    Let R = s⋅G - e⋅P.
  ;;     R = point-add(point-mul(G, s), point-mul(P, n - e))
  (define R (add-point (rmul-point G s) (rmul-point Point (- N e))))
  ;;    Fail if is-infinite(R).
  ;;    Fail if not has-even-y(R).
  ;;    Fail if x(R) ≠ r.
  ;(when (equal? R I) (error "R is infinite"))
  ;(when (= (modulo (field-element-value (point-y R)) 2) 1) (error "y(R) is odd"))
  ;(when (not (equal? r-compute r)) (error "x(R) is not equal to r"))
  ;;   Return success iff no failure occurred before reaching this point.
  (define r-compute (field-element-value (point-x R)))
  (cond
    [(equal? R I) #f]
    [(= (modulo (field-element-value (point-y R)) 2) 1) #f]
    [(not (equal? r-compute r)) #f]
    [else #t]))

(define (tweak-pubkey
         pubkey
         h) ;  pub as a x-only public key in hex format, h as a hexstring
  (define Point (lift-x (string->number pubkey 16))) ; works
  (define hashhex (tagged-hash "TapTweak" (string-append pubkey h)))
  (define hashval (string->number hashhex 16)) ; works
  (when (>= hashval N)
    (error "value is superior to the order of the curve"))
  (define Q (add-point Point (rmul-point G hashval))) ; tweak of the public key
  ; convert Q to hex (only the x part)
  (define Qx (point-to-pubschnorr-string Q))
  Qx)

(define (point-to-pubschnorr-string
         pub) ; take a public point and return a hexstring of the x value
  (substring (point-to-string pub) 0 64))

(provide sign-schnorr
         lift-x
         verify-schnorr
         tweak-pubkey
         point-to-pubschnorr-string)
