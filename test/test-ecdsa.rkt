#lang racket
(require rackunit
         rackunit/text-ui
         "../crypto.rkt"
         "../curve.rkt"
         "../field.rkt")

(define test-ecdsa
  (test-suite
   "Tests for schnorr.rkt"
   (let ()
     (define pub1
       (point
        (field-element
         #x887387E452B8EACC4ACFDE10D9AAF7F6D9A0F975AABB10D006E4DA568744D06C
         P)
        (field-element
         #x61DE6D95231CD89026E286DF3B6AE4A894A3378E393E93A0F45B666329A0AE34
         P)
        secp256k1))
     (define z1
       #xEC208BAA0FC1C19F708A9CA96FDEFF3AC3F230BB4A7BA4AEDE4942AD003C0F60)
     (define r1
       #xAC8D1C87E51D0D441BE8B3DD5B05C8795B48875DFFE00B7FFCFAC23010D3A395)
     (define s1
       #x68342CEFF8935EDEDD102DD876FFD6BA72D6A427A3EDB13D26EB0781CB423C4)
     (define sig1 (signature r1 s1))

     (test-case "Test authenticity for ECDSA signatures"
                (check-true (verify sig1 pub1 z1)))

     (define r1bis
       #xAB8D1C87E51D0D441BE8B3DD5B05C8795B48875DFFE00B7FFCFAC23010D3A395)
     (define sig11 (signature r1bis s1))

     (test-case "# Test case 1.1: false signature r"
                (check-false (verify sig11 pub1 z1)))

     (define s1bis
       #x68242CEFF8935EDEDD102DD876FFD6BA72D6A427A3EDB13D26EB0781CB423C4)
     (define sig12 (signature r1 s1bis))

     (test-case "# Test case 1.2: false signature s"
                (check-false (verify sig12 pub1 z1)))

     (define z1bis
       #xEC208BAA0FC1C29F708A9CA96FDEFF3AC3F230BB4A7BA4AEDE4942AD003C0F60)

     (test-case "# Test case 1.3: false message"
                (check-false (verify sig1 pub1 z1bis)))

     (define pub1bis
       (point
        (field-element
         #x887386E452B8EACC4ACFDE10D9AAF7F6D9A0F975AABB10D006E4DA568744D06C
         P)
        (field-element
         #x61DE6D95231CD89026E286DF3B6AE4A894A3378E393E93A0F45B666329A0AE34
         P)
        secp256k1))

     (test-case "# Test case 1.4: false pub key x"
                (check-false (verify sig1 pub1bis z1)))

     (define pub1bisbis
       (point
        (field-element
         #x887387E452B8EACC4ACFDE10D9AAF7F6D9A0F975AABB10D006E4DA568744D06C
         P)
        (field-element
         #x61DE6D94231CD89026E286DF3B6AE4A894A3378E393E93A0F45B666329A0AE34
         P)
        secp256k1))

     (test-case "# Test case 1.4bis: false pub key y"
                (check-false (verify sig1 pub1bisbis z1)))

     (define z2
       #x7C076FF316692A3D7EB3C3BB0F8B1488CF72E1AFCD929E29307032997A838A3D)
     (define r2
       #xEFF69EF2B1BD93A66ED5219ADD4FB51E11A840F404876325A1E8FFE0529A2C)
     (define s2
       #xC7207FEE197D27C618AEA621406F6BF5EF6FCA38681D82B2F06FDDBDCE6FEAB6)
     (define sig2 (signature r2 s2))

     (test-case "# Test case 2: different signature and same pubkey"
                (check-true (verify sig2 pub1 z2)))

     (define e3 (generate_random)) ; a private key is just a random number
     (define pub3 (rmul_point G e3)) ; G*e to get the public key
     (define z3 (generate_random)) ; just a random message
     (define sig3 (sign e3 z3))

     (test-case "# Test case 3: sign and verify"
                (check-true (verify sig3 pub3 z3))))))

(run-tests test-ecdsa)
