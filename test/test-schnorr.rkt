#lang racket
(require rackunit
         rackunit/text-ui
         "../crypto.rkt"
         "../curve.rkt"
         "../field.rkt")

(define test-schnorr
  (test-suite
   "Tests for schnorr.rkt"
   (let ()
     (define e4 (generate_random)) ; a private key is just a random number
     (define pub4
       (pub_to_pubschnorr (rmul_point G e4))) ; G*e to get the public key
     (define z4 (generate_random)) ; just a random message
     (define sig4 (sign_schnorr e4 z4))
     (test-case "# Test case 4: sign and verify Schnorr"
                (check-true (verify_schnorr sig4 pub4 z4)))

     (define e5
       #x0000000000000000000000000000000000000000000000000000000000000003) ; a private key is just a random number
     (define pub5
       "F9308A019258C31049344F85F89D5229B531C845836F99B08601F113BCE036F9") ; G*e to get the public key
     (define z5
       #x0000000000000000000000000000000000000000000000000000000000000000) ; just a random message
     (define sig5_compute (sign_schnorr e5 z5))
     (define sig5
       "E907831F80848D1069A5371B402410364BDF1C5F8307B0084C55F1CE2DCA821525F66A4A85EA8B71E482A74F382D2CE5EBEEE8FDB2172F477DF4900D310536C0")

     (test-case "# Test case 5: verify Schnorr"
                (check-true (verify_schnorr sig5 pub5 z5))
                (check-true (verify_schnorr sig5_compute pub5 z5)))

     (define e5bis
       #x0000000000000000000000000000000000000000000000000000000000000006) ; a private key is just a random number
     (define pub5bis
       "fff97bd5755eeea420453a14355235d382f6472f8568a18b2f057a1460297556") ; G*e to get the public key
     (define sig5_computebis (sign_schnorr e5bis z5))

     (test-case "# Test case 5bis: verify Schnorr : odd y pubkey"
                (check-true (verify_schnorr sig5_computebis pub5bis z5)))

     (define sig5bis
       "E807831F80848D1069A5371B402410364BDF1C5F8307B0084C55F1CE2DCA821525F66A4A85EA8B71E482A74F382D2CE5EBEEE8FDB2172F477DF4900D310536C0")

     (test-case "# Test case 5.1: false signature s"
                (check-false (verify_schnorr sig5bis pub5 z5)))

     (define z5bis
       #xEC208BAA0FC1C29F708A9CA96FDEFF3AC3F230BB4A7BA4AEDE4942AD003C0F60)

     (test-case "# Test case 5.2: false message"
                (check-false (verify_schnorr sig5 pub5 z5bis)))

     (define pub53
       (point
        (field-element
         #x887386E452B8EACC4ACFDE10D9AAF7F6D9A0F975AABB10D006E4DA568744D06C
         P)
        (field-element
         #x61DE6D95231CD89026E286DF3B6AE4A894A3378E393E93A0F45B666329A0AE34
         P)
        secp256k1))
     (define pub53_val (pub_to_pubschnorr pub53)) ;

     (test-case "# Test case 5.3: false pub key x"
                (check-false (verify_schnorr sig5 pub53_val z5)))

     (define pub5bisbis
       (point
        (field-element
         #x887387E452B8EACC4ACFDE10D9AAF7F6D9A0F975AABB10D006E4DA568744D06C
         P)
        (field-element
         #x61DE6D94231CD89026E286DF3B6AE4A894A3378E393E93A0F45B666329A0AE34
         P)
        secp256k1))
     (define pub5bisbis_val (pub_to_pubschnorr pub5bisbis)) ;

     (test-case "# Test case 5.4: false pub key y"
                (check-false (verify_schnorr sig5 pub5bisbis_val z5))))))

(run-tests test-schnorr)
