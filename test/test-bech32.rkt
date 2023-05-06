#lang racket
(require rackunit
         rackunit/text-ui
         "../bech32.rkt")

(define test-bech32
  (test-suite
   "Tests for bech32.rkt"
   (let ()
     (test-case
      "# Test vector 1"
      (check-equal?
       (bech32_encode "751e76e8199196d454941c45d1b3a323f1433bd6" #:version 0)
       (string-downcase "BC1QW508D6QEJXTDG4Y5R3ZARVARY0C5XW7KV8F3T4"))
      (check-equal? (bech32_decode "BC1QW508D6QEJXTDG4Y5R3ZARVARY0C5XW7KV8F3T4")
                    "751e76e8199196d454941c45d1b3a323f1433bd6"))
     (test-case
      "# Test vector 2"
      (check-equal?
       (bech32_encode
        "1863143c14c5166804bd19203356da136c985678cd4d27a1b8c6329604903262"
        #:version 0
        #:hrp "tb")
       "tb1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3q0sl5k7")
      (check-equal?
       (bech32_decode
        "tb1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3q0sl5k7")
       "1863143c14c5166804bd19203356da136c985678cd4d27a1b8c6329604903262"))
     (test-case
      "# Test vector 3"
      (check-equal?
       (bech32_encode
        "751e76e8199196d454941c45d1b3a323f1433bd6751e76e8199196d454941c45d1b3a323f1433bd6")
       "bc1pw508d6qejxtdg4y5r3zarvary0c5xw7kw508d6qejxtdg4y5r3zarvary0c5xw7kt5nd6y")
      (check-equal?
       (bech32_decode
        "bc1pw508d6qejxtdg4y5r3zarvary0c5xw7kw508d6qejxtdg4y5r3zarvary0c5xw7kt5nd6y")
       "751e76e8199196d454941c45d1b3a323f1433bd6751e76e8199196d454941c45d1b3a323f1433bd6"))
     (test-case "# Test vector 4"
                (check-equal? (bech32_encode "751e" #:version 16)
                              (string-downcase "BC1SW50QGDZ25J"))
                (check-equal? (bech32_decode "BC1SW50QGDZ25J") "751e"))
     (test-case
      "# Test vector 5"
      (check-equal?
       (bech32_encode "751e76e8199196d454941c45d1b3a323" #:version 2)
       "bc1zw508d6qejxtdg4y5r3zarvaryvaxxpcs")
      (check-equal? (bech32_decode "bc1zw508d6qejxtdg4y5r3zarvaryvaxxpcs")
                    "751e76e8199196d454941c45d1b3a323"))
     (test-case
      "# Test vector 6"
      (check-equal?
       (bech32_encode
        "000000c4a5cad46221b2a187905e5266362b99d5e91c6ce24d165dab93e86433"
        #:version 0
        #:hrp "tb")
       "tb1qqqqqp399et2xygdj5xreqhjjvcmzhxw4aywxecjdzew6hylgvsesrxh6hy")
      (check-equal?
       (bech32_decode
        "tb1qqqqqp399et2xygdj5xreqhjjvcmzhxw4aywxecjdzew6hylgvsesrxh6hy")
       "000000c4a5cad46221b2a187905e5266362b99d5e91c6ce24d165dab93e86433"))
     (test-case
      "# Test vector 7"
      (check-equal?
       (bech32_encode
        "000000c4a5cad46221b2a187905e5266362b99d5e91c6ce24d165dab93e86433"
        #:hrp "tb")
       "tb1pqqqqp399et2xygdj5xreqhjjvcmzhxw4aywxecjdzew6hylgvsesf3hn0c")
      (check-equal?
       (bech32_decode
        "tb1pqqqqp399et2xygdj5xreqhjjvcmzhxw4aywxecjdzew6hylgvsesf3hn0c")
       "000000c4a5cad46221b2a187905e5266362b99d5e91c6ce24d165dab93e86433"))
     (test-case
      "# Test vector 8"
      (check-equal?
       (bech32_encode
        "79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798")
       "bc1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vqzk5jj0")
      (check-equal?
       (bech32_decode
        "bc1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vqzk5jj0")
       "79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798")))))

(run-tests test-bech32)
