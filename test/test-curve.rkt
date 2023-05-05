#lang racket
(require rackunit
         rackunit/text-ui
         "../curve.rkt"
         "../field.rkt")

(define test-curve
  (test-suite
   "Tests for curve.rkt"
   (let ()
     (define p2
       (point
        (field-element
         #x9577FF57C8234558F293DF502CA4F09CBC65A6572C842B39B366F21717945116
         P)
        (field-element
         #x10B49C67FA9365AD7B90DAB070BE339A1DAF9052373EC30FFAE4F72D5E66D053
         P)
        secp256k1))
     (define e (+ (expt 2 240) (expt 2 31)))
     (test-case "Test with assert G*e = p2"
                (check-equal? (rmul_point G e) p2)))))

(run-tests test-curve)
