#lang racket
(require math/number-theory)

(struct galois-field (prime) #:prefab)

(define (in_field? gf value)
  (and (<= 0 value) (< value (galois-field-prime gf))))

(struct field-element (value field) #:prefab)

(define (add_element fe1 fe2)
  (field-element (with-modulus (galois-field-prime (field-element-field fe1)) (mod+ (field-element-value fe1) (field-element-value fe2))) (field-element-field fe1)))

(define (sub_element fe1 fe2)
  (field-element (with-modulus (galois-field-prime (field-element-field fe1)) (mod- (field-element-value fe1) (field-element-value fe2))) (field-element-field fe1)))

(define (rmul_element fe1 scalar)
  (field-element (modulo (* (field-element-value fe1) scalar) (galois-field-prime (field-element-field fe1))) (field-element-field fe1)))

(define (mul_element fe1 fe2)
  (field-element (with-modulus (galois-field-prime (field-element-field fe1)) (mod* (field-element-value fe1) (field-element-value fe2))) (field-element-field fe1)))

(define (pow_element fe1 exponent)
  (field-element (with-modulus (galois-field-prime (field-element-field fe1)) (modexpt (field-element-value fe1) exponent)) (field-element-field fe1)))

(define (truediv_element fe1 fe2)
  (field-element (with-modulus (galois-field-prime (field-element-field fe1)) (mod/ (field-element-value fe1) (field-element-value fe2))) (field-element-field fe1)))

(struct elliptic-curve (a b field) #:prefab)

(define (on_curve? point_val ec)
  (define x (point-x point_val))
  (define y (point-y point_val))
  (define a (elliptic-curve-a ec))
  (define b (elliptic-curve-b ec))
  (define ysquare (pow_element y 2))
  (define xcube (pow_element x 3))
  (define ax (mul_element a x))
  ; y^2 = x^3 + ax + b
  (equal? ysquare (add_element (add_element xcube ax) b)))

(define P #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F)

(define field_P (galois-field P))

(define A (field-element 0 field_P))
(define B (field-element 7 field_P))

(define secp256k1 (elliptic-curve A B field_P))

(struct point (x y curve) #:prefab)

(define G (point (field-element #x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798 field_P) (field-element #x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8 field_P) secp256k1))

(define N #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141)

(define (add_point point1 point2)
  (cond [(equal? point1 I) point2]
        [(equal? point2 I) point1]
        [(and (equal? (point-x point1) (point-x point2)) (equal? (point-y point1) (rmul_element (point-y point2) -1))) I]
        [(not (equal? (point-x point1) (point-x point2)))
          (define x1 (point-x point1))
          (define x2 (point-x point2))
          (define y1 (point-y point1))
          (define y2 (point-y point2))
          ; s = (y2 - y1) / (x2 - x1)
          (define s (truediv_element (sub_element y2 y1) (sub_element x2 x1)))
          ; x3 = s ** 2 - x1 - x2
          (define x3 (sub_element (sub_element (pow_element s 2) x1) x2))
          ; y3 = s * (x1 - x3) - y1
          (define y3 (sub_element (mul_element s (sub_element x1 x3)) y1))
          (point x3 y3 secp256k1)]
        [(and (equal? point1 point2) (equal? (point-y point1) +inf.0)) I]
        [(equal? point1 point2)
            ;x1, y1, a = self.x, self.y, self.curve.a
            (define x1 (point-x point1))
            (define y1 (point-y point1))
            (define a (elliptic-curve-a (point-curve point1)))
            ; s = (3 * x1 ** 2 + a) / (2 * y1)
            (define s (truediv_element (add_element (rmul_element (pow_element x1 2) 3) a) (rmul_element y1 2)))
            ; x3 = s ** 2 - 2 * x1
            (define x3 (sub_element (pow_element s 2) (rmul_element x1 2)))
            ; y3 = s * (x1 - x3) - y1
            (define y3 (sub_element (mul_element s (sub_element x1 x3)) y1))
            (point x3 y3 secp256k1)]))

(define I (point null null secp256k1))

(define (binary_expansion value scalar)
  (cond [(equal? scalar 0) I]
        [(equal? scalar 1) value]
        [(equal? (modulo scalar 2) 1) (add_point value (binary_expansion value (- scalar 1)))]
        [else (binary_expansion (add_point value value) (/ scalar 2))]))

(define (rmul_point value scalar)
    (binary_expansion value scalar))

(define p2 (point (field-element #x9577FF57C8234558F293DF502CA4F09CBC65A6572C842B39B366F21717945116 field_P) (field-element #x10B49C67FA9365AD7B90DAB070BE339A1DAF9052373EC30FFAE4F72D5E66D053 field_P) secp256k1))

(define e (+ (expt 2 240) (expt 2 31)))

(provide (struct-out galois-field)
         (struct-out field-element)
         (struct-out elliptic-curve)
         (struct-out point)
         in_field? add_element sub_element rmul_element mul_element
         pow_element truediv_element
         add_point rmul_point on_curve?
         I G N p2 e secp256k1)
