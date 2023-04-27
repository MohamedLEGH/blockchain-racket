#lang racket
(require "field.rkt")

(struct elliptic-curve (a b field) #:prefab)

;; parameters of the secp256k1 curve

; prime of the field
(define P #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F)

; max val on the curve : G*N = I
(define N #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141)

; parameter A = 0
(define A (field-element 0 P))

; parameter B = 7
(define B (field-element 7 P))

(define secp256k1 (elliptic-curve A B P))

(struct point (x y curve) #:prefab)

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

; Generator of the curve
(define G
  (point (field-element
          #x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798
          P)
         (field-element
          #x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8
          P)
         secp256k1))

; Identity for the addition
(define I (point null null secp256k1))

(define (add_point point1 point2)
  ; TODO: check if points are on the curve
  ; TODO: add else clause : error

  (cond
    [(equal? point1 I) point2]
    [(equal? point2 I) point1]
    [(and (equal? (point-x point1) (point-x point2))
          (equal? (point-y point1) (rmul_element (point-y point2) -1)))
     I]
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
     (define s
       (truediv_element (add_element (rmul_element (pow_element x1 2) 3) a)
                        (rmul_element y1 2)))
     ; x3 = s ** 2 - 2 * x1
     (define x3 (sub_element (pow_element s 2) (rmul_element x1 2)))
     ; y3 = s * (x1 - x3) - y1
     (define y3 (sub_element (mul_element s (sub_element x1 x3)) y1))
     (point x3 y3 secp256k1)]
     [else (error "cannot add the 2 points on the curve")]))

(define (binary_expansion value scalar)
  (cond
    [(equal? scalar 0) I]
    [(equal? scalar 1) value]
    [(equal? (modulo scalar 2) 1)
     (add_point value (binary_expansion value (- scalar 1)))]
    [else (binary_expansion (add_point value value) (/ scalar 2))]))

(define (rmul_point value scalar)
  ; TODO: check if point are on the curve
  (binary_expansion value scalar))

;; add test with assert G*e = p2

(define p2
  (point (field-element
          #x9577FF57C8234558F293DF502CA4F09CBC65A6572C842B39B366F21717945116
          P)
         (field-element
          #x10B49C67FA9365AD7B90DAB070BE339A1DAF9052373EC30FFAE4F72D5E66D053
          P)
         secp256k1))

(define e (+ (expt 2 240) (expt 2 31)))

(equal? (rmul_point G e) p2)

(provide (struct-out elliptic-curve)
         (struct-out point)
         on_curve?
         add_point
         rmul_point
         G
         P
         N
         secp256k1)
