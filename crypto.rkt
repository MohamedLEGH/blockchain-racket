#lang racket

; simple version
(define (inverse value prime)
 (for/first ([i (in-range prime)] #:when (= (modulo (* value i) prime) 1))
   i))

(define (euclidian_extended a b q r t1 t2 t3)
  (define new_a b)
  (define new_b r)
  (define new_q (quotient new_a new_b))
  (define new_r (remainder new_a new_b))
  (define new_t1 t2)
  (define new_t2 t3)
  (define new_t3 (- new_t1 (* new_q new_t2)))
  (if (= new_r 0) 
    new_t2
    (euclidian_extended new_a new_b new_q new_r new_t1 new_t2 new_t3)))

(define (inverse_2 value prime)
  ;(when (= value 1) 1)
  ;(when (= value 0) (error "0 have no inverse"))
  ;(when (>= value prime) (error "value is not in range"))
  (define first_a prime)
  (define first_b value)
  (define first_q (quotient first_a first_b))
  (define first_r (remainder first_a first_b))
  (define first_t1 0)
  (define first_t2 1)
  (define first_t3 (- first_t1 (* first_q first_t2)))
  (if (= first_r 0)
    (modulo first_t2 prime)
    (modulo (euclidian_extended first_a first_b first_q first_r first_t1 first_t2 first_t3) prime)))

(struct galois-field (prime) #:prefab)

(define (in_field gf value)
  (and (<= 0 value) (< value (galois-field-prime gf))))

(struct field-element (value field) #:prefab)

(define (add_element fe1 fe2)
  (field-element (modulo (+ (field-element-value fe1) (field-element-value fe2)) (galois-field-prime (field-element-field fe1))) (field-element-field fe1)))

(define (sub_element fe1 fe2)
  (field-element (modulo (- (field-element-value fe1) (field-element-value fe2)) (galois-field-prime (field-element-field fe1))) (field-element-field fe1)))

(define (rmul_element fe1 scalar)
  (field-element (modulo (* (field-element-value fe1) scalar) (galois-field-prime (field-element-field fe1))) (field-element-field fe1)))

(define (mul_element fe1 fe2)
  (field-element (modulo (* (field-element-value fe1) (field-element-value fe2)) (galois-field-prime (field-element-field fe1))) (field-element-field fe1)))

(define (pow_element fe1 exponent)
  (field-element (modulo (expt (field-element-value fe1) exponent) (galois-field-prime (field-element-field fe1))) (field-element-field fe1)))

(define (truediv_element fe1 fe2)
  (define inv_fe2 (inverse (field-element-value fe2) (galois-field-prime (field-element-field fe2))))
  (field-element (modulo (* (field-element-value fe1) inv_fe2) (galois-field-prime (field-element-field fe1))) (field-element-field fe1)))

(provide (struct-out galois-field)
         (struct-out field-element)
         in_field add_element sub_element rmul_element mul_element
         pow_element truediv_element inverse inverse_2)
