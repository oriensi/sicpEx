;; ch1

10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(= a b)
(if (and (> b a) (< b (* a b)))
    b
    a)
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
(+ 2 (if (> b a) b a))
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

;; 3个数中大的两个数的和
(define (sum2max3 x y z)
  (+ (if (> x y) x y)
     (if (> x y)
         (if (> y z) y z)
         (if (> x z) x z))))
;; (define (sum2max3-1 x y z)
;;   (- (+ x y z)
;;      (if (> x y)
;;          (if (> y z) z y)
;;          (if (> x z) z x))))

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; 应用序，会计算参数的值代入表达式，死循环
;; NEXT
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))
;; (test 0 (p))

;; TODO if 和 cond， 正则序应用序吧，还不清楚
;; 代码先上来
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (square x)
  (* x x))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (sqrt x)
  (sqrt-iter 1.0 x))
