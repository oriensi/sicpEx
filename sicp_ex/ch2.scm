;; 2.7
(define (make-center-interval m n)
  (make-interval (- m n) (+ m n)))
(define (make-interval a b)
  (cons a b))
(define (upper-bound interval)
  (if (> (car interval) (cdr interval))
      (car interval)
      (cdr interval)))
(define (lower-bound interval)
  (if (< (car interval) (cdr interval))
      (car interval)
      (cdr interval)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (make-interval (* (lower-bound x) (lower-bound y))
                 (* (upper-bound x) (upper-bound y))))
(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))
;;
(define (part1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (part2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))


(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))
;; 2.17
(define (last-pair items)
  (cond ((null? items)       '())
        ((null? (cdr items))  items)
        (else (last-pair (cdr items)))))
;; 2.18
(define (reverse items)
  (define (reverse-item org obj)
    (if (null? org)
        obj
        (reverse-item (cdr org) (cons (car org) obj))))
  (reverse-item items '()))
;; 利用 append 构建形如
;; (append (append (cdr ()) (cons '())) (cons '()))
(define (reverse-d items)
  (if (null? items)
      '()
      (append (reverse-d (cdr items)) (cons (car items) '()))))
