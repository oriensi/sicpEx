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

;; 2.20 返回所有与起始元素奇偶性相同的元素
(define (if-append x y)
  (if (null? y)
      '()
      (if (x (car y))
          (cons (car y) (if-append x (cdr y)))
          (if-append x (cdr y)))))
(define (same-parity . w)
  (if (null? w)
      '()
      (let ((ou (remainder (car w) 2)))
        (if-append (lambda (x) (= ou (remainder x 2))) w))))

;; 2.21 map
(define (square-list items)
  (if (null? items)
      '()
      (cons (* (car items) (car items))
            (square-list (cdr items)))))
(define (square-list-map items)
  (map (lambda (x) (* x x)) items))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;; 2.25 取出7
(car (cdr (car (cdr (cdr (list 1 3 '(5 7) 9))))))
(car (car (list (list 7))))
(cadr (cadr (cadr (cadr (cadr (cadr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))))))))

;; 2.27 deep-reverse
(define (deep-reverse x)
  (cond ((null? x) '())
        ((pair? (car x)) (append (deep-reverse (cdr x)) (cons (deep-reverse (car x)) '())))
        (else (append (deep-reverse (cdr x)) (cons (car x) '())))))

;; 2.28 列出所有树叶
(define x (list (list 1 2) (list 3 4)))
(define (fringe x)
  (cond ((null? x) '())
        ((not (pair? (car x))) (cons (car x) (fringe (cdr x))))
        (else (append (fringe (car x)) (fringe (cdr x))))))

