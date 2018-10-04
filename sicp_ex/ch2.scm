(define nil '())
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

;; 找零
(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount (- kinds-of-coins 1))
                 (cc (- amount (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))
(define (length list)
  (if (null? list)
      0
      (+ (length (cdr list)) 1)))
(define (get-item items index)
  (if (= index 0)
      (car items)
      (get-item (cdr items) (- index 1))))
(define (get-rindex items index)
  (get-item (reverse items) index))

(define (cc-2 amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (null? coin-values)) 0)
        (else (+ (cc-2 amount (cdr coin-values))
                 (cc-2 (- amount (car coin-values)) coin-values)))))

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
;; (define x (list (list 1 2) (list 3 4)))
(define (fringe x)
  (cond ((null? x) '())
        ((not (pair? x)) (list x))
        (else (append (fringe (car x)) (fringe (cdr x))))))

;; 2.29 二叉活动体, 长度 × 重量
;; list
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))
(define (left-branch tree)
  (car tree))
(define (right-branch tree)
  (car (cdr tree)))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))
;; 2.29 cons
;; (define (make-mobile left right)
;;   (cons left right))
;; (define (make-branch length structure)
;;   (cons length structure))
;; (define (right-branch tree)
;;   (cdr tree))
;; (define (branch-structure branch)
;;   (cdr branch))
(define (total-weight tree)
  (if (not (pair? tree))
      tree
      (+ (if (pair? (branch-structure (left-branch tree)))
             (total-weight (branch-structure (left-branch tree)))
             (branch-structure (left-branch tree)))
         (if (pair? (branch-structure (right-branch tree)))
             (total-weight (branch-structure (right-branch tree)))
             (branch-structure (right-branch tree))))))
(define (balence? tree)
  (if (not (pair? tree))
      #t
      (if
       (= (* (branch-length (left-branch tree))
             (total-weight (branch-structure (left-branch tree))))
          (* (branch-length (right-branch tree))
             (total-weight (branch-structure (right-branch tree)))))
       (and (balence? (branch-structure (left-branch tree)))
            (balence? (branch-structure (right-branch tree))))
       #f)))

;; 2.30
(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (* sub-tree sub-tree)))
       tree))

;; 2.31
(define (tree-map fun tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map fun sub-tree)
             (fun sub-tree)))
       tree))
(define (square-tree-map tree)
  (tree-map (lambda (x)
              (* x x))
            tree))

;; 2.32 生成子集
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x)
                            (cons (car s) x))
                          rest)))))
;; (subsets (list 1 2 3))
;; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

;; 2.33
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (map2 p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))
(define (append2 seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length2 sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))
(define (square-tree2 tree)
  (map2
   (lambda (sub-tree)
     (if (pair? sub-tree)
         (square-tree2 sub-tree)
         (* sub-tree sub-tree)))
   tree))
;; (square-tree2 (list 1 (list 2 (list 3 4) 5) (list 6 7)))
;; (1 (4 (9 16) 25) (36 49))

;; 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))
;; (horner-eval 2 (list 1 3 0 5 0 1)) ; 当x=2时, 1 + 3x + 5x^3 + x^5
;; 79

;; 2.35
(define (count-leaves2 t)
  (accumulate (lambda (x y)
                (+ x y))                ; 累积每个子树的叶子数
              0
              (map (lambda (x)
                     (if (pair? x)
                         (count-leaves2 x)
                         1))
                   t)))
;; (count-leaves2 (cons (list 1 2) (list 3 4)))
;; 4

;; 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map (lambda (x)
                                       (car x))
                                     seqs))
            (accumulate-n op init (map (lambda (x)
                                         (cdr x))
                                       seqs)))))
;; (define n (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
;; (accumulate-n + 0 n)
;; (22 26 30)

;; 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define (matrix-*-vector m v)
  (map (lambda (x)
         (dot-product x v))
       m))
;; 行列变换 n_{ij} = m_{ji}
(define (transpose mat)
  (accumulate-n cons '() mat))
;; 矩阵相乘 p_{ij} = ∑_{k}m_{ik}n_{kj}
;; (define (matrix-*-matrix m n)
;;   (let ((cols (transpose n)))
;;     (map (lambda (m-row)
;;            (map (lambda (n-col)
;;                   (accumulate + 0 (map * n-col m-row)))
;;                 cols))
;;          m)))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (matrix-*-vector cols row))
         m)))

;; 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
(define fold-right accumulate)
;; 如需要 fold-left fold-right 执行如果相同 op 需要满足「交换律」交换操作符两边的操作数不影响运算结果

;; 2.39
(define (reverse sequence)
  (fold-right (lambda (x y)
                (if (null? y)
                    (list x)
                    (append y (cons x nil))))
              nil sequence))
;; (define (reverse sequence)
;;   (fold-left (lambda (x y) (cons y x)) nil sequence))

;; 嵌套映射
(define (enumerate-interval start end)
  (define (range ret start end)
    (cond
     ((> start end) nil)
     ((= start end) (cons start ret))
     (else (range (cons end ret) start (- end 1)))))
  (range nil start end))

;; (accumulate append
;;             nil
;;             (map (lambda (i)
;;                    (map (lambda (j) (list i j)) ; (() /* 1时nil */ ((2 1)) /* 2时(2 1) */ ...)
;;                         (enumerate-interval 1 (- i 1))))
;;                  (enumerate-interval 1 n))) ; (1 2 3)
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
(define (prime? x)
  (define (match? i)
    (cond ((= i x) #t)
          ((> (* i i) x) #t)
          ((= (remainder x i) 0) #f)
          (else (match? (+ i 1)))))
  (match? 2))
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

(define (remove x s)
  (filter (lambda (i) (= i x)) s))
(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p)
                        (cons x p))
                      (permutations remove x s))
                 s))))

;; 2.40
;; (define (unique-pairs n)
;;   (accumulate
;;    append
;;    nil
;;    (map (lambda (i)
;;           (map (lambda (j)
;;                  (list i j))
;;                (enumerate-interval 1 (- i 1))))
;;         (enumerate-interval 1 n))))
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

;; 2.41
(define (append-mulit-2-1-list result multi-list)
    (cond ((null? multi-list) result)
          ((null? (car multi-list)) (append-mulit-2-1-list result (cdr multi-list)))
          ((not (list? (car multi-list))) (append multi-list result))
          ((not (list? (car (car multi-list)))) (append multi-list result))
          (else (append-mulit-2-1-list (append-mulit-2-1-list result (car multi-list)) (cdr multi-list)))))
(define (sum-list list)
  (define (sum-iter ret list)
    (if (null? list)
        ret
        (sum-iter (+ ret (car list)) (cdr list))))
  (sum-iter 0 list))
(define (three-sum-equal max s)
  (filter (lambda (i)
            (= s (sum-list i)))
          (append-mulit-2-1-list
           nil
           (flatmap (lambda (i)
                      (map (lambda (j)
                             (map (lambda (k)
                                    (list i j k))
                                  (enumerate-interval 1 (- j 1))))
                           (enumerate-interval 1 (- i 1))))
                    (enumerate-interval 1 max)))))
