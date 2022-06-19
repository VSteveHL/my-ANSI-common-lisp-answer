(declaim (optimize (debug 3)))

;; 1.
(equal  14        (+ (- 5 1) (+ 3 7)))
(equal '(1 5)     (list 1 (+ 2 3)))
(equal 7          (if (listp 1) (+ 1 2) (+ 3 4)))
(equal '(nil 3)   (list (and (listp 3) t) (+ 1 2)))

;; 2
(cons 'a '(b c))
(cons 'a (cons 'b '(c)))
(cons 'a (cons 'b (cons 'c nil)))

;; 3
(defun my-forth (lst)
  (car (cdr (cdr (cdr lst)))))

(equal 4 (my-forth '(1 2 3 4 5)))

;; 4
(defun bigger (x y)
  (if (> x y) x y))

(equal 2 (bigger 1 2))

;; 5
;;; (a) 传入一个list，如果list中有nil返回t
(defun enigma (x)
  (and (not (null x))
       (or (null (car x))
           (enigma (cdr x)))))

(equal t (enigma '(1 2 nil 3)))
(equal nil (enigma '(1 2 3)))
(equal nil (enigma '()))

;;; (b) 返回 x 在 y 中的序号，从 0 开始。如果找不到 x 则返回 nil
(defun mystery (x y)
  (if (null y)
      nil
      (if (eql (car y) x)
          0
          (let ((z (mystery x (cdr y))))
            (and z (+ z 1))))))

(equal 4 (mystery 16 '(1 2 4 8 16 32)))

;; 6
;;; x = car
(equal 'b (car (car (cdr '(a (b c) d)))))

;;; x = or
(equal 13 (or 13 (/ 1 0)))

;;; x = apply
(equal '(1) (apply #'list 1 nil))

;; 7
(defun have-list-p (lst)
  (and (not (null lst))
       (or (listp (car lst))
           (have-list-p (cdr lst)))))

(equal nil (have-list-p '(1 2 3)))
(equal t (have-list-p '(1 2 nil 3)))
(equal t (have-list-p '(1 2 '(3 4) 5)))

;; 8
;;; a 迭代
(defun print-dot-iter (num)
  (dotimes (i num)
    (format t "."))
  (format t "~%"))

(print-dot-iter 8)

;;; a 递归
(defun print-dot-recursion (num)
  (if (<= num 0)
      (format t "~%")
      (progn
        (format t ".")
        (print-dot-recursion (1- num)))))

(print-dot-recursion 8)

;;; b 迭代
(defun count-a-iter (lst)
  (let ((count 0))
    (dolist (i lst)
      (when (equal 'a i) (incf count)))
    count))

(equal 3 (count-a-iter '(a b a c d a)))
(equal 0 (count-a-iter '(b c e d f)))

;;; b 递归
(defun count-a-recursion (lst)
  (if (null lst)
      0
      (+ (if (equal 'a (car lst)) 1 0)
         (count-a-recursion (cdr lst)))))

(equal 3 (count-a-recursion '(a b a c d a)))
(equal 0 (count-a-iter '(b c e d f)))

;; 9
;;; a 的错误，remove 并不修改原始列表
(defun summit (lst)
  (setf lst (remove nil lst))
  (apply #'+ lst))

(equal 6 (summit '(1 2 nil 3)))

;;; b 的错误，没有终止条件导致无限循环
(defun summit (lst)
  (if (null lst)
      0
      (let ((x (car lst)))
        (if (null x)
            (summit (cdr lst))
            (+ x (summit (cdr lst)))))))

(equal 10 (summit '(1 2 nil 3 4)))

;; 完成时间 2022-06-04T10:52:56+08:00


