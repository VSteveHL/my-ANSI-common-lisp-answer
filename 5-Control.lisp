(declaim (optimize (debug 3)))

;; 1
((lambda (x) (cons x x)) (car y))
(lambda (x) )

((lambda (x) (cons x (+ x z))) (car x))

;; 2
(defun mystery (x y)
  (unless (null y)
    (cond ((eql (car y) x) 0)
          (t (let ((z (mystery x (cdr y))))
               (and z (+ z 1)))))))

;; 3
(defun my-square (x)
  (if (and (integerp x) (<= x 5))
      x
      (* x x)))

(my-square 2)

;TODO: ;; 4
(defun my-month-num (m y)
  )

;; 5
;;; 迭代
(defun precedes (x y)
  (loop for i from 1 below (length y)
        when (equal x (char y i))
          collect (char y (1- i))))

;;; 递归
(defun precedes (x y)
  (when (> (length y) 1)
    (let ((letter (char y 1))
          (other (precedes x (subseq y 1))))
      (if (equal x (char y 1))
          (cons (char y 0) other)
          other))))

(precedes #\a "abracadabra")

;; 6
;;; 迭代
(defun intersperse (elt lst)
  (let (out
        (lst (reverse lst)))
    (dolist (item lst (cdr out))
      (push item out)
      (push elt out))))

;;; 迭代
(defun intersperse (elt lst)
  (if (cdr lst)
    (append (list (car lst) elt)
            (intersperse elt (cdr lst)))
    lst))

(intersperse '- '(a b c d))

;; 7
;;; 辅助函数
(defun pair-p (x y)
  (equal 1 (abs (- x y))))

(pair-p 2 1)

;;; 递归
(defun my-pair (lst)
  (if (cdr lst)
    (and (pair-p (first lst) (second lst))
         (my-pair (cdr lst)))
    t))

(my-pair '(1 2 1 0 -1 -2 -3))
(my-pair '(1 2 4 3 5))

;;; do
(defun my-pair-2 (lst)
  (do ((num1 (first lst) (first lst))
       (num2 (cadr lst) (cadr lst))
       (lst lst (cdr lst))
       (out t))
      ((null num2) out)
    (setf out (and out (pair-p num1 num2)))))

(my-pair-2 '(1 2 1 0 -1 -2 -3))
(my-pair-2 '(1 2 4 3 5))

;;; mapc 和 return
;TODO: 没用到return啊
(defun my-pair-3 (lst)
  (let ((out t))
    (mapc (lambda (x y)
            (setf out (and (pair-p x y))))
          lst (cdr lst))
    out))

(my-pair-3 '(1 2 1 0 -1 -2 -3))
(my-pair-3 '(1 2 4 3 5))

;;; 写第三个的时候突然想出来的
(defun my-pair-4 (lst)
  (reduce (lambda (x y) (and x y)) (mapcar #'pair-p lst (cdr lst))))

(my-pair-4 '(1 2 1 0 -1 -2 -3))
(my-pair-4 '(1 2 4 3 5))

;; 8
(defun my-max-and-min (vec)
  (let ((num (svref vec 0)))
    (if (equal 1 (length vec))
        (values num num)
        (multiple-value-bind (my-max my-min) (my-max-and-min (subseq vec 1))
          (cond ((> num my-max) (setf my-max num))
                ((< num my-min) (setf my-min num)))
          (values my-max my-min)))))

(my-max-and-min #(1 2 3 5 4 -1))

;TODO: 9
