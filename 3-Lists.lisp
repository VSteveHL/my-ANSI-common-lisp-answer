(declaim (optimize (debug 3)))

;; 2
(defun new-union (x y)
  (let ((out nil))
    (dolist (i x) (push i out))
    (dolist (j y) (pushnew j out))
    (reverse out)))

(new-union '(a b c) '(b a d))

;; 3
(defun occurrences (lst)
  (let* ((unique-lst (remove-duplicates lst))
         (out (mapcar (lambda (x) (cons x 0)) unique-lst)))
    (dolist (i lst) (incf (cdr (assoc i out))))
    (sort out #'> :key #'cdr)))

(occurrences '(a b a d a c d c a))

;; 4
;;; 因为 (eql '(a) '(a)) => NIL
;;; 可以通过把 test 设置为 equal 来返回 t
(member '(a) '((a) (b)) :test #'equal)

;; 5
;;; 递归
(defun pos+recursion (lst)
  (when lst
    (cons (car lst) (mapcar #'1+ (pos+recursion (cdr lst))))))

(pos+recursion '(7 5 1 4))

;;; 迭代
(defun pos+iter (lst)
  (dotimes (i (length lst))
    (incf (nth i lst) i))
  lst)

(pos+iter '(7 5 1 4))

;;; mapcar
(defun pos+ (lst)
  (mapcar (let ((idx -1))
            (lambda (x) (incf idx) (+ x idx)))
          lst))

(pos+ '(7 5 1 4))

;; 6
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 一开始没读懂题，写了个用来辅助自己思考的函数，帮助理解题目
;; (defun my-trans (lst)
;;   (when lst
;;     (cons (my-trans (cdr lst)) (car lst))))
;; (my-trans '(a b c))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; cons
(defun new-cons (a b)
  (cons b a))

(new-cons 'a 'c)

;;; list
(defun new-list (&rest lst)
  (reduce #'cons (reverse lst) :initial-value nil))

(new-list 'a 'b 'c)

;;; length
(defun new-length (lst)
  (if lst (1+ (new-length (car lst))) 0))

(new-length (new-list 'a 'b 'c 'd))

;;; member
(defun new-member (x lst)
  (when lst
    (or (equal x (cdr lst)) (new-member x (car lst)))))

(new-member 'b (new-list 'a 'b 'c))

;; 7
;; 把最后一个函数的 list 换成 cons，这样可以少用一些 cons 对。

;; 8
(defun showdots (lst)
  (if (null lst)
      (format t "NIL")
      (progn
        (format t "(~A . " (car lst))
        (showdots (cdr lst))
        (format t ")"))))

(showdots '(a b c))

;; 9
(defparameter *net* '((a b c) (b c) (c a d e) (d f) (e g) (f g) (g d)))

(defun new-path (node path)
  (let ((start (car node))
        (rest-node (cdr node))
        (out nil))
    (dolist (elt rest-node)
      (dolist (item path)
        (when (and (not (member elt item)) (equal (car item) start))
          (push (cons elt item) out))))
    out))

(new-path '(a b c) '((a))) ; => ((C A) (B A))
(new-path '(c d e a) '((c a) (b a))) ; => ((A C A) (E C A) (D C A))
(new-path '(c d e a) '((c a) (b a))) ; => ((E C A) (D C A))


(defun all-path (start net)
  (let ((out (list (list start))))
    (dolist (elt net)
      (setf out (append (new-path elt out) out)))
    out))

(defun start-to-end-path (start end net)
  (let ((path (all-path start net)))
    (remove-if-not (lambda (x) (equal (car x) end)) path)))

(defun find-max-path (start end net)
  (let* ((path (start-to-end-path start end net))
         (max-len (apply #'max (mapcar #'length path))))
    (remove-if-not (lambda (x) (equal (length x) max-len)) path)))

(find-max-path 'a 'g *net*)
