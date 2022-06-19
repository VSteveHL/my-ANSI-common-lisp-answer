(declaim (optimize (debug 3)))

;; 1
(defun quarter-turn (lst)
  (let* ((dim (array-dimensions lst))
        (len (car dim))
        (out (make-array dim)))
    (dotimes (i len)
      (dotimes (j len)
        (setf (aref out j (- len 1 i))
              (aref lst i j))))
    out))

(quarter-turn #2A((a b) (c d)))
(quarter-turn #2A((a b c) (d e f) (g h i)))

;; 2
(defun my-copy-list (lst)
  (reduce (lambda (x y) (append x (list y))) lst :initial-value nil))

(my-copy-list '(a b (c d) e))

(defun my-reverse (lst)
  (reduce (lambda (x y) (cons y x)) lst :initial-value nil))

(my-reverse '(a b c))


;; 3
(declaim (optimize (debug 3)))

(defstruct node
  elt
  (lchild nil)
  (mchild nil)
  (rchild nil))

(defun random-tree (depth)
  "for test"
  (when (> depth 0)
    (let ((num (random 10)))
      (make-node :elt num
                 :lchild (random-tree (1- depth))
                 :mchild (random-tree (1- depth))
                 :rchild (random-tree (1- depth))))))

(random-tree 3)

(defun my-copy (tree)
  (when tree
    (make-node :elt (node-elt tree)
               :lchild (my-copy (node-lchild tree))
               :mchild (my-copy (node-mchild tree))
               :rchild (my-copy (node-rchild tree)))))

(let ((x (random-tree 2)))
  (print x)
  (print (my-copy x)))

(defun my-tree-member (obj tree)
  (when tree
    (or (equal obj (node-elt tree))
        (my-tree-member obj (node-lchild tree))
        (my-tree-member obj (node-mchild tree))
        (my-tree-member obj (node-rchild tree)))))

(let ((x (random 15))
      (tree (random-tree 2)))
  (print (my-tree-member x tree))
  (print x)
  (print tree))

                                        ;TODO: 4

                                        ;TODO: 5

;; 6
(defun assoc-to-hash (lst)
  (let ((out (make-hash-table)))
    (dolist (elt lst)
      (setf (gethash (car elt) out) (cdr elt)))
    out))

(defun hash-to-assoc (hash-table)
  (let ((out nil))
    (maphash (lambda (k v)
               (push (cons k v) out))
             hash-table)
    out))

(defparameter *temp* '((a . 1) (b . 2) (c . 3)))
(assoc 'a *temp*)
(assoc-to-hash *temp*)

(maphash (lambda (k v) (cons k v)) (assoc-to-hash *temp*))
(hash-to-assoc (assoc-to-hash *temp*))

