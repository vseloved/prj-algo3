(defstruct node
  id edges)

(defstruct edge
  src dst weight)

(defstruct (graph (:constructor %make-graph) (:conc-name nil))
  (nodes (make-hash-table)) (nodes-count 0))

(defun make-graph (&rest edges)
  (let ((g (%make-graph)))
    (dolist (edge edges)
      (destructuring-bind (src dst weight) edge
	(add-edge src dst weight g)))
    g))

(defun get-node (id g)
  (gethash id (nodes g)))

(defun get-nodes (g)
  (hash-keys (nodes g)))

(defun add-node (id g)
  (let ((node (make-node :id id)))
    (setf (gethash id (nodes g)) node)
    (incf (nodes-count g))
    node))

(defun add-edge (src dst weight g)
  (or (get-node src g) (add-node src g))
  (or (get-node dst g) (add-node dst g))
  (push (make-edge :src src :dst dst :weight weight)
	(node-edges (get-node src g))))

(defun adj-lists->adj-mat (g)
  (let* ((size (nodes-count g))
	 (mat (make-array (list size size) :initial-element -1)))
    (maphash (lambda (id node)
               (setf (aref mat id id) 0)
               (dolist (edge (node-edges node))
                 (setf (aref mat id (edge-dst edge))
                       (edge-weight edge))))
             (nodes g))
    mat))

;;; utils

(defun hash-keys (ht)
  (loop :for key being the hash-keys :of ht :collect key))

(defun 2d-array-x-length (a)
  (car (array-dimensions a)))

(defun 2d-array-y-length (a)
  (cadr (array-dimensions a)))

(defun print-2d-array (a)
  (loop :for i :from 0 :below (2d-array-y-length a) :do
     (loop :for j :from 0 :below (2d-array-x-length a) :do
	(format t "~2d ~^" (aref a i j)))
     (format t "~%")))
