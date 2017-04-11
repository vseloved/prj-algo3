(in-package :break-text)

;; Basic Graph's components
(defstruct (graph :conc-name)
  (nodes (make-hash-table))
  (nodes-count 0))

(defstruct node
  id label edges)

(defstruct edge
  src dst weight)

;; Basic graph operations
(defun get-node (id g)
  (gethash id (nodes g)))

(defun add-node (label g)
  (let ((node (make-node :id (nodes-count g) :label label)))
    (setf (gethash (nodes-count g) (nodes g)) node)
    (incf (nodes-count g))
    node))

(defun add-edge (src dst weight g)
  (or (gethash src (nodes g))
      (error "There is no src node"))
  (or (gethash dst (nodes g))
      (error "There is no dst node"))
  (push (make-edge :src src :dst dst :weight weight)
	(node-edges (gethash src (nodes g)))))
