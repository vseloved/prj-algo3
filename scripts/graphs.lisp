(ql:quickload :cl-dot)
(ql:quickload :rutilsx)

(use-package :rutil)

(defstruct (node (:conc-name nil)
                 (:print-object
                  (lambda (node stream)
                    (format stream ":~A(~{~A~^ ~})"
                            (id node)
                            (mapcar (lambda (node-weight)
                                      (format nil "~A~@[/~A~]"
                                              (id (lt node-weight))
                                              (rt node-weight)))
                                    (children node))))))
  id
  children)

(defclass graph ()
  ((nodes :initarg :nodes :initform (make-hash-table) :accessor nodes)))

(defun make-graph (&rest edges)
  (let ((g (make-instance 'graph)))
    (dolist (edge edges)
      (with (((head-id child-id &optional weight) edge)
             (head (getsethash head-id (nodes g)
                               (make-node :id head-id)))
             (child (getsethash child-id (nodes g)
                                (make-node :id child-id))))
        (push (pair child weight) (children head))))
    g))


;;; drawing a graph

(defmethod cl-dot:graph-object-node ((graph graph) object)
  (make-instance 'cl-dot:node
                 :attributes (list :label (format nil "~A" (id object)))))

(defmethod cl-dot:graph-object-edges ((graph graph))
  (let ((edges (make-array 0 :adjustable t :fill-pointer t)))
    (maphash (lambda (k v)
               (dolist (node-weight (children v))
                 (vector-push-extend (list k (id (lt node-weight))
                                           :label (rt node-weight))
                                     edges)))
             (nodes graph))
    edges))

(defmethod cl-dot::nodes-of ((graph graph))
  (let (rez)
    (maphash (lambda (k v)
               (push v rez))
             (nodes graph))
    (reverse rez)))

(defmethod cl-dot::edges-of ((graph graph))
  (let (rez)
    (maphash (lambda (k v)
               (dolist (node-weight (children v))
                 (push (list k (id (lt node-weight)) (rt node-weight))
                       rez)))
             (nodes graph))
    (reverse rez)))

(setf cl-dot::*graph-attributes*
      (cons (list :rankdir 'cl-dot::text)
            cl-dot::*graph-attributes*))

(defmethod cl-dot::attributes-of ((graph graph))
  (list :rankdir "LR"))

(defmethod cl-dot::id-of ((node node))
  (id node))

(defmethod cl-dot::id-of ((node number))
  node)

(defmethod cl-dot::attributes-of ((node node))
  nil)
  
(defmethod cl-dot::source-of ((edge list))
  (first edge))

(defmethod cl-dot::source-port-of ((edge list))
  nil)

(defmethod cl-dot::target-of ((edge list))
  (second edge))

(defmethod cl-dot::target-port-of ((edge list))
  nil)

(defmethod cl-dot::attributes-of ((edge list))
  (list :label (third edge)))


;;; topological sort

(defun topo-sort (graph &optional
                          (unvisited (copy-hash-table (nodes graph)))
                          (rez (make-array 0 :adjustable t :fill-pointer t))
                          (node (block take-first
                                  (maphash (lambda (k v)
                                             (return-from take-first v))
                                           unvisited))
                                recursive-p))
  (remhash (id node) unvisited)
  (dolist (node-weight (children node))
    (let ((child (first node-weight) ))
      (when (gethash (id child) unvisited)
        (topo-sort graph unvisited rez child))))
  (print node)
  (vector-push-extend node rez)
  (when (and (not recursive-p)
             (> (hash-table-count unvisited) 0))
    ;; some more unvisited nodes remain
    (topo-sort graph unvisited rez))
  rez)
