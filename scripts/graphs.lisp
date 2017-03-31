(ql:quickload :cl-dot)
(ql:quickload :rutilsx)

(use-package :rutilsx)

(defstruct (edge (:conc-name nil))
  src dst label)

(defstruct (node (:conc-name nil)
                 (:print-object
                  (lambda (node stream)
                    (format stream "#~A{~{~A~^ ~}}"
                            (name node)
                            (mapcar (lambda (edge)
                                      (format nil "~A~@[/~A~]"
                                              (name (dst edge))
                                              (label edge)))
                                    (edges node))))))
  id name edges)

(defstruct (graph (:conc-name nil))
  (nodes (make-hash-table :test 'equal)))

(defun graph (&rest edges)
  (let ((g (make-graph))
        (i -1))
    (dolist (edge edges)
      (with (((src-name dst-name &optional edge-label) edge)
             (src (getsethash src-name (nodes g)
                              (make-node :id (incf i) :name src-name)))
             (dst (getsethash dst-name (nodes g)
                              (make-node :id (incf i) :name dst-name))))
        (push (make-edge :src src :dst dst :label edge-label)
              (edges src))))
    g))

(defun adj-mat (g)
  (with ((node-count (hash-table-count (nodes g)))
         (mat (make-array (list node-count node-count)
                          :initial-element most-positive-fixnum)))
         ;; (display (make-array (list (1+ node-count) (1+ node-count)))))
    (maphash (lambda (name node)
               (setf (aref mat (id node) (id node)) 0)
               (dolist (edge (edges node))
                 (setf (aref mat
                             (id node)
                             (id (dst edge)))
                       (label edge))))
             (nodes g))
    ;; (maphash (lambda (name node)
    ;;            (setf (aref display 0 (1+ (id node))) name
    ;;                  (aref display (1+ (id node)) 0) name)
    ;;            (dolist (edge (edges node))
    ;;              (setf (aref display (1+ (id node)) (1+ (id (dst edge))))
    ;;                    (setf (aref mat (id node) (id (dst edge)))
    ;;                          (label edge)))))
    ;;          (nodes g))
    ;; (print display)
    mat))


;;; drawing a graph

(defun draw-graph (graph path)
  "Draws GRAPH to a PNG file at PATH."
  (cl-dot:dot-graph graph path :format :png)
  path)

(defmethod cl-dot:graph-object-node ((graph graph) object)
  (make-instance 'cl-dot:node
                 :attributes (list :label (princ-to-string (name object)))))

(defmethod cl-dot:graph-object-edges ((graph graph))
  (let ((edges (make-array 0 :adjustable t :fill-pointer t)))
    (maphash (lambda (id node)
               (dolist (edge (edges node))
                 (vector-push-extend edge edges)))
             (nodes graph))
    edges))

(defmethod cl-dot::nodes-of ((graph graph))
  (let (nodes)
    (maphash (lambda (name node)
               (push node nodes))
             (nodes graph))
    (reverse nodes)))

(defmethod cl-dot::edges-of ((graph graph))
  (let (edges)
    (maphash (lambda (name node)
               (dolist (edge (edges node))
                 (push edge edges)))
             (nodes graph))
    (reverse edges)))

(setf cl-dot::*graph-attributes*
      (cons (list :rankdir 'cl-dot::text)
            cl-dot::*graph-attributes*))

(defmethod cl-dot::attributes-of ((graph graph))
  (list :rankdir "LR"))

(defmethod cl-dot::id-of ((node node))
  (name node))

(defmethod cl-dot::id-of (node)
  node)

(defmethod cl-dot::attributes-of ((node node))
  nil)
  
(defmethod cl-dot::source-of ((edge edge))
  (src edge))

(defmethod cl-dot::source-port-of ((edge edge))
  nil)

(defmethod cl-dot::target-of ((edge edge))
  (dst edge))

(defmethod cl-dot::target-port-of ((edge edge))
  nil)

(defmethod cl-dot::attributes-of ((edge edge))
  (list :label (label edge)))


;;; topological sort

(defun topo-sort (graph &optional
                          (unvisited (copy-hash-table (nodes graph)))
                          (rez (make-array 0 :adjustable t :fill-pointer t))
                          (node (block take-first
                                  (maphash (lambda (id node)
                                             (return-from take-first node))
                                           unvisited))
                                recursive-p))
  (remhash (id node) unvisited)
  (dolist (edge (edges node))
    (let ((child (dst edge)))
      (when (gethash (id child) unvisited)
        (topo-sort graph unvisited rez child))))
  (vector-push-extend node rez)
  (when (and (not recursive-p)
             (> (hash-table-count unvisited) 0))
    (topo-sort graph unvisited rez))
  rez)
