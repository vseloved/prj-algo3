(defun make-tree (edge-list)
  (let* ((edges-count (length edge-list))
         (first-edge  (first edge-list))
         (rest-edges  (cdr edge-list))
         (tree        (list (car first-edge)
                            (cdr first-edge))))
    (labels ((add-edge (subtree edge)
               (let ((u (first edge))
                     (v (second edge)))
                 (when (and subtree (consp subtree))
                   (cond ((eql (car subtree) u)
                          (push (list v) (cdr subtree))
                          (setf rest-edges (delete edge rest-edges)))
                         (t
                          (add-edge (car subtree) edge)
                          (add-edge (cdr subtree) edge)))))))
      (loop
         :while (> edges-count (length rest-edges))
         :do (progn
               (setf edges-count (length rest-edges))
               (loop
                  :for edge :in rest-edges
                  :do (add-edge tree edge))))
      (values tree
              rest-edges))))

(defmacro topo-sort-algorithm (tree &body visit-form)
  `(let* ((traversed))
     (labels ((traverse (subtree)
                (when subtree
                  (if (consp subtree)
                      (progn (traverse (cdr subtree))
                             (traverse (car subtree)))
                      (progn ,@visit-form)))))
       (traverse ,tree)
       (reverse traversed))))

(defgeneric topo-sort (tree collect-form)
  (:method (tree (collect-form (eql :simple-pushnew)))
    (declare (ignore collect-form))
    (topo-sort-algorithm tree (pushnew subtree traversed)))
  (:method (tree (collect-form (eql :ht-query-and-push)))
    (declare (ignore collect-form))
    (let ((visited (make-hash-table)))
      (topo-sort-algorithm tree (unless (gethash subtree visited)
                                  (push subtree traversed)
                                  (setf (gethash subtree visited) t))))))

