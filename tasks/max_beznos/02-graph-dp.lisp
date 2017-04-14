;;; topological sort using DFS
;;;
;;;  Expected complexity is more than O(V + E),
;;;  because getting childs on every iteration isn't O(1).
;;;  This is due to simple (and suboptimal) graph representation.
;;;
;;;  Accepted input is a list of edges: ((1 2) (1 3) (2 5) ...)
;;;
(defun topo-sort (adj-list-graph)
  (macrolet ((parent (edge) `(first ,edge))
             (child  (edge) `(second ,edge))
             (childs (vertex)
               `(loop
                   :for edge :in adj-list-graph
                   :when (eql ,vertex (parent edge))
                   :collect (child edge))))
    (let ((visited  (make-hash-table))
          (traverse (list))
          (vertices (remove-duplicates
                     (loop
                        :for edge :in adj-list-graph
                        :collect (parent edge)
                        :collect (child  edge)))))
      (labels ((visit (vertex)
                 (loop
                    :for n :in (childs vertex)
                    :do (unless (gethash n visited)
                          (visit n)))
                 (push vertex traverse)
                 (setf (gethash vertex visited) 'visited)))
        (loop
           :for v :in vertices
           :do (unless (gethash v visited)
                 (visit v))))
      (loop
         :for v :in (reverse traverse)
         :collect v))))
