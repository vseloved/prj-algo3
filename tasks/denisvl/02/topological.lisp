(defun dfs(v adj visited)
  (setf (aref visited v) 1)
  (loop
    for i from 0 to (- (array-dimension adj 1) 1)
    when (and (= 1 (aref adj v i)) (/= (aref visited i) 2))
    do (if (= 1 (aref visited i))
         (error "cycle")
         (dfs i adj visited)))
  (setf (aref visited v) 2)
  (print v)
)

(defun topological()
(let* ((adj (make-array '(7 7) :initial-contents
             '((0 1 1 0 0 0 0)
               (0 0 0 0 1 0 0)
               (0 0 0 0 0 1 0)
               (0 0 0 0 0 0 0)
               (0 0 0 1 0 1 0)
               (0 0 0 1 0 0 0)
               (0 0 0 0 0 0 0))))
      (visited (make-array (array-dimension adj 0) :initial-element 0)))
  (loop
    for i from 0 to (- (array-dimension adj 1) 1)
    when (= 0 (aref visited i))
    do (dfs i adj visited)))
)

(topological)
