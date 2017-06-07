(load (merge-pathnames "queue.lisp" (or *load-truename* *compile-file-truename*)))

(defun bfs-on-mat (g source sink &optional paths)
  (let* ((size (sqrt (array-total-size g)))
	 (paths (or paths (make-array size :initial-element nil)))
	 (visited (make-array size :initial-element nil))
	 (q (make-queue)))
    
    (queue-push source q)
    (setf (aref visited source) t)

    (loop :for u = (queue-pop q) :while u :do
       (loop :for v :from 0 :below size :do
	  (when (and (null (aref visited v)) (> (aref g u v) 0))
	    (setf (aref visited v) t
		  (aref paths v) u)
	    (queue-push v q))))
 
    (aref visited sink)))
