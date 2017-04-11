(in-package :break-text)

(defun tsort (graph &optional
		      (node (take (nodes graph)))
		      (unvisited (copy-keys (nodes graph)))
		      (sorted (make-array 0 :adjustable t :fill-pointer t)))
  (with-slots (id edges) node
    (remhash id unvisited)
    (dolist (edge edges)
      (let ((child (edge-dst edge)))
	(when (gethash child unvisited)
	  (tsort graph (get-node child graph) unvisited sorted))))
    (vector-push-extend id sorted))
  sorted)

(defun take (h)
  "Take a first value from a given hash table."
  (loop for value being the hash-values of h do (return-from take value)))

(defun copy-keys (h)
  "Copy keys from an old hash table to a new hash table."
  (let ((new-h (make-hash-table :test #'equal)))
    (loop for key being the hash-keys of h do
	 (setf (gethash key new-h) t))
    new-h))
