(load "union-find.lisp")

(defparameter *graph*
  '((0 1 4) (0 7 8)
    (1 2 8) (2 3 7)
    (3 4 9) (3 5 14)
    (5 4 9) (6 5 2)
    (7 6 1) (7 8 7)
    (8 2 2) (2 5 4)
    (8 6 6) (1 7 11)))

(defun count-nodes (edges)
  (let ((ht (make-hash-table)))
    (loop :for edge :in edges :do
       (setf (gethash (car edge) ht) t
	     (gethash (cddr edge) ht) t))
    (hash-table-size ht)))

(defun kruskal (edges)
  (let* ((n (count-nodes edges))
	 (uf (make-union-find n))
	 (mst (list)))
    (loop
       :for edge :in (sort (copy-seq edges) #'< :key #'caddr)
       :while (< (length mst) (1- n))
       :do (when (not (connected? uf (car edge) (cadr edge)))
	     (push edge mst)
	     (union-sets uf (car edge) (cadr edge))))
    mst))

(defun test ()
  (let ((answer '((3 4 9) (0 7 8)
		  (2 3 7) (2 5 4)
		  (0 1 4) (8 2 2)
		  (6 5 2) (7 6 1))))
    (loop :for edge :in (kruskal *graph*) :do
       (assert (find edge answer :test #'equal)))))
