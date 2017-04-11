(in-package :break-text)

(defvar *words-dict* (make-hash-table :test #'equal))

(defun add-word (str)
  (setf (gethash str *words-dict*) t))

(defun get-word (str)
  (gethash (string-downcase str) *words-dict*))

(defun load-words (filename)
  (with-open-file (in filename)
    (loop
       for line = (read-line in nil)
       while line
       do (add-word (string-right-trim '(#\Return) line)))))
  
(defun reset-words ()
  (setf *words-dict* (make-hash-table :test #'equal)))

(defvar *bi-gram-dict* (make-hash-table :test #'equal))

(defun add-bi-gram (line)
  (destructuring-bind (bi-gram count) (cl-ppcre::split "\\t" line)
    (setf (gethash bi-gram *bi-gram-dict*)
	  (parse-integer count))))

(defun get-bi-gram (bi-gram)
  (gethash bi-gram *bi-gram-dict* 0))

(defun load-bi-grams (filename)
  (with-open-file (in filename)
    (loop
       for line = (read-line in nil)
       while line
       do (add-bi-gram line))))

(defun reset-bi-grams ()
  (setf *bi-gram-dict* (make-hash-table :test #'equal)))

(defun max-value-index (array)
  "Find index of the last maximum value."
  (loop
     with max-index = 0
     with max-value = (aref array 0)
     for x :across array
     for i :from 0
     when (>= x max-value)
     do
       (setf max-index i)
       (setf max-value x)
     finally (return max-index)))

(defun break-text (str)
  (let* ((graph (text->graph str))
	 (sorted (reverse (tsort graph)))
	 (costs (make-array (nodes-count graph)))
	 (paths (make-array (nodes-count graph))))

    ;;; Prepare nodes for traversing.
    (loop :for v :across sorted do
	 (setf (aref costs v) -1))

    ;;; Set start node.
    (setf (aref costs 0) 0)
    (setf (aref paths 0) 0)

    ;;; Travers graph.
    (loop :for v :across sorted do
       (loop
	  for edge in (node-edges (get-node v graph))
	  for u = (edge-dst edge) :do
	    (let ((new-distance (+ (aref costs v)
				   (edge-weight edge))))
	      (when (> new-distance (aref costs u))
		(setf (aref costs u) new-distance)
		(setf (aref paths u) v)))))
    
    ;;; Reconstruct text.
    (loop for i = (max-value-index costs) then (aref paths i)
       while (> i 0)
       collect (node-label (get-node i graph)) into result
       finally (return (format nil "~{~a~^ ~}" (nreverse result))))))

(defun text->graph (str)
  (let* ((size (length str))
	 (g (make-graph))
	 (backtrack (make-array (1+ size) :initial-element nil)))
    
    (add-node nil g)
    (push 0 (aref backtrack 0))

    (loop for i from 0 to size do
	 (loop
	    for j from (1+ i) to size
	    for chunk = (subseq str i j)
	    when (get-word chunk) do
	      (let ((dst-node-id (node-id (add-node chunk g)))
		    (src-nodes (aref backtrack (- j (length chunk)))))
		(push dst-node-id (aref backtrack (+ i (length chunk))))
		(loop
		   for src-node-id in src-nodes
		   for prefix = (node-label (get-node src-node-id g))
		   for bi-gram = (concatenate 'string prefix " " chunk)
		   for cost = (get-bi-gram bi-gram)
		   do
		     (add-edge src-node-id dst-node-id cost g)))))
    g))
