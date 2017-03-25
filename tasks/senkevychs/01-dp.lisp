(defparameter *possible-partitions* nil)

;;; returns a list of all possible partitions of an input text
(defun text-partition (text dictionary-path)
  (setf *possible-partitions* nil)
  (move-forward text dictionary-path))

;;; runs through the text and for each symbol saves the list of indices
;;; of all symbols from which the current one could be accessed
(defun move-forward (text dictionary-path)
  (let* ((dictionary (read-file dictionary-path)) 
	 (n (length text))
	 (max (max-length dictionary))
	 (lookup-table (make-array n :initial-element nil)))
    (dotimes (i n)
      (dotimes (j max)
	(when (and (< (+ i j) n) (search-word (subseq text i (+ i j 1)) dictionary))
	  (push i (aref lookup-table (+ i j))))))
    (move-back text lookup-table "")
    *possible-partitions*))

(defun read-file (path)
  (let ((dictionary nil) (in (open path :if-does-not-exist nil)))
  (when in
    (loop for line = (read-line in nil)
       while line do (push line  dictionary))
    (close in))
  (coerce (reverse dictionary) 'vector)))

(defun max-length (dictionary)
  (let ((max 0) (word nil))
    (dotimes (i (array-total-size dictionary))
      (setq word (aref dictionary i))
      (when (> (length word) max)
	 (setq max (length word))))
    max))

;;; searches for the word in the dictionary (binary search)
;;; it is assumed that the dictionary has been initially sorted
(defun search-word (word dictionary)
  (let ((n (length dictionary)))
    (when (or (= n 0) (and (= n 1) (not (string-equal word (aref dictionary 0)))))
      (return-from search-word nil))
    (cond
      ((string-lessp word (aref dictionary (floor n 2)))
       (search-word word (subseq dictionary 0 (floor n 2))))
      ((string-greaterp word (aref dictionary (floor n 2)))
       (search-word word (subseq dictionary (+ 1 (floor n 2)) n)))
      (t t))))

;;; goes through the table recursively and builds all possible partition
(defun move-back (text lookup current)
  (if (equalp lookup #())
      (push (string-trim " " current) *possible-partitions*)
      (unless (equal (car (aref lookup 0)) nil)
	(dolist (i (aref lookup (- (length lookup) 1)))
	  (move-back
	   text
	   (subseq lookup 0 i)
	   (concatenate 'string (subseq text i (length lookup)) " " current))))))
