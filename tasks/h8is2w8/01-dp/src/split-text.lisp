(in-package :cl-user)

(defpackage :split-text
  (:use :common-lisp))

(in-package :split-text)

(defvar *dict* (make-hash-table :test #'equal))

(defun add-to-dict (word)
  (let ((cleaned-word (string-right-trim '(#\Return) word)))
    (setf (gethash cleaned-word *dict*) t)))

(defun find-in-dict (word)
  (gethash word *dict*))

(defun reset-dict ()
  (setf *dict* (make-hash-table :test #'equal)))

(defun load-dict (filename)
  (with-open-file (in filename)
    (loop
       for word = (read-line in nil)
       while word
       do (add-to-dict word))))

(defun SPLIT-TEXT (str &optional (memo (make-hash-table :test #'equal)))
  (or (gethash str memo)
      (loop
	 for i from 1 below (length str)
	 for word = (subseq str 0 i)
	 when (find-in-dict word)
	 append
	   (loop for chunk in (SPLIT-TEXT (subseq str i) memo)
	      collect (concatenate 'string word " " chunk))
	 into results
	 finally
	   (if (find-in-dict str)
	       (push str results))
	   (setf (gethash str memo) results)
	   (return results))))
