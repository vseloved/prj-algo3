(in-package :cl-user)

(defpackage :split-text
  (:use :common-lisp))

(in-package :split-text)

(defvar *dict* nil)

(defun reset-dict ()
  (setf *dict* nil))

(defun load-dict (filename)
  (with-open-file (in filename)
    (flet ((add-to-dict (word)
	     (pushnew (string-right-trim '(#\Return) word) *dict*)))
      (loop
	 for word = (read-line in nil)
	 while word
	 do (add-to-dict word)))))

(defun SPLIT-TEXT (string &optional (memo (make-hash-table :test #'equal)))
  (or (gethash string memo)
      (loop
	 for i from 1 below (length string)
	 for word = (subseq string 0 i)
	 when (find word *dict* :test #'equal)
	 append
	   (loop for chunk in (SPLIT-TEXT (subseq string i) memo)
	      collect (concatenate 'string word " " chunk))
	 into results
	 finally
	   (if (find string *dict* :test #'equal)
	       (push string results))
	   (setf (gethash string memo) results)
	   (return results))))
