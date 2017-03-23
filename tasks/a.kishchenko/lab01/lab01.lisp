;;;;
;;;; lab 01 for Advanced Algo course 
;;;; made by Andrew Kishchenko
;;;;

(defconstant +dfile+ "lab01_dict")
(defconstant +dfile_huge+ "../../dict_en.txt")

;;; dictionary hashtable
;;; key -> start letter
;;; value -> list of all words that begin from this letter
;;; a -> (a, aa, aaa, aaa, abc, abb)
;;; b -> (b, bbb, bbbccc, bbbc)
(defparameter *dict* (make-hash-table :size 26))

;;; our broken text without whitespaces (getting from STDIN)
(defparameter *text* nil)
;;; tree path
(defparameter *tree* (make-hash-table))

;;; Debug snippets
(defun pr (a) (format t "~a~%" a))
(defun pr2 (a b) (format t "~a~a~%" a b))
(defun prt (a) (format t "type is ~a, value is ~a~%" (type-of a) a))

;;; read file with dictionary
;;; and fill hashtable
(defun fill-dict()
  (let ((in (open +dfile_huge+ :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
            while line do (when (or (> (length line) 1) (and (= (length line) 1) (char-equal #\a (char line 0)))) (if (gethash (char line 0) *dict*)
                              (setf (gethash (char line 0) *dict*) (push line (gethash (char line 0) *dict*)))
                              (setf (gethash (char line 0) *dict*) (list line)))))
      (close in)))
  (pr "read finish"))

;;;
;;; Hashtable helpers
;;;
(defun get-list (i)
  (gethash (char *text* i) *dict*))

(defun print-dict-kvp (key value)
  (format t "~a -> ~a~%" key value))

(defun print-ht-debug(ht)
  (with-hash-table-iterator (i ht)
    (loop (multiple-value-bind (entry-p key value) (i)
        (if entry-p
            (print-dict-kvp key value)
          (return))))))

;;; read text from stdin
;;; assume than in input "ourtextwithoutspaces" and double quotes surrounding
(defun read-text ()
  (setf *text* (read))
  (setf *arr* (make-array (length *text*))))

(defun parse-text()
  (let ((tree ()) (errorIndex -1))
    (setf errorIndex (check-letter 0 *tree*))
    (get-all-path *tree* 0 0 (make-array 32767))))

(defun wordp (word index)
  (search word *text* :start2 index :end2 (+ index (length word))))

(defun valid-len (i)
  (if (< i (length *text*)) t nil))

;;; add value to hashtable
(defun add-to-ht (ht k v)
  (setf (gethash k ht) v))

;;; debug method, print path
(defun get-prev-depth (arr n)
  (dotimes (i n)
    (format t "~a " (aref arr i)))
  (format t "~%"))

;;; print fixed text
(defun print-fixed-text (pos spacepos text arr n)
  (when (< pos (length text))
    (when (eql pos (aref arr spacepos))
      (format t " ")
      (when (< spacepos n)
        (setf spacepos (+ 1 spacepos))))
    (format t "~a" (char text pos))
    (print-fixed-text (+ pos 1) spacepos text arr n)))

(defun get-all-path(ht key d arr)
  (if (> (hash-table-count ht) 0)
    (with-hash-table-iterator (iter ht)
      (loop (multiple-value-bind (entry-p key value) (iter)
        (if entry-p
          (progn (setf (aref arr d) key) (get-all-path (gethash key ht) key (+ d 1) arr))
          (return)))))
    (progn (print-fixed-text 0 0 *text* arr d) (format t "~%"))))

(defun check-letter (i ht)
  (when (valid-len i)
    (let ((newht (make-hash-table))(l (get-list i))(key 0))
      (if l
        (dolist (x l)
          (when (wordp x i)
            (setf newht (make-hash-table))
            (setf key (+ i (length x)))
            (add-to-ht ht key newht)
            ;; (format t "[find] word = ~a at index ~a~%" x key)
            (check-letter key newht)))
        (check-letter (+ i 1) ht)))))

;;;
;;; main
;;;
(defun my-main()
  (fill-dict)
  ;;(print-ht-debug *dict*)
  (read-text)
  (parse-text))

(my-main)
