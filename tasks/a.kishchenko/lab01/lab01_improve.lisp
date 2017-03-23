;;;;
;;;; lab 01 for Advanced Algo course
;;;; second iteration -- more efficient algo
;;;; made by Andrew Kishchenko
;;;;

(defconstant +dfile+ "lab01_dict")
(defconstant +dngram+ "count_1w.txt")

(defconstant +maxwordlen+ 24)

;;; key -> word, value -> freq
(defparameter *ngram* (make-hash-table :size 1000000 :test 'equal))
;;; our broken text without whitespaces (getting from STDIN)
(defparameter *text* nil)
;;; tree path
(defparameter *tree* (make-hash-table))

;;; Debug snippets
(defun pr (a) (format t "~a~%" a))
(defun pr2 (a b) (format t "~a~a~%" a b))
(defun prt (a) (format t "type is ~a, value is ~a~%" (type-of a) a))

;;; add parsed ngram to hashtable
(defun add-ngram (word freq)
  (setf (gethash word *ngram*) freq))

;;; parse ngram, format: word[tab]frequency
(defun parse-ngram (line)
  (dotimes (i (length line))
    (when (char-equal (char line i) #\Tab)
      (add-ngram (subseq line 0 (- i 1))(parse-integer (subseq line i (length line)))))))

;;; read file with ngrams
(defun fill-ngram()
  (let ((in (open +dngram+ :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
            while line do (parse-ngram line)))
    (close in))
  (pr "ngram read/parse finish"))

;;;
;;; Hashtable helpers
;;;
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
  (setf *text* (read)))

(defun parse-text()
  (let ((tree ()))
    (get-words *tree* *text* 0)
    (get-all-path *tree* 0 0 (make-array 32767))))

(defun freqp (seq)
  (let ((freq (gethash seq *ngram*)))
    (if (not freq) 0 freq)))

(defun wordp (seq)
  (if (> (freqp seq) 0) t nil))

(defun valid-len (txt i)
  (if (< i (length txt)) t nil))

(defun max-bounds (st txt)
  (min (length txt) (+ st +maxwordlen+)))

(defun get-words (ht txt start)
  (let ((st (+ start 1)))
    (when (valid-len txt start)
      (let ((end (max-bounds st txt)))
        (loop for i from st to end
              do (collect-word ht (subseq txt start i) i txt))))))

(defun collect-word (ht seq index txt)
  (when (wordp seq)
    ;; (format t "seq: ~a index: ~a~%" seq index)
    (setf (gethash index ht) (make-hash-table))
    (get-words (gethash index ht) txt index)))

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

;;;
;;; main
;;;
(defun my-main()
  (fill-ngram)
  (read-text)
  (parse-text))

(my-main)
