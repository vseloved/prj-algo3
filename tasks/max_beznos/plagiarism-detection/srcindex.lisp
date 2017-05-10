(defpackage #:srcindex
  (:use #:cl)
  (:export :word-list-from-file
           :word-list-from-text
           :reset-source-index
           :build-source-index
           :find-sources))

(in-package #:srcindex)

(defparameter *min-word-length*          8)
(defparameter *mismatch-threshold*       0.05)
(defparameter *mismatch-count-credit*   -5)
(defparameter *srcindex-hash-table-size* 4000000)

(defvar *source-index* nil)

(defun reset-source-index ()
  (setf *source-index* nil)
  (sb-ext:gc :full t)
  (setf *source-index* (make-hash-table :test 'equalp :size *srcindex-hash-table-size*)))

(deftype index-type ()
  `(integer 0 ,(expt 2 24)))

(defstruct (source-text (:conc-name st-))
  (path      ""  :type pathname)
  (position  0   :type index-type)
  (back-list nil :type list)
  (next-list nil :type list))

(defun word-list (istream &optional (split-offset nil))
  (let ((w (make-array 256 :element-type 'character :fill-pointer 0))
        (ws-next (list))
        (ws-back (list))
        (offset  0)
        (trim-chars '(#\! #\? #\( #\) #\" #\, #\; #\: #\. #\Newline)))
    (labels ((push-unless-empty (w pos)
               (let ((trimmed (string-trim trim-chars w)))
                 (unless (string= trimmed "")
                   (if (and split-offset
                            (< offset split-offset))
                       (push (cons (copy-seq trimmed) (the index-type (- pos (length w)))) ws-back)
                       (push (cons (copy-seq trimmed) (the index-type (- pos (length w)))) ws-next))
                   (incf offset)))))
      (loop
         :with i = 0
         :for  c = (read-char istream nil '*eof*)
         :for  p = 0 :then (1+ p)
         :until (eql c '*eof*)
         :do (cond ((and (or (char= c #\Space)
                             (char= c #\Newline)))
                    (when (push-unless-empty w p)
                      (setf (fill-pointer w) 0
                            i 0)))
                   (t
                    (incf (fill-pointer w))
                    (setf (aref w i) c)
                    (incf i)))
         :finally (push-unless-empty w p)))
    (values (nreverse ws-next)
            ws-back)))

(defun word-list-from-file (filename)
  (with-open-file (is filename)
    (word-list is)))

(defun word-list-from-text (text)
  (with-input-from-string (is text)
    (word-list is)))

(defun build-source-index (filename)
  (loop
     :with st-total-inserts = 0
     :with back-list = (list)
     :for next-list  = (word-list-from-file filename) :then (setf next-list (cdr next-list))
     :for wp = (car next-list) :then (car next-list)
     :while next-list
     :do (let* ((w (car wp))
                (p (cdr wp))
                (source (sb-ext:with-locked-hash-table (*source-index*)
                          (gethash w *source-index* (list)))))
           (when (>= (length w) *min-word-length*)
             (push (make-source-text :path filename
                                     :position p
                                     :back-list back-list
                                     :next-list (cdr next-list))
                   source)
             (sb-ext:with-locked-hash-table (*source-index*)
               (setf (gethash w *source-index*) source))
             (incf st-total-inserts))
           (push wp back-list))
     :finally (return st-total-inserts)))

(defun source-match (source-ws text-ws)
  (declare (optimize (speed 3) (safety 0)))
  (loop
     :with end-wp
     :with mismatch-count = (the fixnum *mismatch-count-credit*)
     :for current-length  = (the fixnum 0) :then (the fixnum (1+ current-length))
     :for twp :in text-ws
     :for swp :in source-ws
     :while (or (= 0 current-length)
                (< (/ mismatch-count current-length) (the single-float *mismatch-threshold*)))
     :do (progn
           (setf end-wp
                 (list (car swp) (cdr swp) (cdr twp)))
           (unless (string= (the (vector character) (car twp))
                            (the (vector character) (car swp)))
             (incf (the fixnum mismatch-count))))
     :finally (return (cons (list end-wp) current-length))))

(defun find-sources (word-list &key (min-words-match 2))
  (loop
     :with sources    = (list)
     :with back-list  = (list)
     :with skip-count = 0
     :for next-list   = word-list :then (setf next-list (cdr next-list))
     :for wp = (car next-list) :then (car next-list)
     :while next-list
     :do (let ((w (car wp))
               (p (cdr wp)))
           (when (> skip-count 0)
             (decf skip-count))
           (multiple-value-bind (source found)
               (gethash w *source-index* (list))
             (when (and found (= skip-count 0))
               (loop
                  :with last-added-range = nil
                  :for s :in source
                  :when (or (not last-added-range)
                            (not (and (>= p (car last-added-range))
                                      (<= p (cdr last-added-range)))))
                  :do (let* ((match-tail (source-match (st-next-list s) (cdr next-list)))
                             (match-head (source-match (st-back-list s) back-list))
                             (match-len  (+ 1 (cdr match-head) (cdr match-tail)))
                             (match-full
                              (append (car match-head)
                                      (list (list w (st-position s) p))
                                      (reverse (car match-tail)))))
                        (cond ((>= match-len min-words-match)
                               (push (list (st-path s) match-len match-full w (st-position s))
                                     sources)
                               (setf skip-count (+ (cdr match-tail) (abs *mismatch-count-credit*)))
                               (setf last-added-range (cons (third (first match-full))
                                                            (third (car (last match-full))))))
                              (t
                               (setf skip-count (1- (* 2 (abs *mismatch-count-credit*))))))))))
           (push wp back-list))
     :finally (return (sort sources #'> :key #'second))))
