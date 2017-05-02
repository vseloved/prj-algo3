(defpackage #:srcindex
  (:use #:cl)
  (:export :word-list-from-file
           :word-list-from-text
           :reset-source-index
           :build-source-index
           :find-sources))

(in-package #:srcindex)

(defparameter *min-word-length*          7)
(defparameter *mismatch-threshold*       0.05)
(defparameter *mismatch-count-credit*   -5)
(defparameter *srcindex-hash-table-size* 2000000)

(defvar *source-index* (make-hash-table :test 'equalp :size *srcindex-hash-table-size*))

(defstruct (source-text (:conc-name st-))
  path
  (position 0 :type fixnum)
  back-list
  next-list)

(defun word-list (istream)
  (let ((ws (list))
        (trim-chars '(#\" #\, #\; #\: #\. #\Newline)))
    (labels ((push-unless-empty (w pos)
               (let ((trimmed (string-trim trim-chars w)))
                 (unless (string= trimmed "")
                   (push (list trimmed (- pos (length w))) ws)))))
      (loop
         :with w = ""
         :for  c = (read-char istream nil '*eof*)
         :for  p = 0 :then (incf p)
         :until (eql c '*eof*)
         :do (cond ((and (or (char= c #\Space)
                             (char= c #\Newline)))
                    (when (push-unless-empty w p)
                      (setf w "")))
                   (t
                    (setf w (concatenate 'string w (string c)))))
         :finally (push-unless-empty w p)))
    (reverse ws)))

(defun word-list-from-file (filename)
  (with-open-file (is filename)
    (word-list is)))

(defun word-list-from-text (text)
  (with-input-from-string (is text)
    (word-list is)))

(defun reset-source-index ()
  (setf *source-index* (make-hash-table :test 'equalp :size *srcindex-hash-table-size*)))

(defun build-source-index (filename)
  (loop
     :with back-list = (list)
     :for next-list  = (word-list-from-file filename) :then (setf next-list (cdr next-list))
     :for wp = (car next-list) :then (car next-list)
     :while next-list
     :do (let* ((w (first wp))
                (p (second wp))
                (source (gethash w *source-index* (list))))
           (when (>= (length w) *min-word-length*)
             (push (make-source-text :path filename
                                     :position p
                                     :back-list back-list
                                     :next-list (cdr next-list))
                   source)
             (setf (gethash w *source-index*) source))
           (push wp back-list))))

(defun source-match (source-ws text-ws)
  (map 'list #'car
       (loop
          :with match = (list)
          :with mismatch-count = *mismatch-count-credit*
          :for twp :in text-ws
          :for swp :in source-ws
          :while (or (= 0 (length match))
                     (< (/ mismatch-count (length match)) *mismatch-threshold*))
          :do (let ((match-to-source-word (string= (first twp) (first swp))))
                (push (list (list (first swp) (second swp) (second twp)) match-to-source-word)
                      match)
                (when (not match-to-source-word)
                  (incf mismatch-count)))
          :finally (return (loop
                              :for w = (car match) :then (car match)
                              :while (and w (not (cadr w)))
                              :do (pop match)
                              :finally (return match))))))

(defun find-sources (word-list &key (min-words-match 2))
  (loop
     :with sources    = (list)
     :with back-list  = (list)
     :with skip-count = 0
     :for next-list   = word-list :then (setf next-list (cdr next-list))
     :for wp = (car next-list) :then (car next-list)
     :while next-list
     :do (let ((w (first wp))
               (p (second wp)))
           (when (> skip-count 0)
             (decf skip-count))
           (multiple-value-bind (source found)
               (gethash w *source-index* (list))
             (when (and found (= skip-count 0))
               (loop
                  :for s :in source
                  :do (let* ((match-tail (reverse (source-match (st-next-list s) (cdr next-list))))
                             (source-matched-words
                              (append (source-match (st-back-list s) back-list)
                                      (list (list w (st-position s) p))
                                      match-tail)))
                        (cond ((>= (length source-matched-words) min-words-match)
                               (push (list (st-path s) source-matched-words
                                           w (st-position s))
                                     sources)
                               (setf skip-count (length match-tail)))
                              (t
                               (setf skip-count (1- (* 2 (abs *mismatch-count-credit*))))))))))
           (push wp back-list))
     :finally (return (sort sources #'> :key (lambda (s) (length (cadr s)))))))
