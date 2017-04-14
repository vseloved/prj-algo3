;;; text segmentation problem

;;; load-dictionary expect that dictionary file has format:
;;;
;;;  ngram TAB year TAB match_count TAB volume_count NEWLINE
;;;
(defun load-dictionary (filename &key (head nil))
  (labels ((parse-dictionary-record (record)
             (multiple-value-bind (ngram rest-line-index)
                 (let ((first-tab (or (position #\Tab record)
                                      (length record))))
                   (values (string-trim '(#\Return) (subseq record 0 first-tab))
                           first-tab))
               (let ((counts (loop
                                :with start = rest-line-index
                                :while (< start (length record))
                                :collect (multiple-value-bind (x next-start)
                                             (read-from-string record nil 'eof :start start)
                                           (setf start next-start)
                                           x))))
                 (append (list (remove #\Space ngram)) counts)))))
    (let ((dictionary-ht (make-hash-table :test 'equalp)))
      (with-open-file (is filename :direction :input)
        (loop
           :for count  = 0 :then (1+ count)
           :for record = (read-line is nil '*eof*) :then (read-line is nil '*eof*)
           :until (or (eql record '*eof*)
                      (and head (= count head)))
           :do (let* ((parsed-record (parse-dictionary-record record))
                      (ngram         (first parsed-record)))
                 (unless (string= "" ngram)
                   (setf (gethash ngram dictionary-ht) (butlast parsed-record))))))
      dictionary-ht)))

(defun binary-search (start end predicate)
  (let ((next-start (+ start (ceiling (- end start) 2)))
        (next-end   (- end (floor (- end start) 2))))
    (cond ((= start (1- end))
           (values start end))
          ((funcall predicate start next-end)
           (binary-search start next-end predicate))
          (t
           (binary-search next-start end predicate)))))

(defun graph-edge-index (v-index-from v-index-to vertices-count)
  (+ (- v-index-to v-index-from)
     (* v-index-from vertices-count)
     (/ (* -1 v-index-from (1- v-index-from))
        2)))

(defun graph-vertex-index (edge-index vertices-count)
  (flet ((hint (i)
           (+ (* i vertices-count)
              (/ (* -1 i (1- i))
                 2)))
         (binary-search (start end predicate)
           (let ((next-start (+ start (ceiling (- end start) 2)))
                 (next-end   (- end (floor (- end start) 2))))
             (cond ((= start (1- end))
                    (values start end))
                   ((funcall predicate start next-end)
                    (binary-search start next-end predicate))
                   (t
                    (binary-search next-start end predicate))))))
    (let ((from (binary-search 0 vertices-count
                               (lambda (start end)
                                 (and (<= (hint start) edge-index)
                                      (<= edge-index (hint end)))))))
      (values from (+ from (- edge-index (hint from)))))))


(defun segment (text &key dictionary-file)
  (let* ((dictionary-ht    (load-dictionary dictionary-file))
         (text-len         (length text))
         (max-edge-index   (graph-edge-index (1- text-len) text-len text-len))
         (graph            (make-array (1+ max-edge-index) :initial-element nil))
         (precedence-ht    (make-hash-table)))
    (loop
       :for edge-index :from 1 :to max-edge-index
       :do (multiple-value-bind (start end)
               (graph-vertex-index edge-index text-len)
             (multiple-value-bind (w found)
                 (gethash (subseq text start end) dictionary-ht)
               (when found
                 (setf (aref graph edge-index)
                       (or w 1))
                 (pushnew start (gethash end precedence-ht))))))
    (let ((relax-ht      (make-hash-table))
          (segmentations (list)))
      (macrolet ((make-relax-record (d p)
                   `(make-array 2 :initial-contents (list ,d ,p))))
        (setf (gethash 0 relax-ht) (make-relax-record 0 nil))
        (loop
           :repeat (1- text-len)
           :do (loop
                  :for edge-index :from 1 :to max-edge-index
                  :do (multiple-value-bind (u v)
                          (graph-vertex-index edge-index text-len)
                        (let* ((u-relax (gethash u relax-ht
                                                 (make-relax-record most-positive-fixnum nil)))
                               (v-relax (gethash v relax-ht
                                                 (make-relax-record most-positive-fixnum nil)))
                               (u-d     (aref u-relax 0))
                               (v-d     (aref v-relax 0))
                               (w       (aref graph edge-index)))
                          (when w
                            (cond ((> v-d (+ u-d w))
                                   (setf (gethash v relax-ht)
                                         (make-relax-record (+ u-d w) (list u)))))))))))
      (labels
          ((collect-segmentations (&optional (v text-len) (segments (list)))
             (let ((shortest-path-len (aref (gethash text-len relax-ht) 0)))
               (cond ((> (length segments) shortest-path-len))
                     ((and (= v 0)
                           (= text-len (cadar (last segments)))
                           (= (length segments) shortest-path-len))
                      (push segments segmentations))
                     ((> v 0)
                      (loop
                         :for u :in (gethash v precedence-ht)
                         :do (let ((segments-copy (copy-list segments)))
                               (push (list u v) segments-copy)
                               (collect-segmentations u segments-copy))))))))
        (collect-segmentations))
      (loop
         :for s :in segmentations
         :do (format t "~{ ~a~}~%"
                     (mapcar (lambda (u-v) (subseq text (first u-v) (second u-v))) s))))))
