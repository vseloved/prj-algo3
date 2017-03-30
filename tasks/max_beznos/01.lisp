;;; text segmentation problem

;;; load-dictionary expect that dictionary file has format:
;;;
;;;  ngram TAB year TAB match_count TAB volume_count NEWLINE
;;;
(defun load-dictionary (filename &key (head nil))
  (labels ((parse-dictionary-record (record)
             (multiple-value-bind (ngram rest-line-index)
                 (let ((first-tab (or (position #\Tab record) 0)))
                   (values (subseq record 0 first-tab)
                           first-tab))
               (let ((counts (loop
                                :with start = rest-line-index
                                :while (< start (length record))
                                :collect (multiple-value-bind (x next-start)
                                             (read-from-string record nil 'eof :start start)
                                           (setf start next-start)
                                           x))))
                 (append (list (remove #\Space ngram)) counts)))))
    (let ((dict-list
           (with-open-file (is filename :direction :input)
             (remove-if (lambda (s) (= 0 (length s)))
                        (loop
                           :for count  = 0 :then (1+ count)
                           :for record = (read-line is nil '*eof*) :then (read-line is nil '*eof*)
                           :until (or (eql record '*eof*)
                                      (and head (= count head)))
                           :collect (parse-dictionary-record record))))))
      (make-array (list (length dict-list) 4)
                  :initial-contents dict-list))))

(defun collect-hint-list (dictionary-array-with-rates)
  (let ((last-index (1- (first (array-dimensions dictionary-array-with-rates)))))
    (loop
       with range-first-char = nil
       with range-begin      = 0
       with result           = (list)
       for index from 0 to last-index
       do (let ((entry (aref dictionary-array-with-rates index 0)))
            (cond
              ((= 0 (length entry)))
              ((not range-first-char) (setf range-first-char (char entry 0)))
              ((or (not (char= range-first-char (char entry 0)))
                   (= index last-index))
               (push (list range-first-char range-begin index)
                     result)
               (setf range-begin index)
               (setf range-first-char (char entry 0)))))
       finally (return (reverse result)))))

(defun match-entry (dictionary-array-with-rates text &key start end (max-rate nil))
  (when (and start end)
    (if (not max-rate)
        (loop
           :for index :from start :below end
           :thereis (let ((entry (aref dictionary-array-with-rates index 0)))
                      (cond ((and (<= (length entry) (length text))
                                  (string= entry (subseq text 0 (length entry))))
                             index))))
        (loop
           :with max-match-count-index = nil
           :for index :from start :below end
           :do (let ((entry (aref dictionary-array-with-rates index 0)))
                 (when (and (<= (length entry) (length text))
                            (string= entry (subseq text 0 (length entry)))
                            (or (not max-match-count-index)
                                (> (aref dictionary-array-with-rates index 2)
                                   (aref dictionary-array-with-rates max-match-count-index 2))))
                   (setf max-match-count-index index)))
           :finally (return max-match-count-index)))))

(defun segment (text &key dictionary-file (limit nil) (max-rate nil))
  (let* ((dictionary-array (load-dictionary dictionary-file))
         (hint-list        (collect-hint-list dictionary-array))
         (segmentations    (list)))
    (tagbody
       (labels
           ((collect-segmentations (text &key limit (segments (list)) (start nil))
              (cond ((and limit
                          (= limit (length segmentations)))
                     (go :stop))
                    (t
                     (if (string= text "")
                         (push segments segmentations)
                         (let* ((hint (find-if (lambda (h) (char= (first h) (char text 0)))
                                               hint-list))
                                (entry-index (match-entry dictionary-array text
                                                          :start    (or start (second hint))
                                                          :end      (third hint)
                                                          :max-rate max-rate)))
                           (cond ((and start (not entry-index)))
                                 (entry-index
                                  (unless max-rate
                                    (collect-segmentations text
                                                           :limit limit
                                                           :segments (copy-list segments)
                                                           :start (1+ entry-index)))
                                  (let ((entry (aref dictionary-array entry-index 0)))
                                    (collect-segmentations (subseq text (length entry))
                                                           :limit limit
                                                           :segments (append segments
                                                                             (list entry))))))))))))
         (collect-segmentations text :limit limit))
     :stop)
    segmentations))
