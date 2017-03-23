;;; text segmentation problem

(defun load-dictionary (filename &key (head nil))
  (let ((trim-chars (list #\Return)))
    (with-open-file (is filename :direction :input)
      (remove-if (lambda (s) (= 0 (length s)))
       (loop
          for count = 0 then (1+ count)
          for word = (read-line is nil '*eof*) then (read-line is nil '*eof*)
          until (or (eql word '*eof*)
                    (and head (= count head)))
          collect (string-trim trim-chars word))))))

(defun collect-hint-list (dictionary-array)
  (let ((last-index (1- (first (array-dimensions dictionary-array)))))
    (loop
       with range-first-char = nil
       with range-begin      = 0
       with result           = (list)
       for index from 0 to last-index
       do (let ((entry (aref dictionary-array index)))
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

(defun match-entry (dictionary-array text &key start end)
  (loop
     for index from start below end
     thereis (let ((entry (aref dictionary-array index)))
               (cond ((and (<= (length entry) (length text))
                           (string= entry (subseq text 0 (length entry))))
                      index)))))

(let* ((dictionary       (load-dictionary "../dict_en.txt"))
       (dictionary-array (make-array (length dictionary) :initial-contents dictionary))
       (hint-list        (collect-hint-list dictionary-array)))
  (defun segment (text &key (limit nil))
    (let ((result-segments (list)))
      (tagbody
         (labels
             ((collect-segmentations (text &key limit (segments (list)) (start nil))
                (cond ((and limit
                            (= limit (length result-segments)))
                       (go :stop))
                      (t
                       (if (string= text "")
                           (push segments result-segments)
                           (let* ((hint (find-if (lambda (h) (char= (first h) (char text 0)))
                                                 hint-list))
                                  (entry-index (match-entry dictionary-array text
                                                            :start (if start start (second hint))
                                                            :end   (third hint))))
                             (cond ((and start (not entry-index)))
                                   (entry-index
                                    (collect-segmentations text
                                                           :limit limit
                                                           :segments (copy-list segments)
                                                           :start (1+ entry-index))
                                    (let ((entry (aref dictionary-array entry-index)))
                                      (collect-segmentations (subseq text (length entry))
                                                             :limit limit
                                                             :segments (append segments
                                                                               (list entry))))))))))))
           (collect-segmentations text :limit limit))
       :stop)
      result-segments)))
