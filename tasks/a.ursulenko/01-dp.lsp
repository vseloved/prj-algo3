(defun prompt-read (msg)
  "Helper for reading from standard input"
  (format t "~a > " msg)
  (force-output *query-io*)
  (read-line *query-io*))

(defun make-list-array (size)
  "Return array of lists for given size"
  (make-array size :initial-element (list)))

(defun find-matched (seq words current)
  "Find all possible words for given sequence. Return pairs of word - index"
  (mapcar
    #'(lambda (word) (list (+ current (length word)) word))
    (let ((matched (list)))
      (loop for i from 1 to (length seq) do
            (let ((subword (subseq seq 0 i)))
              (when (gethash subword words)
                  (push subword matched))))
      matched)))

(defun combine-results (current-index
                         found-matches
                         &optional (current-split (list)))
  "Run through indexed solution and combine all possible splits and print them"
  (let ((current-matched (aref found-matches current-index)))
    (when (not current-matched)
        (format t "~{~a~^ ~}~%" current-split))
    (dolist
      (index-word current-matched)
      (destructuring-bind
        (index word) index-word
        (combine-results
                        index
                        found-matches
                        (cons word current-split))))))

(defun split-sentence (sentence words)
  "Find all mathing words for starting position and combine them into sentence"
  (let ((found-matches (make-list-array (1+ (length sentence)))))
    (loop
      for i from 0 to (1- (length sentence)) do
      ;; If its not first cell and it has no indexes, then there is no possible
      ;; way to get here by previous subwords, so just skip this position
      (when (or (zerop i) (< 0 (length (aref found-matches i))))
        (dolist
          (matched (find-matched (subseq sentence i) words i))
          (destructuring-bind
            (index word) matched
            (let ((already-indexed (aref found-matches index)))
              (setf
                (aref found-matches index)
                (cons (list i word) already-indexed)))))))
    (combine-results (length sentence) found-matches)))

(defun read-input-dict ()
  "Read dict from input"
  (let ((words (make-hash-table :test 'equal)))
    (loop
      (let ((word (prompt-read "Enter word (type \"!\" to stop)")))
        (if (string= word "!")
            (return words)
            (setf (gethash word words) t))))
    words))

(defun read-file-dict (filepath)
  "Read contents of dict file"
  (let ((in (open filepath))
        (words (make-hash-table :test 'equal)))
    (when in
      (loop for word = (read-line in nil) while word do
            (setf (gethash (string-right-trim '(#\Return) word) words) t))
      (close in))
    words))

(defun main ()
  "Main function which reads required input nad tries to split given sentence"
  (split-sentence
    (prompt-read "Enter string to fix")
    (read-file-dict (prompt-read "Enter path to dict file"))))
    ; (read-input-dict)))

(main)
