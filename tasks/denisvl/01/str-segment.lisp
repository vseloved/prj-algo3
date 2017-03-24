(defun load-dict-from-file (filename)
  (let ((dict (make-hash-table :test 'equal)))
    (with-open-file (stream filename)
      (loop
        for line = (read-line stream nil)
        while line
        do (setf (gethash (string-right-trim '(#\Newline #\Return) line) dict) line)))
    (return-from load-dict-from-file dict)
  )
)

(print (load-dict-from-file "../../dict_en.txt"))

(defun get-test-dict()
  (let ((dict (make-hash-table :test 'equal)))
    (loop for item in (list "aaa" "ab")
      do (setf (gethash item dict) item))
    (return-from get-test-dict dict))
)
(print (get-test-dict))

(defun segment-str(s dict &optional (mem (make-hash-table :test 'equal)))
  (or (gethash s dict)
      (gethash s mem)
      (loop
        for i from 1 to (length s)
        for prefix = (subseq s 0 i)
        do (if (gethash prefix dict)
             (let ((res (segment-str (subseq s i) dict mem)))
               (if (stringp res)
                 (progn
                   (setf (gethash s mem) (concatenate 'string prefix " " res))
                   (return-from segment-str (gethash s mem))))))))
)

(print (segment-str "aaaab" (get-test-dict)))

(print (segment-str "helloworld" (load-dict-from-file "../../dict_en.txt")))
