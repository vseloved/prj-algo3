;(defun load-dict-from-file (filename)
;  (with-open-file (stream filename)
;    (loop repeat 100
;      for line = (read-line stream nil)
;      while line
;      collect line
;    )
;  )
;)

(defun get-test-dict()
  (let ((dict (make-hash-table :test 'equal)))
    (loop for item in (list "aaa" "ab")
      do (setf (gethash item dict) item))
    (return-from get-test-dict dict))
)
(print (get-test-dict))

(defun segment-str(s dict &optional (mem (make-hash-table :test 'equal)))
  (when (gethash s dict)
    (return-from segment-str s))
  (loop for i from 1 to (length s)
    do (let ((prefix (subseq s 0 i)))
         (if (gethash prefix dict)
             (let ((res (segment-str (subseq s i) dict mem)))
               (if (stringp res)
                 (return-from segment-str (concatenate 'string prefix " " res))))))
  )
)

(print (segment-str "aaaabab" (get-test-dict)))
