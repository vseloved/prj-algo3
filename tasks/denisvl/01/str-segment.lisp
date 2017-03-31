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


(defun words-for-position(s pos dict)
  (loop
    for len from 1 to (min 10 (- (length s) pos))
    for prefix = (subseq s pos (+ pos len))
    when (gethash prefix dict)
    collect len)
)

(defun get-dp(s dict)
  (let ((dp (make-array (+ 1 (length s)) :initial-element nil)))
    (loop
      for pos from 0 to (- (length s) 1)
      for words = (words-for-position s pos dict)
      when (or (= pos 0) (aref dp pos))
      do (loop
           for p in words
           when (not (aref dp (+ pos p)))
           do (setf (aref dp (+ pos p)) pos)))
    (return-from get-dp dp))
)

(defun get-path(dp)
  (reverse (loop
    for p = (- (length dp) 1) then (aref dp p)
    until (or (= p 0))
    collect p))
)

(defun segment-dp(s dict)
  (let ((dp (get-dp s dict))
        (pos 0))
    (if (not (aref dp (length s)))
      (return-from segment-dp s))
    (return-from segment-dp (apply #'concatenate 'string (loop
      for w in (get-path dp)
      collect (subseq s pos w)
      when (< w (length s))
      collect " "
      do (setf pos w)))))
)

(print (segment-dp "abaaaabababab" (get-test-dict)))
