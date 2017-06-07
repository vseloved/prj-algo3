(defstruct (queue (:constructor %make-queue (xs &aux (ys (last xs)))))
  (head xs :read-only t)
  (tail ys :read-only t))

(defun make-queue (&key initial-contents)
  (%make-queue (cons nil initial-contents)))

(defmethod print-object ((q queue) stream)
  (print-unreadable-object (q stream :type t)
    (format stream "~:[EMPTY~;~:*~a~]" (rest (queue-head q)))))

(defun queue-push (x q)
  (with-slots (tail) q
    (first (setf tail (cdr (rplacd tail (list x)))))))

(defun queue-pop (q)
  (with-slots (head) q
    (when (rest head)
      (first (setf head (rest head))))))
