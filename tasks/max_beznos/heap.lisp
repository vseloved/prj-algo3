
(defstruct (heap-cell (:conc-name hc-))
  (key     most-positive-fixnum)
  (element nil))

(defgeneric max-heap-compare (x y)
  (:method ((x heap-cell) (y heap-cell))
    (> (hc-key x) (hc-key y))))

(defgeneric invert-key (k)
  (:method ((k fixnum))
    (case k
      (most-positive-fixnum most-negative-fixnum)
      (most-negative-fixnum most-positive-fixnum)
      (otherwise (* -1 k))))
  (:method ((k heap-cell))
    (let ((key (hc-key k)))
      (make-heap-cell :key (invert-key key)
                      :element (hc-element k)))))

(defun hparent (i)
  (floor i 2))

(defun hright (i)
  (1+ (* i 2)))

(defun hleft (i)
  (- (hright i) 1))

(defun max-heapify (a i)
  (let ((maxi)
        (heap-size (length a)))
    (if (and (< (hleft i) heap-size)
             (max-heap-compare (aref a (hleft i))
                               (aref a i)))
        (setf maxi (hleft i))
        (setf maxi i))
    (when (and (< (hright i) heap-size)
               (max-heap-compare (aref a (hright i))
                                 (aref a maxi)))
      (setf maxi (hright i)))
    (unless (= maxi i)
      (rotatef (aref a maxi) (aref a i))
      (max-heapify a maxi))))

(defun heap-extract-max (a)
  (rotatef (aref a 0) (aref a (1- (length a))))
  (let ((max (vector-pop a)))
    (max-heapify a 0)
    max))

(defun heap-extract-min (a)
  (invert-key (heap-extract-max a)))

(defun heap-increase-key (a index key)
  (setf (hc-key (aref a index)) key)
  (loop
     :for i = index :then (hparent i)
     :while (and (> i 0)
                 (max-heap-compare (aref a i)
                                   (aref a (hparent i))))
     :do (rotatef (aref a i)
                  (aref a (hparent i)))))

(defun heap-decrease-key (a index key)
  (heap-increase-key a index (invert-key key)))

(defun max-heap-insert (a item)
  (heap-increase-key a (vector-push-extend item a) (hc-key item)))

(defun min-heap-insert (a item)
  (max-heap-insert a (invert-key item)))



