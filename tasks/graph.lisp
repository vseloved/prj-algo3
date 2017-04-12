(defparameter *example-graph*
  (graph '(1 2 7)
         '(1 3 3)
         '(2 4 8)
         '(2 5 3)
         '(3 2 1)
         '(3 4 2)
         '(3 5 1)
         '(4 6 2)
         '(4 1 2)
         '(5 4 9)
         '(5 6 4)
         '(6 3 4)
         '(7 8 10)
         '(5 8 1)
         '(9 10 3)))

(defun floyd-warshall (g)
  (let ((v (ht-count (nodes g)))
        (m (adj-mat g)))
    (dotimes (i v)
      (dotimes (j v)
        (dotimes (k v)
          (let ((alt (+ (aref m j i) (aref m i k))))
            (when (< alt (aref m j k))
              (setf (aref m j k) alt))))))
    m))

(defparameter *shortest-paths*
  (print-adj-mat *example-graph* (floyd-warshall *example-graph*)))

;;      1    2    3    4    5    6    7    8    9    10  
;;      __________________________________________________
;; 1   |0    4    3    5    4    7    -    5    -    -    
;; 2   |10   0    11   8    3    7    -    4    -    -    
;; 3   |4    1    0    2    1    4    -    2    -    -    
;; 4   |2    6    5    0    6    2    -    7    -    -    
;; 5   |11   9    8    9    0    4    -    1    -    -    
;; 6   |8    5    4    6    5    0    -    6    -    -    
;; 7   |-    -    -    -    -    -    0    10   -    -    
;; 8   |-    -    -    -    -    -    -    0    -    -    
;; 9   |-    -    -    -    -    -    -    -    0    3    
;; 10  |-    -    -    -    -    -    -    -    -    0    


;;; utilities

(ql:quickload :local-time)

(defmacro microtime ((&optional label) &body body)
  (with-gensyms (beg)
    `(let ((,beg (local-time:now)))
       (prog1 (progn ,@body)
         (format *debug-io* "~A took ~9F sec~%" (or ,label ',body)
                 (local-time:timestamp-difference (local-time:now) ,beg))))))

(defun print-adj-mat (graph mat)
  (let ((v (ht-count (nodes graph))))
    (format t "    ~{ ~4A~}~%" (keys (nodes graph)))
    (format t "     ~{~A~}~%" (loop :repeat (hash-table-count (nodes graph))
                                    :collect "_____"))
    (maphash (lambda (name node)
               (format t "~4A|" name)
               (maphash (lambda (name2 node2)
                          (let ((cur (aref mat (id node) (id node2))))
                            (format t "~4A "
                                    (if (= most-positive-fixnum cur)
                                        "-"
                                        cur))))
                        (nodes graph))
               (format t "~%"))
             (nodes graph)))
  (format t "~%")
  mat)
