
;;; Bellman-Ford algorithm
;;;
(defun shortest-path-bf (graph source destination)
  (let* ((adj-m       (adj-mat graph))
         (nodes-count (hash-table-count (nodes graph)))
         (rm-adj-a    (make-array (array-total-size adj-m)
                                  :displaced-to adj-m
                                  :element-type (array-element-type adj-m)))
         (relax-a     (make-array (1+ nodes-count)
                                  :initial-element (cons most-positive-fixnum nil))))
    (setf (aref relax-a source) (cons 0 nil))
    (loop
       :repeat (1- nodes-count)
       :do (loop
              :for edge-index :from 0 :below (array-total-size rm-adj-a)
              :do (multiple-value-bind (u v)
                      (values (1+ (floor edge-index nodes-count))
                              (1+ (mod edge-index nodes-count)))
                    (unless (= u v)
                      (let* ((u-d (car (aref relax-a u)))
                             (v-d (car (aref relax-a v)))
                             (w   (aref rm-adj-a edge-index)))
                        (when w
                          (cond ((> v-d (+ u-d w))
                                 (setf (aref relax-a v) (cons (+ u-d w) u))))))))))
    (values (car (aref relax-a destination))
            (loop
               :with v = destination
               :for  u = (cdr (aref relax-a v)) :then (cdr (aref relax-a v))
               :while (setf v u)
               :collect u :into path
               :finally (return (reverse (append (list destination) path)))))))

;;;
;;; ((TEST-SHORTEST-PATH-BF *EXAMPLE-GRAPH*)) took  0.008787 sec
;;;
(defun test-shortest-path-bf (graph)
  (let ((nodes-count (hash-table-count (nodes graph)))
        (m           (adj-mat graph)))
    (dotimes (u nodes-count)
      (dotimes (v nodes-count)
         (setf (aref m u v)
               (shortest-path-bf graph (1+ u) (1+ v)))))
    m))
