
(defun restore-short-path (relax-array destination get-u-fun)
  (loop
     :with v = destination
     :for  u = (funcall get-u-fun (aref relax-array v))
         :then (funcall get-u-fun (aref relax-array v))
     :while (setf v u)
     :collect u :into path
     :finally (return (reverse (append (list destination) path)))))

;;; Dijkstra algorithm
;;;
(defun shortest-path-dijkstra (graph source destination)
  (let* ((adj-m       (adj-mat graph))
         (nodes-count (hash-table-count (nodes graph)))
         (relax-a     (make-array (1+ nodes-count) :initial-element (make-heap-cell)))
         (queue       (make-array 0 :element-type 'heap-cell :adjustable t :fill-pointer 0)))
    (dotimes (i nodes-count)
      (min-heap-insert queue
                       (make-heap-cell :key     (if (= (1+ i) source) 0 most-positive-fixnum)
                                       :element (1+ i))))
    (loop
       :until (or (not queue)
                  (= destination (hc-element (aref queue 0))))
       :do (let* ((queue-min (heap-extract-min queue))
                  (u         (hc-element queue-min))
                  (u-d       (hc-key queue-min)))
             (loop
                :for i :from 0 :below (length queue)
                :do (let* ((v (hc-element (aref queue i)))
                           (w (aref adj-m (1- u) (1- v))))
                      (unless (= w most-positive-fixnum)
                        (cond ((> (invert-key (hc-key (aref queue i))) (+ u-d w))
                               (heap-decrease-key queue i (+ u-d w))
                               (setf (aref relax-a v) queue-min))))))))
    (values (if queue
                (hc-key (heap-extract-min queue))
                most-positive-fixnum)
            (restore-short-path relax-a destination #'hc-element))))

;;;
;;; ((TEST-SHORTEST-PATH-DIJKSTRA *EXAMPLE-GRAPH*)) took    0.0013 sec
;;;
(defun test-shortest-path-dijkstra (graph)
  (let ((nodes-count (hash-table-count (nodes graph)))
        (m           (adj-mat graph)))
    (dotimes (u nodes-count)
      (dotimes (v nodes-count)
         (setf (aref m u v)
               (shortest-path-dijkstra graph (1+ u) (1+ v)))))
    m))

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
            (restore-short-path relax-a destination #'cdr))))

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
