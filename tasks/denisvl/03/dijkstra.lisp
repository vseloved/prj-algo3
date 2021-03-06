(load "~/Documents/algo/prj-algo3/scripts/graphs.lisp")
(load "~/Documents/algo/prj-algo3/tasks/graph.lisp")

(quicklisp:quickload 'cl-heap)

(defun dijkstra (graph v)
  (let* ((n (ht-count (nodes graph)))
        (res (make-array n :initial-element most-positive-fixnum))
        (queue (make-instance 'cl-heap:priority-queue :sort-fun #'>)))
    (setf (aref res v) 0)
    (cl-heap::enqueue queue (cons v 0) 0)
    (loop
      while (cl-heap::peep-at-queue queue)
      for top = (cl-heap::dequeue queue)
      for top_v = (car top)
      for top_d = (cdr top)
      when (<= top_d (aref res top_v))
      do (with ((edges (edges (gethash (+ top_v 1) (nodes *example-graph*)))))
          (when edges
          (dolist (edge edges)
                 (with ((dst (id (dst edge)))
                        (len (label edge))
                        (new_len (+ (aref res top_v) len)))
                   (when (< new_len (aref res dst))
                     (setf (aref res dst) new_len)
                     (cl-heap::enqueue queue (cons dst new_len) new_len)))))))
  res)
)

(print (dijkstra *example-graph* 2))
