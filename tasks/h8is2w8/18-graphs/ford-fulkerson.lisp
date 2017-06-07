(load "../utils/graph.lisp")
(load "../utils/bfs.lisp")

(defparameter *graph*
  (adj-list->adj-mat
   (make-graph '(0 1 16)
	       '(0 2 13)
	       '(1 3 12)
	       '(1 2 10)
	       '(2 1 4)
	       '(2 4 14)
	       '(3 5 20)
	       '(3 2 9)
	       '(4 3 7)
	       '(4 5 4))))

(defun ford-fulkerson (g source sink)
  (let* ((size (sqrt (array-total-size g)))
	 (paths (make-array size :initial-element nil))
	 (r (make-array (array-dimensions g))))

    ;;; Fill in redsiduals graph.
    (loop :for i :from 0 :below size :do
       (loop :for j :from 0 :below size :do
	  (setf (aref r i j) (aref g i j))))
    
    ;;; Find maximum flow.
    (loop :with max-flow = 0 :while (bfs-on-mat r source sink paths) :do
       (let ((tmp-flow most-positive-fixnum))
	 ;;; Find possible flow.
	 (loop :with current = sink :while (not (eql current source)) :do
	    (let ((next (aref paths current)))
	      (setf tmp-flow (min tmp-flow (aref r next current))
		    current next)))
	 ;;; Decrease residual capacity.
	 (loop :with current = sink :while (not (eql current source)) :do
	    (let ((next (aref paths current)))
	      (decf (aref r next current) tmp-flow)
	      (incf (aref r current next) tmp-flow)
	      (setf current next)))
	 (incf max-flow tmp-flow))   
       :finally (return max-flow))))

(defun test ()
  (assert 23 (ford-fulkerson *graph* 0 5)))
