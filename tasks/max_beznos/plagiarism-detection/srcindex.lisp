(defpackage #:srcindex
  (:use #:cl)
  (:export :local-thread-bindings
           :word-list-from-file
           :word-list-from-text
           :reset-source-index
           :build-source-index
           :length-in-chars
           :find-sources))

(in-package #:srcindex)

(defparameter *min-word-length*          10)
(defparameter *mismatch-threshold*       0.05)
(defparameter *mismatch-count-credit*   -10)
(defparameter *srcindex-hash-table-size* 4000000)
(defparameter *word-list-memo-max-depth* 1)

(defvar *source-index* nil)

(defvar *word-list-memo* nil)

(defvar *word-load-buffer* (make-array 1024 :element-type 'character :fill-pointer 0))

(defun reset-source-index ()
  (setf *source-index* nil)
  (setf *word-list-memo* nil)
  (sb-ext:gc :full t)
  (setf *source-index* (make-hash-table :test 'equalp :size *srcindex-hash-table-size*)))

(defun local-thread-bindings ()
  (list `(*word-list-memo*   . (list))
        `(*word-load-buffer* . (make-array 1024 :element-type 'character :fill-pointer 0))
        `(*standard-output*  . ,*standard-output*)
        `(*error-output*     . ,*error-output*)))

(deftype index-type ()
  `(integer 0 ,(expt 2 24)))

(defstruct (source-reference (:conc-name sr-))
  (filename  "" :type pathname)
  (offset     0 :type index-type))

(defun word-list (istream &optional (split-offset nil))
  (declare (optimize (speed 3) (safety 0)))
  (let ((w *word-load-buffer*)
        (ws-next (list))
        (ws-back (list))
        (offset  (the fixnum 0)))
    (labels ((push-unless-empty (w pos)
               (declare (type (vector character) w))
               (unless (string= w "")
                 (if (and split-offset
                          (< offset (the fixnum split-offset)))
                     (push (list (copy-seq w) (the fixnum (- pos (length w)))) ws-back)
                     (push (list (copy-seq w) (the fixnum (- pos (length w)))) ws-next))
                 (incf (the fixnum offset)))))
      (loop
         :with i = 0
         :for  c = (read-char istream nil '*eof*)
         :for  p = (the fixnum 0) :then (the fixnum (1+ p))
         :until (eql c '*eof*)
         :do (cond ((and (or (char= c #\Space)
                             (char= c #\Newline)))
                    (when (push-unless-empty w p)
                      (setf (fill-pointer w) 0
                            i 0)))
                   ((let ((d (char-code c)))
                      (or (and (/= i 0) (or (= d #x2d) (= d #x27)))
                          (and (<= #x41 d) (<= d #x5a))
                          (and (<= #x61 d) (<= d #x7a))))
                    (incf (fill-pointer w))
                    (setf (aref w i) c)
                    (incf i)))
         :finally (push-unless-empty w p)))
    (values (nreverse ws-next)
            ws-back)))

(defun word-list-from-file (filename)
  (with-open-file (is filename)
    (word-list is)))

(defun word-list-from-text (text)
  (with-input-from-string (is text)
    (word-list is)))

(defstruct (word-list-memo (:conc-name wlm-))
  filename
  (split-offset 0 :type index-type)
  next-list
  back-list)

(defun resplit-word-list-memo (memo split-offset)
  (declare (optimize (speed 3) (safety 0)))
  (let ((next-list (wlm-next-list memo))
        (back-list (wlm-back-list memo)))
    (loop
       :repeat (abs (- (wlm-split-offset memo) (the index-type split-offset)))
       :do (cond ((< split-offset (wlm-split-offset memo))
                  (push (pop back-list) next-list))
                 ((> split-offset (wlm-split-offset memo))
                  (push (pop next-list) back-list))))
    (setf (wlm-split-offset memo) split-offset
          (wlm-next-list memo) next-list
          (wlm-back-list memo) back-list)
    (values next-list
            back-list
            t)))

(defun word-list-from-file-use-memo (filename &optional (split-offset nil))
  (declare (optimize (speed 3) (safety 0)))
  (let ((memo (find-if (lambda (m)
                         (pathname-match-p filename (wlm-filename m)))
                       *word-list-memo*)))
    (cond ((and memo (pathname-match-p filename (wlm-filename memo)))
           (resplit-word-list-memo memo split-offset))
          (t
           (multiple-value-bind (next-list back-list)
               (with-open-file (is filename)
                 (word-list is split-offset))
             (let ((new-memo (make-word-list-memo :filename filename
                                                  :split-offset split-offset
                                                  :next-list next-list
                                                  :back-list back-list)))
               (if (> *word-list-memo-max-depth* (length *word-list-memo*))
                   (push new-memo *word-list-memo*)
                   (progn (alexandria:rotate *word-list-memo*)
                          (setf (car *word-list-memo*) new-memo))))
             (values next-list
                     back-list
                     nil))))))

(defvar *build-source-index-mutex* (lparallel.thread-util:make-lock))

(defun build-source-index (filename)
  (let ((word-list (word-list-from-file filename)))
    (loop
       :with st-total-inserts = 0
       :for wp :in word-list
       :for offset = (the index-type 0) :then (the index-type (1+ offset))
       :do (lparallel.thread-util:with-lock-held (*build-source-index-mutex*)
             (let* ((w (first wp))
                    (source (gethash w *source-index* (list))))
               (when (>= (length w) *min-word-length*)
                 (push (make-source-reference :filename filename :offset offset) source)
                 (incf st-total-inserts)
                 (setf (gethash w *source-index*) source))))
       :finally (return st-total-inserts))))

(defun source-match (source-ws text-ws)
  (declare (optimize (speed 3) (safety 0)))
  (loop
     :with end-wp
     :with mismatch-count = (the fixnum *mismatch-count-credit*)
     :for current-length  = (the fixnum 0) :then (the fixnum (1+ current-length))
     :for twp :in text-ws
     :for swp :in source-ws
     :while (or (= 0 current-length)
                (< (/ mismatch-count current-length) (the single-float *mismatch-threshold*)))
     :do (progn
           (setf end-wp
                 (list (first swp) (second swp) (second twp)))
           (unless (string= (the (vector character) (first twp))
                            (the (vector character) (first swp)))
             (incf (the fixnum mismatch-count))))
     :finally (return (cons (list end-wp) current-length))))

(defun length-in-chars (word-list extract-fn)
  (declare (optimize (speed 3) (safety 0))
           (type function extract-fn))
  (let ((start (funcall extract-fn (first word-list)))
        (end   (+ (length (the (vector character) (first (car (last word-list)))))
                  (the fixnum (funcall extract-fn (car (last word-list)))))))
    (- (the index-type end) (the index-type start))))

(defun find-sources (word-list &key (min-words-match 2))
  (declare (optimize (speed 3) (safety 0)))
  (let ((skip-offset-range (1- (* 2 (the fixnum (abs (the fixnum *mismatch-count-credit*))))))
        (sources-to-search (loop
                              :for wp :in word-list
                              :for offset = 0 :then (the fixnum (1+ offset))
                              :append (mapcar (lambda (s)
                                                (list offset (sr-filename s) (sr-offset s)))
                                              (gethash (first wp) *source-index*)))))
    (sort sources-to-search #'string<
          :key (lambda (s) (pathname-name (second s))))
    (loop
       :with sources = (list)
       :with target-memo = (make-word-list-memo :filename nil :split-offset 0
                                                :next-list word-list :back-list nil)
       :with target-previous-offset = nil
       :for sentry :in sources-to-search
       :for progress = 0 :then (the fixnum (1+ progress))
       :when (or (not target-previous-offset)
                 (> (abs (- (the fixnum target-previous-offset)
                            (the fixnum (first sentry))))
                    skip-offset-range))
       :do (multiple-value-bind (next-list back-list)
               (resplit-word-list-memo target-memo (first sentry))
             (multiple-value-bind (sref-next-list sref-back-list cache-is-used)
                 (word-list-from-file-use-memo (second sentry) (third sentry))
               (setf target-previous-offset (if cache-is-used (first sentry) nil))
               (let* ((w (first  (car next-list)))
                      (p (second (car next-list)))
                      (source-seed-position (second (car sref-next-list)))
                      (match-tail (source-match (cdr sref-next-list) (cdr next-list)))
                      (match-head (source-match sref-back-list back-list))
                      (match-len  (+ 1 (the fixnum (cdr match-head))
                                       (the fixnum (cdr match-tail))))
                      (match-full
                       (append (car match-head)
                               (list (list w source-seed-position p))
                               (reverse (car match-tail)))))
                 (when (>= (the fixnum match-len) (the fixnum min-words-match))
                   (push (list (second sentry) (length-in-chars match-full #'third)
                               match-full w source-seed-position)
                         sources)))))
       :finally (return (sort sources #'> :key #'second)))))
