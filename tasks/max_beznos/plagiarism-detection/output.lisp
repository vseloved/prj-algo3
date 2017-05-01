(defpackage #:output
  (:use #:cl)
  (:export :write-detected-sources
           :make-source-ref
           :extract-source-ref
           :remove-duplicated-source-refs))

(in-package #:output)

(defstruct (source-ref (:conc-name sref-))
  source-filename
  this-offset
  this-length
  source-offset
  source-length)

(defun extract-length (word-list extract-fn)
  (let ((start (funcall extract-fn (first word-list)))
        (end   (+ (length (first (car (last word-list))))
                  (funcall extract-fn (car (last word-list))))))
    (- end start)))

(defun extract-source-ref (raw-source-ref-list)
  (let ((wms (second raw-source-ref-list)))
    (make-source-ref :source-filename (first raw-source-ref-list)
                     :this-offset   (third (first wms))
                     :this-length   (extract-length wms #'third)
                     :source-offset (second (first wms))
                     :source-length (extract-length wms #'second))))

(defun has-overlapping (sref others)
  (loop
     :for other-sref :in others
     :thereis (and (>= (sref-this-offset sref) (sref-this-offset other-sref))
                   (<= (sref-this-length sref) (sref-this-length other-sref)))))

(defun remove-duplicated-source-refs (source-ref-list)
  (let ((selected-refs-ht (make-hash-table :test 'equalp)))
    (loop
       :for sref :in source-ref-list
       :do (multiple-value-bind (per-source-refs found)
               (gethash (sref-source-filename sref) selected-refs-ht (list))
             (when (or (not found)
                       (not (has-overlapping sref per-source-refs)))
               (push sref per-source-refs)
               (setf (gethash (sref-source-filename sref) selected-refs-ht)
                     per-source-refs))))
    (loop
       :for sref :being :the :hash-value :in selected-refs-ht
       :append sref)))

(defun detect-output-pathname (input-pathname)
  (make-pathname :directory (let ((dir-path (pathname-directory input-pathname)))
                              (append (butlast dir-path)
                                      (list (concatenate 'string (car (last dir-path)) "-detect"))))
                 :name (concatenate 'string (pathname-name input-pathname) "-detect")
                 :type "xml"))

(defun make-short-filename (path)
  (concatenate 'string (pathname-name path) "." (pathname-type path)))

(defun write-detected-sources (processed-reference detected-sources)
  (with-open-file (ofs (ensure-directories-exist (detect-output-pathname processed-reference))
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create
                       :element-type '(unsigned-byte 8))
    (loop
       :for byte
       :across (babel:string-to-octets
                (with-output-to-string (os)
                  (format os "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
                  (format os "<document reference=~s>~%"
                          (make-short-filename processed-reference))
                  (loop
                     :for ds :in detected-sources
                     :do (format os "  <feature name=\"detected-plagiarism\" this_offset=\"~d\"
           this_length=\"~d\" source_reference=~s source_offset=\"~d\" source_length=\"~d\" />~%"
                                 (sref-this-offset ds)
                                 (sref-this-length ds)
                                 (make-short-filename (sref-source-filename ds))
                                 (sref-source-offset ds)
                                 (sref-source-length ds)))
                  (format os "</document>"))
                :use-bom t)
       :do (write-byte byte ofs))))
