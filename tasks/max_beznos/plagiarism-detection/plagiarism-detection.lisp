(defpackage #:plagiarism-detection
  (:nicknames #:pd)
  (:use #:cl)
  (:export :process-corpus
           :process-suspicious-part
           :process-suspicious-file))

(in-package #:plagiarism-detection)

(defun txt-file-p (path)
  (string= "txt" (pathname-type path)))

(defun collect-txt-files (dir)
  (let ((files))
    (cl-fad:walk-directory dir (lambda (path) (push path files)) :test #'txt-file-p)
    files))

(defun load-source (source-dir-path &key (threads 1))
  (srcindex:reset-source-index)
  (setf lparallel:*kernel* (lparallel:make-kernel threads))
  (let ((total-inserts (reduce #'+
                               (lparallel:pmapcar #'srcindex:build-source-index
                                                  :parts threads
                                                  (collect-txt-files source-dir-path)))))
    (format t "source index statistics~%")
    (format t "hash table size  : ~d~%" (hash-table-size srcindex::*source-index*))
    (format t "hash table count : ~d~%" (hash-table-count srcindex::*source-index*))
    (format t "total inserts    : ~d~%" total-inserts)))

(defun process-suspicious-file (suspicious-file &key min-words-match)
  (output:write-detected-sources suspicious-file
                                 (output:remove-duplicated-source-refs
                                  (map 'list #'output:extract-source-ref
                                       (srcindex:find-sources
                                        (srcindex:word-list-from-file suspicious-file)
                                        :min-words-match min-words-match)))))

(defun process-suspicious-part (suspicious-dir-path &key min-words-match (threads 1))
  (setf lparallel:*kernel* (lparallel:make-kernel threads))
  (lparallel:pmapcar (lambda (path)
                       (when (txt-file-p path)
                         (process-suspicious-file path :min-words-match min-words-match)))
                     :parts threads
                     (cl-fad:list-directory suspicious-dir-path))
  nil)

(defun process-corpus (source-dir-path suspicious-dir-path plagiarism-detect-dir-path
                       &key min-words-match)
  (load-source source-dir-path)
  (process-suspicious suspicious-dir-path plagiarism-detect-dir-path
                      :min-words-match min-words-match))

