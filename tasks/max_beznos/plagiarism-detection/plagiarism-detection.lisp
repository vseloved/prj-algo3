(defpackage #:plagiarism-detection
  (:nicknames #:pd)
  (:use #:cl)
  (:export :process-corpus
           :process-suspicious-part
           :process-suspicious-file))

(in-package #:plagiarism-detection)

(defun txt-file-p (path)
  (string= "txt" (pathname-type path)))

(defun load-source (source-dir-path)
  (srcindex:reset-source-index)
  (sb-ext:gc :full t)
  (cl-fad:walk-directory source-dir-path #'srcindex:build-source-index
                         :test #'txt-file-p))

(defun process-suspicious-file (suspicious-file &key min-words-match)
  (output:write-detected-sources suspicious-file
                                 (output:remove-duplicated-source-refs
                                  (map 'list #'output:extract-source-ref
                                       (srcindex:find-sources
                                        (srcindex:word-list-from-file suspicious-file)
                                        :min-words-match min-words-match)))))

(defun process-suspicious-part (suspicious-dir-path &key min-words-match (threads 1))
  (cond ((= threads 1)
         (cl-fad:walk-directory suspicious-dir-path
                                (lambda (path)
                                  (format t "~a~%" (pathname-name path))
                                  (process-suspicious-file path :min-words-match min-words-match))
                                :test #'txt-file-p))
        (t
         (setf lparallel:*kernel* (lparallel:make-kernel threads))
         (lparallel:pmapcar (lambda (path)
                              (when (txt-file-p path)
                                (process-suspicious-file path :min-words-match min-words-match)))
                            :parts threads
                            (cl-fad:list-directory suspicious-dir-path))
         nil)))

(defun process-corpus (source-dir-path suspicious-dir-path plagiarism-detect-dir-path
                       &key min-words-match)
  (load-source source-dir-path)
  (process-suspicious suspicious-dir-path plagiarism-detect-dir-path
                      :min-words-match min-words-match))

