;;;; plagiarism-detection.asd

(asdf:defsystem #:plagiarism-detection
  :description "Describe plagiarism-detection here"
  :author "maxim beznos"
  :license "Specify license here"
  :depends-on (#:alexandria #:babel #:cl-fad #:lparallel)
  :serial t
  :components ((:file "srcindex")
               (:file "output")
               (:file "plagiarism-detection")))

