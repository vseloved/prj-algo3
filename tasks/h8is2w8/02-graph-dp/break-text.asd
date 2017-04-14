(in-package :asdf)

(asdf:defsystem :break-text
  :description "Add spaces to non-space text."
  :depends-on (:cl-ppcre)
  :components ((:module "src"
			:components
			((:file "package")
			 (:file "break-text")
			 (:file "graph")
			 (:file "tsort")))))

(asdf:defsystem :break-text-test
  :description "break-text test suit."
  :depends-on (:break-text :lisp-unit)
  :components ((:module "test"
			:components
			((:file "break-text-test")))))
