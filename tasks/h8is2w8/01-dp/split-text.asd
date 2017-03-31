(in-package :asdf)

(asdf:defsystem :split-text
  :description "Split a text without whitespaces into separate words."
  :components ((:module "src"
			:components
			((:file "split-text")))))

(asdf:defsystem :split-text-test
  :description "split-text test suit."
  :depends-on (:split-text :lisp-unit)
  :components ((:module "test"
			:components
			((:file "split-text-test")))))
