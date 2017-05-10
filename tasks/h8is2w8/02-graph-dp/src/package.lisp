(in-package :cl-user)

(defpackage :break-text
  (:use :common-lisp)
  (:export :*words-dict*
	   :*bi-grams-dict*
	   :add-word
	   :add-bi-gram
	   :break-text
	   :load-dict-from-file))
