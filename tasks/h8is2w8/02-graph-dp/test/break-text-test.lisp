(in-package :cl-user)

(defpackage :break-text-test
  (:use :common-lisp :lisp-unit :break-text)
  (:export :test-code))

(in-package :break-text-test)

(setq *print-failures* t)
(setq *print-summary* t)

;;; utils

(defun break-text (str)
  (break-text::break-text str))

(defun setup-env ()
  (break-text::load-words "./data/short_dict_en.txt")
  (break-text::load-bi-grams "./data/short_bi_grams.txt"))

(defun reset-env ()
  (break-text::reset-words)
  (break-text::reset-bi-grams))

;;; tests

(define-test test-basic-correctness 
  (reset-env)
  (setup-env)
  (assert-equal "a cat" (break-text "acat"))
  (assert-equal "this is a test" (break-text "thisisatest"))
  (assert-equal "I have a dream" (break-text "Ihaveadream"))
  (assert-equal "Colorless green ideas sleep furiously"
		(break-text "Colorlessgreenideassleepfuriously"))
  (assert-equal "Buffalo Buffalo Buffalo Buffalo"
		(break-text "BuffaloBuffaloBuffaloBuffalo"))
  (reset-env))
