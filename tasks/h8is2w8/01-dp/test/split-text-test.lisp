(in-package :cl-user)

(defpackage :split-text-test
  (:use :common-lisp :lisp-unit :split-text)
  (:export :test-code))

(in-package :split-text-test)

(setq *print-failures* t)

;;; utils

(defun find-s (str-1 str-2)
  (find str-1 (split-text::SPLIT-TEXT str-2) :test #'equal))

;;; tests

(split-text::load-dict "test/short_dict_en.txt")

(define-test test-basic-correctness
  (assert-true (find-s "this is test" "thisistest"))
  (assert-true (find-s "a leaf" "aleaf"))
  (assert-true (find-s "cat and dog" "catanddog")))
