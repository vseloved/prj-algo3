(in-package :cl-user)

(defpackage :break-text-test
  (:use :common-lisp :lisp-unit :break-text))

(in-package :break-text-test)

(use-package :break-text)

(setq *print-failures* t)
(setq *print-summary* t)

(define-test test-basic-correctness 
  (let ((*words-dict* (make-hash-table :test #'equal))
	(*bi-grams-dict* (make-hash-table :test #'equal)))
    (load-dict-from-file "data/short_dict_en.txt" #'add-word)
    (load-dict-from-file "data/short_bi_grams.txt" #'add-bi-gram)
    (assert-equal "a cat" (break-text "acat"))
    (assert-equal "this is a test" (break-text "thisisatest"))
    (assert-equal "I have a dream" (break-text "Ihaveadream"))
    (assert-equal "Colorless green ideas sleep furiously"
		  (break-text "Colorlessgreenideassleepfuriously"))
    (assert-equal "Buffalo Buffalo Buffalo Buffalo"
		  (break-text "BuffaloBuffaloBuffaloBuffalo"))))
