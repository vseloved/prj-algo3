(defparameter *example-graph*
  (graph '(1 2 7)
         '(1 3 3)
         '(2 4 8)
         '(2 5 3)
         '(3 4 2)
         '(3 5 1)
         '(4 6 2)
         '(5 4 9)
         '(5 6 1)
         '(7 8 10)))

;;; graphs representing different ways to split a string "thisisatest"
;;; with a dictonary:
;;;  a
;;;  i
;;;  t
;;;  s
;;;  is
;;;  at
;;;  his
;;;  this
;;;  test

(defparameter *strsplit-basic-graph*
  (graph '(0 1 "t")
         '(1 4 "his")
         '(0 4 "this")
         '(4 5 "i")
         '(5 6 "s")
         '(4 6 "is")
         '(6 7 "a")
         '(6 8 "at")
         '(7 8 "t")
         '(7 11 "test")))

(defparameter *strsplit-1gram-graph*
  (graph '(0 1 "p(t)")
         '(1 4 "p(his)")
         '(0 4 "p(this)")
         '(4 5 "p(i)")
         '(5 6 "p(s)")
         '(4 6 "p(is)")
         '(6 7 "p(a)")
         '(6 8 "p(at)")
         '(7 8 "p(t)")
         '(7 11 "p(test)")))

(defparameter *strsplit-2grams-graph*
  (graph '(0 4 "p(t his)")
         '(0 5 "p(this i)")
         '(0 6.1 "p(this is)")
         '(4 5 "p(his i)")
         '(4 6.1 "p(his is)")
         '(5 6.2 "p(i s)")
         '(6.1 7 "p(is a)")
         '(6.2 7 "p(s a)")
         '(5 8 "p(is at)")
         '(6.2 8 "p(s at)")
         '(7 8 "p(a t)")
         '(7 11 "p(a test)")))

