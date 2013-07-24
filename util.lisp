(defpackage #:utilities
  (:use :cl)
  (:export flatten equalassoc equalmember))

(in-package #:utilities)

(defun flatten (structure)
  "flatten a list."
  (cond ((null structure) nil)
        ((atom structure) `(,structure))
        (t (mapcan #'flatten structure))))

(defmacro equalassoc (exp list)
  "test assoc for equal to work with strings."
  `(assoc ,exp ,list :test #'string-equal))

(defmacro equalmember (exp list)
  "test member for equal to work with stirngs."
  `(member ,exp ,list :test #'string-equal))







