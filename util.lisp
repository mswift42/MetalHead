(defpackage #:utilities
  (:use :cl)
  (:export flatten))

(in-package #:utilities)

(defun flatten (structure)
  (cond ((null structure) nil)
        ((atom structure) `(,structure))
        (t (mapcan #'flatten structure))))



