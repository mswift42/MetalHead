
(in-package #:metalhead)

(defun flatten (structure)
  "flatten a list."
  (cond ((null structure) nil)
        ((atom structure) `(,structure))
        (t (mapcan #'flatten structure))))

(defmacro string-assoc (exp list)
  "test assoc for equal to work with strings."
  `(assoc ,exp ,list :test #'string-equal))

(defmacro string-member (exp list)
  "test member for equal to work with stirngs."
  `(member ,exp ,list :test #'string-equal))

(defun random-string (list)
  "pick a random string from a list of strings."
  (let ((len (length list)))
    (nth (random len) list)))

(defun last-element (list)
  "return last element in a list"
  (first (last list)))









