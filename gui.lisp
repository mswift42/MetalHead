(load "~/quicklisp/setup.lisp")
(ql:quickload "ltk")
(load "actions.lisp")

(defpackage :gui
  (:use :cl :ltk :actions) 
  (:export *text-field make-frame))

(in-package :gui)



(defparameter *text-field*
  (make-instance 'text
		 :font "monospaced" :takefocus :t))


(defun make-frame ()
  (with-ltk ()
    (let* ((f (make-instance 'frame))
	   (scroll (make-instance 'scrolled-text :master f))
	   (outtext (textbox scroll)))
      (pack f)
      (configure outtext :font "monospaced" :background "#aea79f" :wrap :word)
      (pack scroll :anchor :nw :expand t :fill :both)
      (pack *text-field* :side :bottom :expand nil)
      (bind *text-field* "<KeyPress-Return>"
	    (lambda (event) (format-output outtext)))
      (configure f :borderwidth 2))))


(defun format-output (target)
  "Print inputstring with newlines and > ."
  (append-text target (format nil "~%~%> ~A~%~%" (text *text-field*)))
  (clear-text *text-field*)
  (print-silly-stuff target)
  (append-text target
	       (format nil "~A"  (actions::describe-room (actions::current-location)))))


(defun copy-text (target)
  (let ((inp (text *text-field*)))
    (format nil "~&~A" inp)
    (format nil "~&~A" inp)))

(defun print-silly-stuff (target)
  (append-text target "A lot of silly stuff."))



