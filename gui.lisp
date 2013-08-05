(ql:quickload "ltk")
(load "actions.lisp")




(defpackage :gui
  (:use :cl :ltk :actions ) 
  (:export *text-field make-frame format-output
	   *store-string* entnewlinify))

(in-package :gui)


(defparameter *text-field*
  (make-instance 'text
		 :font "monospaced"))

(defun make-frame ()
  (with-ltk ()
    (let* ((f (make-instance 'frame :padding "\"3 3 3 3\""))
	   (scroll (make-instance 'scrolled-text :master f))
	   (outtext (textbox scroll)))
      (pack f )
      (configure outtext :font "monospaced"
		 :background "#e4d2c9" :wrap :word )
      (pack scroll :anchor :nw :expand t :fill :both :ipady 100 )
      (pack *text-field* :side :bottom :expand nil)
      (bind *text-field* "<KeyPress-Return>"
	    (lambda (event) (format-output outtext)))
      (configure f :borderwidth 2))))


(defun format-output (target)
  "Print inputstring with newlines and > .
   Store the inputted string as a list in *store-string*
   Clear *text-field*"
  (append-text target (format nil "~%~%> ~A" (text *text-field*)))
  (push (split-string (text *text-field*)) *store-string*)
  (clear-text *text-field*)
  (append-text target (print-list (parse-command))))

(defun parse-command ()
  (let ((commandlist (entnewlinify *store-string*)))
    (cond
      ((is-direction-p commandlist)
       (walk-direction (is-direction-p commandlist)))
       ((look-command-p commandlist)
	(look-command-p commandlist))
      (t nil))))


(defun split-string (string)
  "split string by space."
  (loop for i = 0 then (1+ j)
        as j = (position #\Space string :start i)
        collect (subseq string i j)
        while j))

(defun print-string (str target)
  (loop for i in str
        do (append-text target (format nil "~&~%~A" i))))

(defparameter *store-string* nil)

(defun entnewlinify (list)
  "remove Newline Character at end of string list."
   (mapcar #'(lambda (x) (string-right-trim '(#\Newline) x)) (first  list)))

















