(ql:quickload "ltk")
(load "~/MetalHead/actions.lisp")




(defpackage :gui
  (:use :cl :ltk :actions ) 
  (:export *text-field make-frame format-output
	   *store-string* entnewlinify))

(in-package :gui)


(defparameter *text-field*
  (make-instance 'text
		 :font "monospaced" :background "#242424"
		 :foreground "#ff9800"))

(defun make-frame ()
  (with-ltk ()
    (let* ((f (make-instance 'frame :padding "\"1 1 1 1\""
			     :relief :groove ))
	   (scroll (make-instance 'scrolled-text :master f))
	   (outtext (textbox scroll)))
      (pack f )
      (configure outtext :font "monospaced"
		 :background "#202020" :foreground "#ffffff" :wrap :word )
      (pack scroll :anchor :nw :expand t :fill :both :ipady 100)
      (pack *text-field* :side  :bottom :expand nil)
      (bind *text-field* "<KeyPress-Return>"
	    (lambda (event) (format-output outtext)))
      (configure f :borderwidth 1))))


(defun format-output (target)
  "Print inputstring with newlines and > .
   Store the inputted string as a list in *store-string*
   Clear *text-field*"
  (append-text target (format nil "~%~%> ~A" (text *text-field*)))
  (push (split-string (text *text-field*)) *store-string*)
  (clear-text *text-field*)
  (append-text target (format nil (print-list (parse-command))))
  (see target "end"))

(defun parse-command ()
  (let ((commandlist (entnewlinify *store-string*)))
    (cond
      ((is-direction-p commandlist)
       (walk-direction (is-direction-p commandlist)))
      ((is-action-p commandlist)
       (funcall (action-for-symbol (is-action-p commandlist))))
      ((look-command-p commandlist)
       (look-command-p commandlist))
      ((is-take-p (first commandlist))
       (take-command commandlist))
      (t (no-action)))))


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

















