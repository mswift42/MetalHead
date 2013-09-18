
(in-package #:metalhead)


(defun main ()
  (with-ltk ()
     (let* ((f (make-instance 'frame :padding "\"1 1 1 1\""
			     :relief :groove ))
	   (scroll (make-instance 'scrolled-text :master f))
	    (label (make-instance 'label :master f :text "MetalHead"))
	   (outtext (textbox scroll))
	   (text-field (make-instance 'text :font "monospaced"
				      :background "#f2f1f0"
				      :foreground "#4c4c4c")))
      (pack f )
      (pack label)
      (configure outtext :font "monospaced"
		 :background "#f2f1f0" :foreground "#4c4c4c" :wrap :word )
      (pack scroll :anchor :nw :expand t :fill :both :ipady 100)
      (pack text-field :side  :bottom :expand nil)
      (bind text-field "<KeyPress-Return>"
	    (lambda (event) (format-output text-field outtext)))
      (configure f :borderwidth 1))))


(defun format-output (source target)
  "Print inputstring with newlines and > .
   Store the inputted string as a list in *store-string*
   Clear source"
  (append-text target (format nil "~%~%> ~A" (text source)))
  (push (split-string (text source)) *store-string*)
  (clear-text source)
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
      ((inventory-command-p commandlist)
       (inventory-command-p commandlist))
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

















