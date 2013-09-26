 
(in-package #:metalhead)


(defun main ()
  "Ltk-gui window for game, split in 2 parts, a outtext part, where 
   the descriptions of items and locations are printed and the reactions 
   to the players input, and the text-field part, a field for the player 
   to enter his commands."
  (with-ltk ()
     (let* ((f (make-instance 'frame :padding "\"1 1 1 1\""
			     :relief :groove ))
	   (scroll (make-instance 'scrolled-text :master f))
	    (label (make-instance 'label :master f :text "MetalHead"))
	   (outtext (textbox scroll))
	   (text-field (make-instance 'text :font "monospaced"
				      :background "#f2f1f0"
				      :foreground "#4c4c4c")))
       (if (= 10 *turns*)
	   (progn (sb-thread:destroy-thread *running-pub-quiz*)
		  (if (> *score* 5)
		      (append-text outtext (print-list (won-ticket-f)))
 		      (lost-ticket-f))))
      (pack f )
      (pack label)
      (configure outtext :font "monospaced"
		 :background "#f2f1f0" :foreground "#4c4c4c" :wrap :word )
      (pack scroll :anchor :nw :expand t :fill :both :ipady 100)
      (pack text-field :side  :bottom :expand nil)
      (bind text-field "<KeyPress-Return>"
	    (lambda (event) (format-output text-field outtext)))
       (configure f :borderwidth 1))))

(defun pub-quiz-window ()
  (with-ltk ()
    (let* ((f (make-instance 'frame :relief :groove))
	   (pub (make-instance 'label :master f :text "Pub Quiz"
			       ))
	   (outtext (make-instance 'text :font "monospaced"
				   :wrap :word))
	   (tf (make-instance 'text  :font "monospaced")))
      (pack f)
      (pack pub :side :left :ipadx 50)
      (pack outtext :ipady 100)
      (setf (text outtext) (pop *questions*))
      (bind tf "<KeyPress-Return>" (lambda (event)
				     (format-quiz tf outtext )))
      (if (= 10 *turns*)
	   (progn (sb-thread:destroy-thread *running-pub-quiz*)
		  ))
      (pack tf))))

(defvar *running-pub-quiz* nil)

(defparameter *questions*
  (question-list 10))

(defun format-output (source target)
  "Print inputstring with newlines and > .
   Store the inputted string as a list in *store-string*
   Clear source and scroll to end of text."
  (if (= *turns* 10)
      (pub-quiz-played-f))
  (append-text target (format nil "~%~%> ~A" (text source)))
  (push (split-string (text source)) *store-string*)
  (clear-text source)
  (append-text target (format nil (print-list (parse-command))))
  (see target "end"))

(defun format-quiz (source target)
  "store question and answer, call parse-quiz function."
  (let ((answer (string-right-trim '(#\Space #\Newline) (text source)))
	(question (string-right-trim '(#\Newline) (text target))))
    (setf *question* question)
    (setf *answer* answer)
    (clear-text target)
    (append-text target (format nil (parse-quiz)))
    (clear-text source)))

(defun parse-quiz ()
  "If answer is correct increase score and turns variables, 
   else only increase turns variable, finally print score."
  (if (correct-answer-p *question* *answer*)
      (progn
	(incf *score*)
	(incf *turns*))
      (progn
	(incf *turns*)))
  (if (> (length *questions*)
	 0)
      (pop *questions*)
      (progn
	(setf (:things *pub*)
	      (delete '*ticket-table* (:things *pub*)))
	(if (> *score* 5)
	    (print-list (won-ticket-f))
	    (print-list (lost-ticket-f)))
;	(bt:destroy-thread *running-pub-quiz*)
	)))

(defparameter *question* nil)
(defparameter *answer* nil)
(defparameter *turns* 0)
(defparameter *score* 0)


(defun parse-command ()
  "parse entered player input."
  (let ((commandlist (entnewlinify *store-string*)))
    (cond
      ((is-direction-p commandlist)
       (walk-direction (is-direction-p commandlist)))
      ((not-here commandlist)
       (list (concatenate 'string "you cannot see "
			  (last-element commandlist)
			  " here")))
      ((is-action-p commandlist)
       (funcall (action-for-symbol (is-action-p commandlist))))
      ((look-command-p commandlist)
       (look-command-p commandlist))
      ((is-take-p (first commandlist))
       (take-command commandlist))
      ((inventory-command-p commandlist)
       (inventory-command-p commandlist))
      (t (no-action)))))

(defparameter *pubquiz-turns* 0)
(defparameter *pubquiz-score* 0)



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

















