;;; gui.lisp uses ltk to make the gui windows for the main game and the pub-quiz. 
(in-package #:metalhead)


(defun main ()
  "Ltk-gui window for game, split in 2 parts, a outtext part, where 
   the descriptions of items and locations are printed and the reactions 
   to the players input, and the text-field part, a field for the player 
   to enter his commands."
  (with-ltk ()
     (let* ((f (make-instance 'frame :padding "\"1 1 1 1\""
			     :relief :groove))
	   (scroll (make-instance 'scrolled-text :master f))
	    (label (make-instance 'label :master f :text "MetalHead"))
	   (outtext (textbox scroll))
	   (text-field (make-instance 'text :font "monospaced"
				      :background "#f2f1f0"
				      :foreground "#4c4c4c")))
      (pack f )
      (pack label)
      (configure outtext :font "monospaced"
		 :background "#f2f1f0" :foreground "#4c4c4c" :wrap :word)
      (setf (text outtext) (format nil (print-list *intro*)))
      (append-text outtext (format nil (print-list (change-location *bedroom*))))
      (pack scroll :anchor :nw :expand t :fill :both :ipady 100)
      (pack text-field :side  :bottom :expand nil)
      (bind text-field "<KeyPress-Return>"
	    (lambda (event) (format-output text-field outtext)))
      (configure f :borderwidth 1 ))))

(defun pub-quiz-window ()
  "Ltk window for pubquiz. Two textfields, the top one for questions
   and the bottom one for player answers. "
  (with-ltk ()
    (let* ((f (make-instance 'frame :relief :groove :height 500 :width 600))
	   (pub (make-instance 'label :master f :text "Pub Quiz"))
	   (outtext (make-instance 'text :font "monospaced"
				         :wrap :word))
	   (tf (make-instance 'text  :font "monospaced")))
      (pack f)
      (pack pub :side :left :ipadx 50)
      (pack outtext :ipady 100)
      (setf (text outtext) (pop *questions*))
      (bind tf "<KeyPress-Return>" (lambda (event) (format-quiz tf outtext )))
      (pack tf))))

(defparameter *running-pub-quiz* nil)
(defparameter *turns* 0)
(defparameter *score* 0)
(defparameter *quiz-size* 10) ; number of questions to be asked for pub-quiz
(defparameter *quiz-win* 3)   ; limit of questions to get right.

(defparameter *questions*
  (question-list *quiz-size*))

(defun format-output (source target)
  "Print inputstring with newlines and > .
   Store the inputted string as a list in *store-string*
   Clear source and scroll to end of text."
  (append-text target (format nil "~%~%> ~A" (text source)))
  (push (split-string (text source)) *store-string*)
  (clear-text source)
  (append-text target (format nil (print-list (parse-command))))
  (see target "end"))

(defun parse-command ()
  "parse entered player input. If entered command is <help> print help screen,
   if command is a <go in direction> command call walk-direction function. If cmd
   refers to a object which is not in current location, return not here string. If
   cmd is a <examine object> and not a valid action commnad return the :sdescription
   of the <object>. If it is a valid action cmd, call the function in (:action <item>)
   if it is a <examine object> cmd call the look-command-p function. If it is a 
   is-take-p command call take-command function."
  (let ((commandlist (entnewlinify *store-string*)))
    (cond
      ((is-help-p (first commandlist))
       (print-help))
      ((is-direction-p commandlist)
       (walk-direction (is-direction-p commandlist)))
      ((not-here commandlist)
       (list (concatenate 'string "you cannot see "
			  (last-element commandlist)
			  " here")))
      ((and (eql :look-closer-v (is-action-p commandlist))
	    (not (action-for-symbol (is-action-p commandlist))))
       (:sdescription (find-synonym-in-location (last-element commandlist))))
      ((is-action-p commandlist)
       (funcall (action-for-symbol (is-action-p commandlist))))
      ((look-command-p commandlist)
       (look-command-p commandlist))
      ((is-take-p (first commandlist))
       (take-command commandlist))
      ((inventory-command-p commandlist)
       (inventory-command-p commandlist))
      (t (no-action)))))

(defun format-quiz (source target)
  "string-right-trim  question and answer, call parse-quiz function."
  (let ((answer (string-right-trim '(#\Space #\Newline) (text source)))
	(question (string-right-trim '(#\Newline) (text target))))
    (clear-text target)
    (append-text target (format nil (parse-quiz question answer)))
    (clear-text source)
    (if (= *turns* *quiz-size*)
	(append-text target
		     (format nil "Your score is ~D in ~D turns." *score* *turns*)))))

(defun parse-quiz (question answer)
  "If answer is correct increase score and turns variables, 
   else only increase turns variable, finally print score."
  (if (correct-answer-p question answer)
      (progn
	(incf *score*)
	(incf *turns*))
      (progn
	(incf *turns*)))
  (if (> (length *questions*) 0)
      (pop *questions*)
      (progn
	(setf (:things *pub*)
	      (delete '*ticket-table* (:things *pub*)))
	(if (>= *score* *quiz-win*)
	    (print-list (won-ticket-f))
	    (print-list (lost-ticket-f))))))


(defparameter *pubquiz-turns* 0)
(defparameter *pubquiz-score* 0)


(defun split-string (string)
  "split string by space."
  (loop for i = 0 then (1+ j)
        as j = (position #\Space string :start i)
        collect (subseq string i j)
        while j))


(defun entnewlinify (list)
  "remove Newline Character at end of string list."
   (mapcar #'(lambda (x) (string-right-trim '(#\Newline) x)) (first  list)))    

(load "world.lisp")
(main)











