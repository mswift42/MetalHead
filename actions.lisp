(load "util.lisp")
(load "world.lisp")
(ql:quickload "clunit")

(defpackage #:actions
  (:use :cl :clunit :utilities :world))

(in-package #:actions)

(defparameter *location*
  '*bedroom*)

(defun current-location ()
  (symbol-value (:location *player*)))


(defun take-object (item)
  (setf (:inventory *player*) (cons item (:inventory *player*)))
  (setf (:things (current-location)) (delete item (:things (current-location)))))

(defun drop-object (item)
  (setf (:inventory *player*) (delete item (:inventory *player*)))
  (setf (:things (current-location)) (cons item (:things (current-location)))))

(defun object-action-list (itemlist)
  "Return a list of all possible actions of all items
   for one location. (Helper Function for actions-for-location."
  (cond
    ((null itemlist) nil)
    (t (append (:action (symbol-value (first itemlist)))
	       (object-action-list (rest itemlist))))))

(defun actions-for-location ()
  "Return alist for possible actions in the present location."
  (object-action-list (:things (symbol-value *location*))))


(defun read-directions (room)
  "Return a list of all possible directions in a location."
  (append (:uexit room) (:cexit room) (:nexit room)))

(defun cexit-read-condition (direction)
  "return predicate necessary to use conditional exit."
  (find-symbol (symbol-name (third (assoc direction (:cexit (current-location)))))))

;; (defun location ()
;;     (:location *player*))

(defparameter *inventory* '())

(defparameter  *directions-synonyms*
    '((e  east) (w  west) (s  south) (n  north) (d  down)
    (u  up) (se  southeast) (sw  southwest) (ne  northeast) (nw  northwest))
  "alist for abbreviations of directions.")

(defparameter *directions*
  '(east west south north down up southeast southwest northeast northwest))

(defun read-direction (input)
  "look up entered direction in directions-synonyms and directions.
   If synonym return full name. If full name entered return it."
  (cond
    ((member input *directions*) input)
    ((assoc input *directions-synonyms*) (second (assoc input *directions-synonyms*)))
    (t nil)))

(defun u-exits (room)
  (:uexit room))


(defun use-laptop-f ()
  (if (equal 'poweroff (first (:flags *laptop*)))
      "Your laptop is turned off"
      "you could browse your favorite websites all day, you good old 
       procrastinator, however I'd propose you simply check your Email."))
(defun power-on-laptop-f ()
  (setf (:flags *laptop*
		    ) '(poweron))
  "You press the power button. You hear some funny noises, and it actually 
   starts booting. One Cup of Tee later, and you start at the login 
   screen. I hope you haven't forgotten the password.")

(defun wear-clothes ()
  "if not wearing clothes, print out text . Else change location to hallway."
  (if (eq (symbol-value (:flags *clothes*)) :notwearing)
      '(you are not wearing any clothes. I am terribly sorry but you should
	not inflict your gross naked body on other people. There
	are plenty beautiful sights in this
	world. You are not one of them.
	When God made you he was either drunk or bored.
	Maybe he was just spiteful
	but for Fuck Sake please put on some clothes.)
      (progn
	(setf *location* *hallway*)
	(describe-room *hallway*))))

(defun functionstring ()
  (symbol-name (third (assoc 'world::west (:cexit (current-location))))))

(defun put-on-clothes ()
  (princ'(with the grace of a young gazelle you put on your clothes. Within
	  seconds your appearance changes from ugly as hell to well
	  below average handsome. Well done.))
  (setf (:flags *clothes*) :wearing))

(defun take-laptop-f ()
  "You cannot take it. It's too heavy, the battery is not working and it's
   highly unlikely that it would survive any form of transport.")


(defun describe-poster ()
  (:sdescription *poster*))

(defparameter verb-synonyms
  '((use use-v)
    (utilize use-v)
    (start start-v)
    (power start-v))
  "association list to lookup the fitting functions in an object to its verb")

(defun return-synonym (verb)
  (first ( rest (assoc verb verb-synonyms)))) 



(defun uexits-next-location (direction uexit-lst)
  "Takes a direction and the list of uexits in a location.
   Returns either the next room if the desired direction is 
   a member of uexits-lst or nil."
  (cond
    ((null uexit-lst) nil)
    ((member direction (first uexit-lst)) (second (first uexit-lst)))
    (t (uexits-next-location direction (rest uexit-lst)))))

(defun nexit-next-location (direction nexit-lst)
  "return possible nexit of direction in a location."
  (cond
    ((null nexit-lst) nil)
    ((member direction (first nexit-lst)) (second (first nexit-lst)))
    (t (nexit-next-location direction (rest nexit-lst)))))

(defun walk-direction (direction room)
  "Return next location of a entered direction in a location."
  (let ((ue (:uexit room))
	(ne (:nexit room)))
    (cond
      ((and ( cexit-read-condition direction))
       (funcall ( cexit-read-condition direction)))
      ((uexits-next-location direction ue) (uexits-next-location direction ue))
      ((nexit-next-location direction ne) (nexit-next-location direction ne))        
      (t nil))))


(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))


(defun game-read ()
  (let ((cmd (read-from-string
	      (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
	     (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defparameter *allowed-commands* '( use-laptop-f))

(defun game-eval (sexp)
  (if (member sexp *allowed-commands*)
      (funcall sexp)
      '(I do not know this command.)))


(defun game-reader (exp)
  "Evaluate player input"
  (cond
    ((walk-direction exp (symbol-value *location*)) (change-location  exp))
    ((assoc exp (actions-for-location))
     (funcall (second (assoc exp (actions-for-location)))))
    (t nil)))

(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
	  (rest (cdr lst)))
      (cond ((eq item #\space) (cons item (tweak-text rest caps lit)))
	    ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
	    ((eq item #\") (tweak-text rest caps (not lit)))
	    (lit (cons item (tweak-text rest nil lit)))
	    ((or caps lit) (cons (char-upcase item) ( tweak-text rest nil lit)))
	    (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "() "
						  (prin1-to-string lst))
				     'list)
			     t
			     nil)
		 'string))
  (fresh-line))

(defun change-location ( direction)
  "When changing locations, set global-variable *location* to new location.
   Describe room either with first or later description."
  (setf *location* (walk-direction direction (symbol-value *location*)))
  (describe-room  (symbol-value *location*)))

(defun describe-list-of-items-in-location (room)
  "Return list of descriptions of all items in a room."
  (mapcar #'(lambda (x) (:fdescription (symbol-value x)))
	  (:things room))) 

(defun describe-list-of-items-in-location-later (room)
  "Return the ldescription of all itemns in a room."
  (mapcar #'(lambda (x) (:ldescription (symbol-value x)))
	  (:things room)))

(defun describe-room ( room)
  "Use lol's game-print function to print first the description of the
   room you are in, then describe all items in the location."
  (if (eq (symbol-value (:flags room)) :notseen)
      (progn
	(game-print (:fdescription room))
	(game-print (flatten ( describe-list-of-items-in-location room)))
	
	(setf ( :flags room) :seen))
      (progn
	(game-print (print-list (:ldescription room)))
	(game-print (flatten
		     (describe-list-of-items-in-location-later room))))))

(defun items-in-room (room)
  "Return all items in a location."
  (:things room))

(defun print-list (lst)
  "convert list of symbols to string"
  (loop for i in lst
     do (princ (format nil "~A " i))))



(clunit:defsuite Room-suite ())
(clunit:defsuite Parse-suite ())


(clunit:deftest test-u-exits (Room-suite)
  (clunit:assert-equal '((east *bedroom*) (west *frontdoor*))
		       (u-exits *hallway*)))

(deftest test-items-in-room (Room-suite)
  (clunit:assert-equal '(*laptop* *clothes* *poster*)
		       (items-in-room *bedroom*)))

(deftest test-uexits-next-location (Room-suite)
  (clunit:assert-equal '*bedroom* (uexits-next-location 'east
							(uexit *hallway*))))

(deftest test-cexit-read-condition (Room-suite)
  (clunit:assert-equal 'wear-clothes (cexit-read-condition 'world::west)))

(deftest test-describe-list-of-items-in-location (Room-suite)                     
  (clunit:assert-equal '((ON A TABLE NEAR THE EXIT TO THE WEST IS A LAPTOP.)
			 (STREWN ALL OVER THE FLOOR ARE YOUR CLOTHES.)
			 (ON THE WALL YOU CAN SEE AN OLD POSTER.))
      (describe-list-of-items-in-location *bedroom*)))



(clunit:deftest test-return-synonym (Parse-suite)
  (clunit:assert-equal 'start-v (return-synonym 'power))
  (clunit:assert-equal 'use-v (return-synonym 'use)))

(clunit:deftest test-read-direction (Parse-suite)
  (clunit:assert-equal 'up (read-direction 'u))
  (clunit:assert-equal 'west (read-direction 'west))
  (clunit:assert-equal 'northeast (read-direction 'ne)))

(clunit:run-suite 'Room-suite)
(clunit:run-suite 'Parse-suite)
;; (defparameter *location*
;;   '*bedroom*
;;   "location of the player character in the game world.")
;; (defparameter *player*
;;   (make-instance 'player
;; 		 :location *bedroom* :inventory '()))
