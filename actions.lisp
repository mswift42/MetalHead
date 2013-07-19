(load "util.lisp")
(load "world.lisp")
(ql:quickload "fiveam")

(defpackage #:actions
  (:use :cl :fiveam :utilities :world))

(in-package #:actions)


(defun current-location ()
  (:location *player*))

(defmethod change-loc ((self player) loc)
  (setf (:location self) loc))

(defun take-object (item)
  "put item into inventory, delete item from location."
  (push item (:inventory *player*))
  (setf (:things (current-location))
	(delete item (:things (current-location)))))

(defun drop-object (item)
  "remove item from inventory, put item into location list."
  (setf (:inventory *player*) (delete item (:inventory *player*)))
  (push item (:things (current-location))))

(defun object-action-list (itemlist)
  "Return a list of all possible actions of all items
   for one location. (Helper Function for actions-for-location."
  (mapcar #'(lambda (x) (:action (symbol-value x))) itemlist))

(defun actions-for-location ()
  "Return alist for possible actions in the present location."
  (object-action-list (:things (current-location))))

(defun read-directions (room)
  "Return a list of all possible directions in a location."
  (append (:uexit room) (:cexit room) (:nexit room)))


(defun cexit-read-condition (direction)
  "return predicate necessary to use conditional exit."
  (find-symbol (symbol-name (third (equalassoc direction (:cexit (current-location)))))))


(defun unworldify (lst)
  "convert world::bla to bla"
  (mapcar #'(lambda (x) (find-symbol x))
	  (mapcar #'(lambda (x) (symbol-name x))
		  lst)))

(defun string-to-symbol (sym)
    "convert symbol name to string minus earmuffs."
    (let* ((name (symbol-name sym))
	   (len (length name)))
      (subseq name 1 (1- len))))

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
  (setf (:flags *laptop*) '(poweron))
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
	(change-loc *player* *hallway*)
	(describe-room *hallway*))))


(defmethod update-flag ((i item) value)
  (setf (:flags i) value))

(defun put-on-clothes ()
  (princ'(with the grace of a young gazelle you put on your clothes. Within
	  seconds your appearance changes from ugly as hell to well
	  below average handsome. Well done.))
  (setf (:flags *clothes*) :wearing)
  (take-object '*clothes*))

(defun take-laptop-f ()
  "You cannot take it. It's too heavy, the battery is not working and it's
   highly unlikely that it would survive any form of transport.")

(defun increment-fish-counter ()
  (let ((counter (second (equalassoc "taken" (:flags *fish*)))))
    (setf (second (equalassoc "taken" (:flags *fish*))) (1+ counter))))

(defun pick-up-trout-f ()
  "take the fish out off the pond."
  (let ((counter (second (equalassoc "taken" (:flags *fish*)))))
    (case counter
      (0 
       (progn
	 (print-list
	  '("With the grace of a young Mark Spitz you jump into "
	    "the pond in order to grab the fish. Of course, now "
	    "realising, that you are not in any way related to a Grizzly "
	    "Bear and noticing the puzzled looks of other people in "
	    "the park, you slowly and shyly make your way out of the "
	    "water. Bravo, You are an IDIOT!"))
	 (increment-fish-counter)))
      (1 
	 (progn
	   (princ (print-list
		   '("I'm sorry but are you seriously trying to pull that "
		     "stupid stunt again? Please, step back for a "
		     "moment and think : "
		     "How on earth do you expect to grab a fish with your "
		     "bare hands? No answer? Nothing? Nada? Please stop being "
		     "such a stupid muppet. Thanks. ")))
	   (increment-fish-counter)))
     (t
      (progn (print-list
	      '("Ok, Ok, I give up. Carefully you wade into the pond "
		"snatnch the fish and put it into your trouser pocket. "
		"Full of confidence you wade out of the water and "
		"enjoy the cheer of at least 20 people who stare at "
		"you admiringly.\n\nWell I made the last bit up, "
		"people stare at you, but certainly not admiringly "
		"mostly of course, because you now have twice gone "
		"into a fish pond, and you have a stinking fish in "
		"your jeans. (This is not a metaphor)"))
	  (take-object *fish*))))))


(defun describe-poster ()
  (:sdescription *poster*))

(defparameter verb-synonyms
  '((use use-v)
    (utilize use-v)
    (start start-v)
    (power start-v))
  "association list to lookup the fitting functions in an object to its verb")

(defun return-synonym (verb)
  "return the function synonym to the entered verb."
  (first (rest (assoc verb verb-synonyms)))) 

(defun uexits-next-location (direction uexit-lst)
  "Takes a direction and the list of uexits in a location.
   Returns either the next room if the desired direction is 
   a member of uexits-lst or nil."
  (cond
    ((null uexit-lst) nil)
    ((member direction (first uexit-lst) :test #'equal)
     (second (first uexit-lst)))
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
      ((uexits-next-location direction ue)
       (uexits-next-location direction ue))
      ((nexit-next-location direction ne)
       (nexit-next-location direction ne))        
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
    ((walk-direction exp (current-location)) (change-location  exp))
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

(defun change-location (direction)
  "When changing locations, set global-variable *location* to new location.
   Describe room either with first or later description."
  (change-loc *player* (walk-direction direction (current-location)))
  (describe-room  (current-location)))

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
	(print-list (flatten ( describe-list-of-items-in-location room)))
	
	(setf ( :flags room) :seen))
      (progn
	(game-print (print-list (:ldescription room)))
	(game-print (flatten
		     (describe-list-of-items-in-location-later room))))))

(defmethod items-in-room ((self loc))
  (:things self))

(defun print-list (lst)
  "convert list of symbols to string"
  (loop for i in lst
     do (princ (format nil "~A " i))))



(test test-u-exits 
  (is (equal '(("east" *bedroom*) ("west" *frontdoor*))
	 (:uexit *hallway*))))

(test test-items-in-room
  (is (equal '(*laptop* *clothes* *poster*)
	     (items-in-room *bedroom*))))

(test test-uexits-next-location 
      (is (equal '*bedroom* (uexits-next-location "east"
						  (:uexit *hallway*)))))

(test test-cexit-read-condition 
      (is (equal 'wear-clothes (cexit-read-condition "west"))))

(test test-describe-list-of-items-in-location                      
    (is (equal '(("on a table near the exit to the west is a laptop.")
		 ("strewn all over the floor are your clothes.")
		 ("On the wall you can see an old poster."))
	       (describe-list-of-items-in-location *bedroom*))))

(test test-return-synonym
  (is (equal 'start-v (return-synonym 'power)))
  (is (equal  'use-v (return-synonym 'use))))

(test test-read-direction
  (is (equal 'up (read-direction 'u)))
  (is (equal 'west (read-direction 'west)))
  (is (equal 'northeast (read-direction 'ne))))

(test test-string-to-symbol
  (is (equal "FISH" (string-to-symbol '*fish*)))
  (is (equal "LAPTOP" (string-to-symbol '*laptop*))))


(fiveam:run!)
