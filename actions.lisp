(load "util.lisp")
(load "world.lisp")
(ql:quickload "fiveam")

(defpackage #:actions
  (:use :cl :fiveam :utilities :world))

(in-package #:actions)


(defun current-location ()
  (:location *player*))

(defmethod change-loc ((self player) newlocation)
  "update *player* instance with new location."
  (setf (:location self) newlocation))

(defmethod exit-lst ((self loc) direction)
  "return list with type of exit + list of next loc 
   and text/condition for nexit and cexit lists."
  (let ((ce (:cexit self))
	(ue (:uexit self))
	(ne (:nexit self)))
    (cond
      ((equalassoc direction ce)
       (flatten (cons 'ce (equalassoc direction ce))))
      ((equalassoc direction ue)
       (flatten (cons 'ue (equalassoc direction ue))))
      ((equalassoc direction ne)
       (flatten (cons 'ne (equalassoc direction ne))))
      (t nil))))

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


(defun symbol-to-string (sym)
  "convert symbol name to string minus earmuffs."
  (let* ((name (symbol-name sym))
	 (len (length name)))
    (subseq name 1 (1- len))))

(defparameter  *directions-synonyms*
  '(("e"  "east") ("w"  "west") ("s" "south") ("n"  "north") ("d"  "down")
    ("u"  "up") ("se"  "southeast") ("sw"  "southwest") ("ne"  "northeast") ("nw"  "northwest"))
  "alist for abbreviations of directions.")

(defparameter *directions*
  '("east" "west" "south" "north" "down" "up"
    "southeast" "southwest" "northeast" "northwest"))

(defun read-direction (input)
  "look up entered direction in directions-synonyms and directions.
   If synonym return full name. If full name entered return it."
  (cond
    ((equalmember input *directions*) input)
    ((equalassoc input *directions-synonyms*)
     (second (equalassoc input *directions-synonyms*)))
    (t nil)))

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
  (if (eq (first  (:flags *clothes*)) :notwearing)
      '("you are not wearing any clothes. I am terribly sorry but you "
	"should not inflict your gross naked body on other people. "
	"There are plenty beautiful sights in this "
	"world. You are not one of them. "
	"When God made you he was either drunk or bored. "
	"Maybe he was just spiteful "
	"but for Fuck Sake please put on some clothes.")
      (change-location *hallway*)))


(defmethod update-flag ((i item) value)
  (setf (:flags i) value))

(defun put-on-clothes ()
  (princ'(with the grace of a young gazelle you put on your clothes. Within
	  seconds your appearance changes from ugly as hell to well
	  below average handsome. Well done.))
  (setf (:flags *clothes*) '(:wearing))
  (setf (:cexit *bedroom*) '(("west" *hallway* wear-clothes t)))
  (take-object '*clothes*))

(defun take-laptop-f ()
  "You cannot take it. It's too heavy, the battery is not working and it's
   highly unlikely that it would survive any form of transport.")

(defun increment-fish-counter ()
  "Increase :taken counter of item *fish*"
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
		 "snatch the fish and put it into your trouser pocket. "
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

(defun read-inscription-f ()
  (print-list '("As you come closer to read the inscription in the "
		"bench, you notice two things: A: the bench smells "
		"of vomit, and B: the text written in the wood reads 
                \"For a heavy time, go into the second toilet stall "
		"in the golden goose\"\nWell, we can't pass up such "
		"an opportunity now, can we?")))

(defparameter verb-synonyms
  '((use use-v)
    (utilize use-v)
    (start start-v)
    (power start-v))
  "association list to lookup the fitting functions in an object to its verb")

(defun return-synonym (verb)
  "return the function synonym to the entered verb."
  (first (rest (assoc verb verb-synonyms)))) 


(defun walk-direction (direction )
  "set *player* location to a viable entered direction. 
   if cexit call cexit-function, if nexit print nexit text and 
   if uexit call change-location function with corresponding location 
   in (exit-lst)"
  (let* ((exitlist (exit-lst (current-location) direction))
	 (exittype (first exitlist)))
    (cond
      ((null exitlist) (no-exit))
      ((eq 'ue exittype) (change-location (symbol-value (third exitlist))))
      ((eq 'ne exittype) (print-list (third exitlist)))
      (t (funcall (find-symbol
		   (symbol-name (fourth exitlist))))))))


(defun no-exit ()
  (print-list '("you cannot go that way" "there is no exit that way")))


(defparameter *allowed-commands* '(use-laptop-f))


(defun change-location (room)
  "When changing locations, set global-variable *location* to new location.
   Describe room either with first or later description."
  (change-loc *player* room)
  (print-list (:fdescription (current-location)))
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
  (if (eq (first (:flags room)) :notseen)
      (progn
	(print-list (:fdescription room))
	(print-list (flatten ( describe-list-of-items-in-location room)))
	
	(setf ( :flags room) '(:seen)))
      (progn
	(print-list (print-list (:ldescription room)))
	(print-list (flatten
		     (describe-list-of-items-in-location-later room))))))

(defmethod items-in-room ((self loc))
  (:things self))

(defun print-list (lst)
  "convert list of symbols to string"
  (reduce #'(lambda (x y) (concatenate 'string x y)) lst))

(test test-u-exits 
  (is (equal '(("east" *bedroom*) ("west" *housefront*))
	     (:uexit *hallway*))))

(test test-items-in-room
  (is (equal '(*laptop* *clothes* *poster*)
	     (items-in-room *bedroom*))))


(test test-describe-list-of-items-in-location                      
  (is (equal '(("on a table near the exit to the west is a laptop.")
	       ("strewn all over the floor are your clothes.")
	       ("On the wall you can see an old poster."))
	     (describe-list-of-items-in-location *bedroom*))))

(test test-equal-lst
  (is (equal '(CE "west" *HALLWAY* WORLD::WEAR-CLOTHES)
	     (exit-lst *bedroom* "west")))
  (is (equal '(UE "east" *bedroom*)
	     (exit-lst *hallway* "east"))))

(test test-return-synonym
  (is (equal 'start-v (return-synonym 'power)))
  (is (equal  'use-v (return-synonym 'use))))

(test test-read-direction
  (is (equal "up" (read-direction "u")))
  (is (equal "west" (read-direction "west")))
  (is (equal "northeast" (read-direction "ne"))))

(test test-string-to-symbol
  (is (equal "FISH" (symbol-to-string '*fish*)))
  (is (equal "LAPTOP" (symbol-to-string '*laptop*))))

(test test-print-list
  (is-true (typep (print-list '("hello" " you" " yes" " you")) 'string)))


(fiveam:run!)
