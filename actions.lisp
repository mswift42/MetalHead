(load "~/MetalHead/util.lisp")
(load "~/MetalHead/world.lisp")

(ql:quickload "fiveam")

(defpackage #:actions
  (:use :cl :fiveam :utilities :world)
  (:export change-loc exit-lst take-object drop-object current-location
	   walk-direction object-action-list actions-for-location
	   *directions-synonyms* *directions* read-direction move-p
	   use-laptop-f power-on-laptop-f put-on-clothes wear-clothes
	   update-flag increment-fish-counter pick-up-trout-f take-laptop-f
	   describe-poster read-inscription-f verb-synonyms return-synonym
	   change-location describe-list-of-items-in-location
	   describe-list-of-items-in-location-later describe-room
	   items-in-room print-list is-direction-p is-look-p look-command-p
	   convert-symbol is-take-p take-command build-substring
	   is-action-p action-for-symbol))

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

(defmethod find-synonym ((self item) string)
  "if synonym is in synonym list of item, return item
   (find-synonym *laptop* 'laptop') -> #<item{af17569}"
  (if (equalmember string (:synonym self))
      self
      nil))

(defun find-synonym-in-location (string)
  "map find-synonym function to all items in a location"
  (some #'(lambda (x) (find-synonym (symbol-value x) string))
	(:things (current-location))))

(defun take-object (item)
  "put item into inventory, delete item from location."
  (push item (:inventory *player*))
  (setf (:things (current-location))
	(remove-if #'(lambda (x) (equal item (symbol-value x))) (:things (current-location)))))

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
  (flatten (object-action-list (:things (current-location)))))


(defun symbol-to-string (sym)
  "convert symbol name to string minus earmuffs."
  (let* ((name (symbol-name sym))
	 (len (length name)))
    (subseq name 1 (1- len))))

(defparameter  *directions-synonyms*
  '(("e"  "east") ("w"  "west") ("s" "south") ("n"  "north") ("d"  "down")
    ("u"  "up") ("se"  "southeast") ("sw"  "southwest")
    ("ne"  "northeast") ("nw"  "northwest"))
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

(defun move-p (string)
  "return if string is a movement command."
   (equalmember string '("go" "move" "walk")))

(defun use-laptop-f ()
  (if (equal 'poweroff (first (:flags *laptop*)))
      '("Your laptop is turned off")
      '("you could browse your favorite websites "
       "all day, you good old procrastinator, "
       "however I'd propose you simply check your Email.")))

(defun power-on-laptop-f ()
  (setf (:flags *laptop*) '(poweron))
  "You press the power button. You hear some funny noises, and it actually 
   starts booting. One cup of Tea later, and you start at the login 
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
  (print-list'("with the grace of a young gazelle "
	       "you put on your clothes. Within "
	       "seconds your appearance changes from "
	       "ugly as hell to well below average handsome. "
	       "Well done."))
   (setf (:flags *clothes*) '(:wearing))
   (setf (:cexit *bedroom*) '(("west" *hallway* wear-clothes t)))
   (take-object '*clothes*))

(defun take-laptop-f ()
  (print-list'("You cannot take it. It's too heavy, "
	       "the battery is not working and it's "
	       "highly unlikely that it would survive "
	       "any form of transport.")))

(defun take-clothes-f ()
  "Text to return when taking clothes"
  '("You take your clothes. Rather awkwardly you are "
    "now standing there like a bloody idiot holding "
    "your clothes in your hands."))

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
  '(("use" :use-v)
    ("utilize" :use-v)
    ("start" :start-v)
    ("power" :start-v)
    ("put on" :wear-v)
    ("wear" :wear-v)
    ("dress with" :wear-v))
  "association list to lookup the fitting functions in an object to its verb")

(defun return-synonym (verb)
  "return the function synonym to the entered verb."
  (second (equalassoc verb verb-synonyms))) 

(defun convert-symbol (s)
  "convert in package world stored symbol to its in package 
   action function value '(convert-symbol :use-laptop-f) -> use-laptop-f"
  (find-symbol (symbol-name s)))

(defun action-for-verb (verb)
  "lookup entered verb in verb-synonyms. if entry found, lookup that 
   entry in actions-for location alist and convert symbol into function."
  (convert-symbol (second (member (return-synonym verb)
				  (actions-for-location)))))

(defun action-for-symbol (s)
  "return fitting function to entered symbol. '(:wear-v)-> put-on-clothes"
  (convert-symbol (second (member s (actions-for-location)))))


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
      ((eq 'ne exittype) (third exitlist))
      (t (funcall (find-symbol
		   (symbol-name (fourth exitlist))))))))



(defun no-exit ()
  (random-string '("you cannot go that way" "there is no exit that way")))


(defparameter *allowed-commands* '(use-laptop-f))


(defun change-location (room)
  "When changing locations, set global-variable *location* to new location.
   Describe room either with first or later description."
  (change-loc *player* room)
  ;(:fdescription (current-location))
  (describe-room  (current-location)))

(defun describe-list-of-items-in-location (room)
  "Return list of descriptions of all items in a room."
  (flatten (mapcar #'(lambda (x) (:fdescription (symbol-value x)))
		   (:things room)))) 

 (defun describe-list-of-items-in-location-later (room)
  "Return the ldescription of all itemns in a room."
  (flatten (mapcar #'(lambda (x) (:ldescription (symbol-value x)))
		   (:things room))))

(defun describe-room ( room)
  "if visiting loc for first time return list of :fdesc room
   appended by description of all items in current loc.
   If loc has been visited, return :ldescription of loc."
  (if (eq (first (:flags room)) :notseen)
      (progn
	(append  (:fdescription room)
		 (describe-list-of-items-in-location room))
	
	(setf (:flags room) '(:seen)))
      (append (:ldescription room) (describe-list-of-items-in-location room))))

(defmethod items-in-room ((self loc))
  (:things self))

(defun print-list (list)
  "concatenate list of strings to one single string."
  (apply #'concatenate 'string list))

(defun is-direction-p (input)
  "return if input is a change-location command
   '('go' 'west') -> 'west' '('west') -> 'west' '('eat' 'salad') nil"
  (let ((len (length input)))
    (cond
      ((and (= 1 len)
	    (read-direction (first input))) (first input))
      ((and (= 2 len)
	    (move-p (first input))
	    (read-direction (second input))) (second input))
      (t nil))))

(defun look-command-p (list)
  "return if input is a 'look' command."
  (let ((len (length list)))
    (cond
      ((and (= 1 len)
	    (is-look-p (first list)))
       (describe-room (current-location)))
      ((and (> len 1)
	    (is-look-p (first list))
	    (find-synonym-in-location (last-element list)))
       (:ldescription
	(find-synonym-in-location (last-element list))))
      (t nil))))

(defun is-look-p (exp)
   "return if command is member of synonyms for 'look'"
   (equalmember exp '("look" "examine" "study" "view" "scan" "parse"
		      "explore" "l")))

(defun is-take-p (exp)
  "return if command if member of synonyms for 'take'"
  (equalmember exp '("t" "take" "grab" "snatch" "get")))

(defun take-command (list)
  (let ((obj (find-synonym-in-location (last-element list))))
    (cond
      ((not obj) (no-object))
      ((assoc :pick-up-v (:action obj))
       (funcall (convert-symbol (second (assoc :pick-up-v (:action obj))))))
      ((not (member :fixed (:flags obj)))
       (take-object (last-element list)))
      (t '("You cannot take that!")))))

(defun is-action-p (list)
  "return action flag from verb synonyms if entered command is 
   a action command"
  (cond
    ((equalassoc (first list) verb-synonyms)
     (second (equalassoc (first list) verb-synonyms)))
    ((equalassoc (build-substring list) verb-synonyms)
     (second (equalassoc (build-substring list) verb-synonyms)))
    (t nil)))

(defun build-substring (list)
  "concatenate first and second word in list to single string"
  (concatenate 'string (first list) " " (second list)))



(defun no-object ()
  '("There is no such thing here"))








;;; Tests ;;;
(test test-u-exits 
  (is (equal '(("east" *bedroom*) ("west" *housefront*))
	     (:uexit *hallway*))))

(test test-items-in-room
  (is (equal '(*laptop* *clothes* *poster*)
	     (items-in-room *bedroom*))))

(test test-is-direction-p
  (is-true (is-direction-p '("go" "north")))
  (is-true (is-direction-p '("west")))
  (is-true (is-direction-p '("move" "se")))
  (is-false (is-direction-p '("eat" "salad"))))

(test test-is-look-p
  (is-true (is-look-p "EXAMINE"))
  (is-true (is-look-p "StuDy")))


(test test-describe-list-of-items-in-location                      
  (is (equal '("on a table near the exit to the west is a laptop."
	       "strewn all over the floor are your clothes."
	       "On the wall you can see an old poster.")
	     (describe-list-of-items-in-location *bedroom*))))

(test test-equal-lst
  (is (equal '(CE "west" *HALLWAY* WORLD::WEAR-CLOTHES)
	     (exit-lst *bedroom* "west")))
  (is (equal '(UE "east" *bedroom*)
	     (exit-lst *hallway* "east"))))

(test test-return-synonym
  (is (equal :start-v (return-synonym 'power)))
  (is (equal  :use-v (return-synonym 'use))))

(test test-read-direction
  (is (equal "up" (read-direction "u")))
  (is (equal "west" (read-direction "west")))
  (is (equal "northeast" (read-direction "ne"))))

(test test-string-to-symbol
  (is (equal "FISH" (symbol-to-string '*fish*)))
  (is (equal "LAPTOP" (symbol-to-string '*laptop*))))

(test test-print-list
  (is-true (typep (print-list '("hello" " you" " yes" " you")) 'string)))

(test test-find-synonym
  (is-true (find-synonym *laptop* "notebook"))
  (is-true (find-synonym *laptop* "LAPTOP")))

(test test-find-synonym-in-location
  (is (equal '("a poster") (:name (find-synonym-in-location "poster"))))
  (is (equal '("laptop") (:name (find-synonym-in-location "laptop")))))

(test test-look-command
  (is (equal '("your old sturdy laptop. Not the latest and shiniest "
 "but money is very expensive so you still " "make do with it.")
	     (look-command-p '("examine" "laptop"))))
  (is (equal '("It is a very old nearly completely faded poster."
 " You can only make out a painted scene of rows "
 "of white crosses in a field.")
	     (look-command-p '("study" "poster")))))

(test test-is-action-p
  (is (eq :wear-v (is-action-p '("put" "on" "clothes"))))
  (is (eq :wear-v (is-action-p '("wear" "clothes")))))


(fiveam:run!)
