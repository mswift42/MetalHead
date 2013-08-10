(load "~/MetalHead/util.lisp")
(load "~/MetalHead/world.lisp")


(defpackage #:actions
  (:use :cl  :utilities :world)
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
	   is-action-p action-for-symbol find-synonym-in-location
	   find-synonym no-action take-clothes-f))

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
	(remove-if #'(lambda (x) (equal item (symbol-value x)))
		   (:things (current-location)))))

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

;; I'm using multiple-value-prog1 here because it evalutates it's
;; first argument and returns it, but silently evaluates the proceeding
;; forms. Otherwise gui.lisp 's format-output function does not know
;; what to print. 
(defun put-on-clothes ()
  (multiple-value-prog1 '("with the grace of a young gazelle "
			"you put on your clothes. Within "
			"seconds your appearance changes from "
			"ugly as hell to well below average handsome. "
			  "Well done.")
    (setf (:flags *clothes*) '(:wearing))
    (setf (:cexit *bedroom*) '(("west" *hallway* wear-clothes t)))
    (take-object '*clothes*)))

(defun take-laptop-f ()
  '("You cannot take it. It's too heavy, "
    "the battery is not working and it's "
    "highly unlikely that it would survive "
    "any form of transport."))

(defun take-clothes-f ()
  "Text to return when taking clothes"
  '("You take your clothes. Rather awkwardly you are "
    "now standing there like a bloody idiot holding "
    "your clothes in your hands.~%"
    "You should maybe think about putting them on."))

(defun increment-fish-counter ()
  "Increase :taken counter of item *fish*"
  (let ((counter (second (equalassoc "taken" (:flags *fish*)))))
    (setf (second (equalassoc "taken" (:flags *fish*))) (1+ counter))))

(defun pick-up-trout-f ()
  "take the fish out off the pond."
  (let ((counter (second (equalassoc "taken" (:flags *fish*)))))
    (case counter
      (0 
       (multiple-value-prog1
	   '("With the grace of a young Mark Spitz you jump into "
	     "the pond in order to grab the fish. Of course, now "
	     "realising, that you are not in any way related to a Grizzly "
	     "Bear and noticing the puzzled looks of other people in "
	     "the park, you slowly and shyly make your way out of the "
	     "water. Bravo, You are an IDIOT!")
	 (increment-fish-counter)))
      (1 
       (multiple-value-prog1
	   '("I'm sorry but are you seriously trying to pull that "
	     "stupid stunt again? Please, step back for a "
	     "moment and think : "
	     "How on earth do you expect to grab a fish with your "
	     "bare hands? No answer? Nothing? Nada? Please stop being "
	     "such a stupid muppet. Thanks. ")
	 (increment-fish-counter)))
       (t
       (multiple-value-prog1
	'("Ok, Ok, I give up. Carefully you wade into the pond "
	  "snatch the fish and put it into your trouser pocket. "
	  "Full of confidence you wade out of the water and "
	  "enjoy the cheer of at least 20 people who stare at "
	  "you admiringly.~%~%Well I made the last bit up, "
	  "people stare at you, but certainly not admiringly "
	  "mostly of course, because you now have twice gone "
	  "into a fish pond, and you have a stinking fish in "
	  "your jeans. (This is not a metaphor)")
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
      ((eq 'ne exittype) (cddr exitlist))
      (t (funcall (find-symbol
		   (symbol-name (fourth exitlist))))))))



(defun no-exit ()
  "list to return when entered direction is not valid."
  (list (random-string '
	 ("you cannot go that way." "there is no exit that way."))))

(defun no-action ()
  "list to return when command is not understood"
  (list 
   (random-string
    '("This doesn't mean anything to me."
      "Sorry, but I do not understand you."
      "I'm not sure what you are trying to say, try harder"
      "One of us does not understand the English language very well."
      "I cannot compute that."))))


(defparameter *allowed-commands* '(use-laptop-f))


(defun change-location (room)
  "When changing locations, set global-variable *location* to new location.
   Describe room either with first or later description."
  (change-loc *player* room)
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
      (multiple-value-prog1
	(append  (:fdescription room)
		 (describe-list-of-items-in-location room)))
      (multiple-value-prog1
	  (append (:ldescription room)
		  (describe-list-of-items-in-location room))
	  (setf (:flags room) :seen))))

;; (defmethod items-in-room ((self loc))
;;   (:things self))

(defun print-list (list)
  "concatenate list of strings to one single string."
  (apply #'concatenate 'string list))

(defun is-direction-p (input)
  "return if input is a change-location command
   '('go' 'west') -> 'west' '('west') -> 'west' '('eat' 'salad') nil"
  (let ((len (length input)))
    (cond
      ((and (= 1 len)
	    (read-direction (first input))) (read-direction (first input)))
      ((and (= 2 len)
	    (move-p (first input))
	    (read-direction (second input))) (read-direction (second input)))
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
      ((member :pick-up-v (flatten (:action obj)))
       (funcall
	(convert-symbol (second (member :pick-up-v
					 (flatten (:action obj)))))))
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



