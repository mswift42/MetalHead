
(in-package #:metalhead)


(defun current-location ()
  (:location *player*))

(defgeneric change-loc (player loc))
 
(defmethod change-loc ((self player) newlocation)
  "update *player* instance with new location."
  (setf (:location self) newlocation))


;; to prevent a player from going back to a 'finished' 
;; for example, *finnegans*, block-exit will change an unconditional
;; exit to a non-exit.
(defun block-exit (location dir-list nexit-text)
  "delete valid exit from :uexit list and add exit as non-exit"
  (setf (:uexit location) (remove-if #'(lambda (x) (equal x dir-list))
				     (:uexit location)))
  (push (list (first dir-list) nexit-text)(:nexit location) ))

(defgeneric exit-lst (loc direction))

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

(defgeneric find-synonym (item string)
  (:documentation "return synonym for item if found."))

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

(defgeneric update-flag (instance old new)
  (:documentation "update flag list of location or item."))

(defmethod update-flag ((self loc) old-value new-value)
  (setf (:flags self)
	(cons new-value (remove-if #'(lambda (x) (eq old-value x))
				   (:flags self)))))

(defmethod update-flag ((self item) old-value new-value)
  (setf (:flags self)
	(cons new-value (remove-if #'(lambda (x) (eq old-value x))
				   (:flags self)))))




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
    (update-flag *clothes* :notwearing :wearing)
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

(defun enter-password-f ()
  "entering password into laptop"
  '(" "))

(defun press-doorbell-f ()
  (multiple-value-prog1
      '("You press the doorbell. With a startling amount "
	"of noise, you hear the intro to Death's \"Leprosy\"~%"
	"Maybe 20 seconds later, the door opens and your friend "
	"beckons you inside.")
    (change-location *friends-hallway*)
    (setf (:cexit *friends-house*) '(("south" *friends-house* bell-rung t)))))

(defun increment-fish-counter ()
  "Increase :taken counter of item *fish*"
  (incf (second (equalassoc "taken" (:flags *fish*))) ))

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
  '("As you come closer to read the inscription in the "
    "bench, you notice two things: A: the bench smells "
    "of vomit, and B: the text written in the wood reads 
                \"For a heavy time, go into the second toilet stall "
    "in the golden goose\"\nWell, we can't pass up such "
    "an opportunity now, can we?"))

(defun bought-beer-v ()
  (if (member *beer* (:inventory *player*))
      (multiple-value-prog1
	  (describe-room *living-room*)
	  (change-location *living-room*))
      '("You need to buy beer first. ")))

(defun talk-to-tony-f ()
   (multiple-value-prog1
       '("After the initial bla bla, how is your head, "
	 "man, were you drunk last night, you made a complete "
	 "ass of yourself, the naked singing on the table, "
	 "in short the usual retrospection of a nights out, "
	 "Tony comes straight to the point:~%~%"
	 "\"Mate, I've tried everywhere, but the Metallica "
	 "concert tonight is completely sold out. I've heard "
	 "however, that there is a pub quiz this afternoon "
	 "at the Happy Goose, with the price being one ticket "
	 "for tonight's show.~%You hopefully still remember "
	 "that I bought the tickets last time, and you had all "
	 "the time in the world to get off your lazy bollocks "
	 "and buy these ones. You will have to win the pub quiz "
	 "give me the ticket, and find yourself some other way "
	 "to get to the concert. Now get a fucking move on\"~%~%"
	 "With these beautiful and inspiring words, Tony leads you "
	 "out of his house and onto the street. ")
     (change-location *friends-house*)
     (setf (:cexit *friends-house*) '(("south" *friends-hallway* nil nil)))))

(defun take-dog-f ()
  '("I am sorry, you cannot take him. "
    "He's not yours, you can't go around and take someone "
    "home, just because he wags his tail at you. This is "
    "not San Francisco. "))

(defun look-litterbox-f ()
  (if (not (member *back-stage-pass* (:things *smoking-room*)))
      (multiple-value-prog1
	  '("The litterbox, made of some sort of "
	    "aluminium, has the shape of a big hourglass. "
	    "The top is filled with sand, where you stub "
	    "out your cigarettes, the bottom has a wide "
	    "holde for the usual rubbish. "
	    "You notice some sort of laminated card lying "
	    "on the floor behind the box. ")
	(push '*back-stage-pass* (:things *smoking-room*)))
      '("A big aluminium litterbox in the shape of a hourglass ")))

(defun look-back-stage-pass-f ()
  (:sdescription *back-stage-pass*))

(defun take-pass-f ()
  (multiple-value-prog1
    '("After realizing that you are holding a backstage pass "
      "you put it around your neck with shaking hands. "
      "This is how Harrison Ford must have felt when he "
      "found the holy grail. After all, you will now "
      "not only be able to see Metallica live and from "
      "a great spot, but also to gorge yourself on a "
      "vip buffet. Free food, free drink and free metal. "
      "Life can be so fucking fantastic. ")
    (take-object '*back-stage-pass*))) 

(defun take-food-f ()
  '("In a very dignified way you stuff some shrimps "
    "into your mouth and grab a beer to wash it down. "
    "I'd like to remind you however, that we didn't come "
    "here for this, did we? "))

(defun take-brush-f ()
  '("Really? You want to take the toilet brush? "
    "Is this some kind of strange fetisch, or is your "
    "approach to life to take everything what's lying around? "
    "You should be ashamed of yourself. "))

(defun take-paper-f ()
  '("Please leave it. How would you like it to go to the "
    "toilet, having to go very urgently, only to realize "
    "that there's no paper? "))

(defun look-cistern-f ()
  (multiple-value-prog1
      '("As you look at the cistern, you notice that it's lid "
	"is sitting loosely on it. Curious as you are, you lift "
	"the lid and find a key on one edge. ")
    (push *key* (:things *toilet-stall*))))

(defun take-key-f ()
  (multiple-value-prog1
      '("You slip the key into your pocket.")
    (take-object '*key*)
    (setf (:cexit *staircase*) '(("south" *cellar* has-key t)))))

(defun has-key ()
  "if player has key in inventory, change location to *cellar*
   otherwise print message that door can only be opened with key."
  (if (member *key* (:inventory *player*))
      (multiple-value-prog1
	  '("You take the key from your pocket, insert it, "
	    "wiggle a bit, eh voila, it opens and you step into the "
	    "cellar")
	(change-location *cellar*))
      '("You need a key to get in there. The door is locked. ")))

(defun talk-to-susan-f ()
  (multiple-value-prog1
      '("Timidly you approach Susan, and ask her if she "
	"could recommend any beer for you to buy. "
	"(I have to hand it to you, that's one pick-up line for "
	"the ages.)~%For some unfathomable reason however, "
	"Susan seems to like you. She flashes a wonderful "
	"smile, neatly emphasizing her cute dimples. "
	"Your heart starts racing, she is absolutely perfect. "
	"~%~%\"Just yesterday, we got this beer from Germany in. "
	"It's called \"Fürstenberg\" and it is a masterpiece in "
	"beer brewing.\"~%~%As it never takes long to persuade you, "
	"you go on and buy 20 bottles. Somehow you find the courage "
	"to ask Susan out and with a gratified smile she promises to "
	"have drinks with you next weekend.~%Happy as if your favourite "
	"football team just won the championship, you take the beer "
	"and leave the store. ")
    (change-location *off-licence*)
    (push *beer* (:inventory *player*))
    (block-exit *off-licence* '("south" *finnegans*)
		'("the shop seems to have closed. " ))))

(defparameter verb-synonyms
  '(("use" :use-v)
    ("utilize" :use-v)
    ("start" :start-v)
    ("power" :start-v)
    ("put on" :wear-v)
    ("wear" :wear-v)
    ("dress with" :wear-v)
    ("type password" :type-pass-v)
    ("enter password" :type-pass-v)
    ("press" :use-v)
    ("examine" :look-closer-v)
    ("talk" :talk-to-v))
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

(defun change-location (room)
  "When changing locations, set global-variable *location* to new location.
   Describe room either with first or later description."
  (change-loc *player* room)
  (describe-room  (current-location)))

(defun describe-list-of-items-in-location (room)
  "Return list of descriptions of all items in a room."
  (flatten (mapcar #'(lambda (x) (:fdescription (symbol-value x)))
		   (:things room)))) 

 ;; (defun describe-list-of-items-in-location-later (room)
 ;;  "Return the ldescription of all itemns in a room."
 ;;  (flatten (mapcar #'(lambda (x) (:ldescription (symbol-value x)))
 ;; 		   (:things room))))

(defun describe-list-of-items-in-location-later (room)
  "return a list with all item descriptions in a location.
   If a location has a :ldescription print :ldescription else use
   :fdescription of item."
  (flatten (mapcar #'(lambda (x) (if (:ldescription (symbol-value x))
				     (:ldescription (symbol-value x))
				     (:fdescription (symbol-value x))))
		   (:things room))))

(defun describe-room ( room)
  "if visiting loc for first time return list of :fdesc room
   appended by description of all items in current loc.
   If loc has been visited, return :ldescription of loc."
  (if (member :notseen (:flags room))
      (multiple-value-prog1
	(append  (:name room) (:fdescription room)
		 (describe-list-of-items-in-location room))
	(update-flag room :notseen :seen) )
      (multiple-value-prog1
	  (append (:name room) (:ldescription room)
		  (describe-list-of-items-in-location-later room)))))


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
	(convert-symbol
	 (second (member :pick-up-v (flatten (:action obj)))))))
      ((not (member :fixed (:flags obj)))
       (take-object (last-element list)))
      (t '("You cannot take that!")))))

(defun build-substring (list)
  "concatenate first and second word in list to single string"
  (concatenate 'string (first list) " " (second list))) 

(defun is-action-p (list)
  "return action flag from verb synonyms if entered command is 
   a action command"
  (cond
    ((equalassoc (first list) verb-synonyms)
     (second (equalassoc (first list) verb-synonyms)))
    ((equalassoc (build-substring list) verb-synonyms)
     (second (equalassoc (build-substring list) verb-synonyms)))
     (t nil)))



(defun no-object ()
  '("There is no such thing "))



