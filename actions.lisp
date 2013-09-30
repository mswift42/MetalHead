;;; actions.lisp - game-logic and all actions, i.e.
;;; player-interaction with game-world.

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
      ((string-assoc direction ce)
       (flatten (cons 'ce (string-assoc direction ce))))
      ((string-assoc direction ue)
       (flatten (cons 'ue (string-assoc direction ue))))
      ((string-assoc direction ne)
       (flatten (cons 'ne (string-assoc direction ne))))
      (t nil))))                                                             

(defgeneric find-synonym (item string)
  (:documentation "return synonym for item if found."))

(defmethod find-synonym ((self item) string)
  "if synonym is in synonym list of item, return item
   (find-synonym *laptop* 'laptop') -> #<item{af17569}"
  (if (string-member string (:synonym self))
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
	(remove-if #'(lambda (x) (or (equal item (symbol-value x))
				     (equal item x)))
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
    ((string-member input *directions*) input)
    ((string-assoc input *directions-synonyms*)
     (second (string-assoc input *directions-synonyms*)))
    (t nil)))

(defun move-p (string)
  "return if string is a movement command."
  (string-member string '("go" "move" "walk")))

(defun use-laptop-f ()
  (if (member :poweroff (:flags *laptop*))
      '("Your laptop is turned off")
      '("you could browse your favourite websites "
	"all day, you good old procrastinator, "
	"however I'd propose you simply check your Email.")))

(defun power-on-laptop-f ()
  (update-flag *laptop* :poweroff :poweron)
  '("You press the power button. You hear "
    "some funny noises, and it "
    "actually starts booting. One cup of Tea "
    "later, and you start at the login screen. "))

(defun wear-clothes ()
  "if not wearing clothes, print out text . Else change location to hallway."
  (if (eq (first  (:flags *clothes*)) :notwearing)
      '("You seriously should think about putting on clothes first. "
	"I'm sure you think you are Gods gift to the other sex, but even "
	"if you were, and you're not, that's still no reason to "
	"go outside half-naked. ")
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


;; I'm using multiple-value-prog1 here because it evaluates it's
;; first argument and returns it, but silently evaluates the proceeding
;; forms. Otherwise gui.lisp 's format-output function does not know
;; what to print. 
(defun put-on-clothes ()
  (multiple-value-prog1
      '("You put on your clothes, a near effortless process, except "
	"your head really hurts when you bend down and those "
	"stupid jeans seem to have gotten tighter again. ")
    (update-flag *clothes* :notwearing :wearing)
    (setf (:cexit *bedroom*) '(("west" *hallway* wear-clothes t)))
    (take-object '*clothes*)))


(defun take-laptop-f ()
  '("You cannot take it. It's too heavy, "
    "the battery is not working and it's "
    "highly unlikely that it would survive "
    "any form of transport."))

(defun read-email-f ()
  '("You read your emails. Your spam filter thankfully takes care "
    "of most blatant scam mails, yet you still managed to get a "
    "special offer for penis enlargement pills. I don't know how "
    "desperate you are, but trust me, they work as well as "
    "hair regrow treatments.~%You got one interesting email though, "
    "your best friend Tony writes you to visit him this afternoon "
    "and to bring some beer, he's got some very urgent news. "))

(defun take-clothes-f ()
  "Text to return when taking clothes"
  '("You take your clothes. Rather awkwardly you are "
    "now standing there like a bloody idiot holding "
    "your clothes in your hands.~%"
    "You should maybe think about putting them on."))


(defun press-doorbell-f ()
  "Check if dialog with tony has happened. if not, print text 
   and set location to friends-hallway. if dialog has happened 
   only print text,i.e. location *friends-hallway* is closed."
  (if (member :friend-visited (:flags *doorbell*))
      '("you press the doorbell and listen to a very romantic "
	"song of death. Unfortunately, it seems as if your friend is "
	"not at home, because despite of you pressing the bell "
	"for 5 minutes now, no one is coming to open the door. ")
      (append
       '("You press the doorbell. With a startling amount "
	 "of noise, you hear the intro to Death's \"Leprosy\"~%"
	 "Maybe 20 seconds later, the door opens and your friend "
	 "beckons you inside.")
       (change-location *friends-hallway*))))

(defun look-doorbell-f ()
  (:sdescription *doorbell*))

(defun look-band-poster-f ()
  '("These are your typical band posters. They are not very "
    "imaginative, some show people wearing leather looking mean "
    "and some show people wearing jeans looking happy. I don't "
    "think the people designing those posters went for originality. "
    "You are a metal band? OK, wear a wristband with spikes, "
    "cross your arms and look constipated. Time to take a picture.~%"
    "One of the posters, however, stands out. A Metallica Poster with "
    "a yellow sticker, reading \"Sold out\" across it. Life's a bummer "
    "if you're a hummer. "))

(defun take-band-poster-f ()
  '("While I appreciate the effort to enlarging your poster collection, "
    "the bible clearly states, that you shouldn't steal. Also, "
    "Metal is the sound of the devil, and playing with yourself makes "
    "you go blind. (I should know, I'm typing this in braille. )"))

(defun increment-fish-counter ()
  "Increase :taken counter of item *fish*"
  (incf (second (string-assoc "taken" (:flags *fish*))) ))

(defun pick-up-trout-f ()
  "take the fish out off the pond."
  (let ((counter (second (string-assoc "taken" (:flags *fish*)))))
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
	   '("OK, OK, I give up. Carefully you wade into the pond "
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
  "tell player about hidden key in pub."
  '("As you come closer to read the inscription in the "
    "bench, you notice two things: A: the bench smells "
    "of vomit, and B: the text written in the wood reads "
    "\"For a heavy time, go into the second toilet stall "
    "in the golden goose\"~%Well, we can't pass up such "
    "an opportunity now, can we? "))

(defun sit-on-bench-f ()
  '("With a big sigh you sit down and enjoy the comfy wood for a "
    "couple of minutes. Unfortunately you get bored easily, so "
    "you get up again. "))

(defun burn-rubble-f ()
  "If player is inclined in a way that would make him burn stuff. "
  '("I know that burning stuff is fun. However, you should try "
    "sneaking into the show, and not alarm everyone to your presence. "))

(defun look-rubble-f ()
  '("it's just wooden palettes stacked upon each other and a "
    "lot of dirt. "))

(defun pub-quiz-played-f ()
  "If player has won the pub-quiz allow access to toilets."
  (if (member :quiz-played (:flags *pub*))
      (change-location *pub-toilets*)
      '("You can't go now. The pub quiz is about to start and "
	"that's far too important to miss. ")))

(defun bought-beer-v ()
  "If player has bought beer at finnegans allow him 
   to enter living-room at friends house."
  (if (member '*beer* (:inventory *player*))
      (multiple-value-prog1
	  (describe-room *living-room*)
	(change-location *living-room*))
      '("You need to buy beer first. ")))

(defun talk-to-tony-f ()
  "Dialog with tony. After info about pub-quiz, set location
   to *friends-house* and add :friend-visited to doorbell-flags, thus
   blocking the exit to inside *friends-hosue*. "
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
    (setf (:inventory *player*) (delete '*beer*
					(:inventory *player*)))
    (push :friend-visited (:flags *doorbell*))))

(defun pub-open-v ()
  "If dialog with tony has happened 'open' pub.
   otherwise print message that pub still has closed."
  (if (member :friend-visited (:flags *doorbell*))
      (change-location *pub*)
      '("The pub is not open yet. You should find some "
	"other way to pass the time. Drunk and stupid is no "
	"way to go through life. ")))

(defun take-dog-f ()
  '("I am sorry, you cannot take him. "
    "He's not yours, you can't go around and take someone "
    "home, just because he wags his tail at you. This is "
    "not San Francisco. "))

(defun look-litterbox-f ()
  "check if back-stage-pass is in inventory. If not, and if 
   litterbox is being examined for first time, print long text 
   and push *back-stage-pass* to (:things *smoking-room*. Else 
   print short text."
  (if (not (or (member '*back-stage-pass* (:things *smoking-room*))
	       (member '*back-stage-pass* (:inventory *player*))))
      (multiple-value-prog1
	  '("The litterbin, made of some sort of "
	    "aluminium, has the shape of a big hourglass. "
	    "The top is filled with sand, where you stub "
	    "out your cigarettes, the bottom has a wide "
	    "hole for the usual rubbish. "
	    "You notice some sort of laminated card lying "
	    "on the floor behind the box. ")
	(push '*back-stage-pass* (:things *smoking-room*)))
      '("A big aluminium litterbin in the shape of a hourglass ")))

(defun look-back-stage-pass-f ()
  (:sdescription *back-stage-pass*))

(defun take-pass-f ()
  (multiple-value-prog1
      '("After realising that you are holding a backstage pass "
	"you put it around your neck with shaking hands. "
	"This is how Harrison Ford must have felt when he "
	"found the holy grail. After all, you will now "
	"not only be able to see Metallica live and from "
	"a great spot, but also to gorge yourself on a "
	"vip buffet. Free food, free drink and free metal. "
	"Life can be so fucking fantastic. ")
    (take-object '*back-stage-pass*))) 

(defun back-stage-pass-f ()
  "if back-stage-pass is in inventory, print a 'successful' message 
   and change location to vip-area. else print 'failure' message."
  (if (member '*back-stage-pass* (:inventory *player*))
      (append
       '("the bouncer takes a look at your backstage "
	 "pass, tells you to have a 'nice one' and "
	 "ushers you inside.~%~%")
       (change-location *vip-area*))
      '("The bouncer crosses his arms, and tells you:~%"
	"\"No pass, no entry.\"")))

(defun take-food-f ()
  '("In a very dignified way you stuff some shrimps "
    "into your mouth and grab a beer to wash it down. "
    "I'd like to remind you however, that we didn't come "
    "here for this, did we? "))

(defun take-brush-f ()
  '("Really? You want to take the toilet brush? "
    "Is this some kind of strange fetish, or is your "
    "approach to life to take everything what's lying around? "
    "You should be ashamed of yourself. "))

(defun take-paper-f ()
  '("Please leave it. How would you like it to go to the "
    "toilet, having to go very urgently, only to realise "
    "that there's no paper? "))

(defun look-cistern-f ()
  "check if key is in inventory or member of (:things *toilet-stall*)
   if not, print long message and push *key* to (:things *toilet-stall*).
   Else, print short message, describing cistern."
  (if (not (or (member '*key* (:things *toilet-stall*))
	       (member '*key* (:inventory *player*))))
      (multiple-value-prog1
	  '("As you look at the cistern, you notice that it's lid "
	    "is sitting loosely on it. Curious as you are, you lift "
	    "the lid and find a key on one edge. ")
	(push '*key* (:things *toilet-stall*)))
      '("A white ceramic cistern. Very useful to flush toilets. ")))

(defun take-key-f ()
  (multiple-value-prog1
      '("You slip the key into your pocket.")
    (take-object '*key*)))

(defun has-key ()
  "if player has key in inventory, change location to *cellar*
   otherwise print message that door can only be opened with key."
  (if (member '*key* (:inventory *player*))
      (append
       '("You take the key from your pocket, insert it, "
	 "wiggle a bit, eh voila, it opens and you step into the "
	 "cellar.~%~%")
       (change-location *cellar*))
      '("You need a key to get in there. The door is locked. ")))

(defun talk-to-susan-f ()
  (multiple-value-prog1
      '("Timidly you approach Susan, and ask her if she "
	"could recommend any beer for you to buy. "
	"(I have to hand it to you, that's one pick-up line for "
	"the ages.)~%For some unfathomable reason however, "
	"Susan seems to like you. She flashes a wonderful "
	"smile, neatly emphasising her cute dimples. "
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
    (push '*beer* (:inventory *player*))
    (block-exit *off-licence* '("south" *finnegans*)
		'("the shop seems to have closed. " ))))

(defun buy-pub-quiz-ticket-f ()
  (multiple-value-prog1
      '("Just as you buy the ticket the pub quiz begins.~%~%")
    (setf *running-pub-quiz* (bt:make-thread (lambda ()
					       (pub-quiz-window))))))

(defun won-ticket-f ()
  (multiple-value-prog1
      '("You did enough to win the quiz. Sadly it seems, "
	"metal is not that popular anymore. Well, as "
	"they say, there's no accounting for taste. "
	"As you are about to order your next pint, "
	"Tony enters the pub, comes up to you and "
	"takes the price of of you.~%~%"
	"\"I really hope, I'll see you tonight\"~%~%"
	"are his parting words.~%You still have to find a way "
	"inside the show for yourself.~%~%"
	"Please close this window now. ")
    (push :quiz-played (:flags *pub*))))

(defun lost-ticket-f ()
  '("You couldn't even get 40 % of the questions right? "
    "Shame on you. ~%~%or as the kids say, GAME OVER~%"
    "You need to restart the game. "))

(defun end-f ()
  (multiple-value-prog1
      (:fdescription *backstage-area*)
      (change-location *backstage-area*)))

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
    ("look at" :look-closer-v)
    ("l at" :look-closer-v)
    ("check" :read-v)
    ("read" :read-v)
    ("burn" :burn-v)
    ("buy" :buy-v)
    ("talk" :talk-to-v)
    ("sit down" :sit-down-v))
  "association list to lookup the fitting functions in an object to its verb")


(defun return-synonym (verb)
  "return the function synonym to the entered verb."
  (second (string-assoc verb verb-synonyms))) 

(defun convert-symbol (s)
  "convert in file  world stored symbol to its in file 
    action stored function value '(convert-symbol :use-laptop-f) -> use-laptop-f"
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
      (t (funcall (find-symbol (symbol-name (fourth exitlist))))))))


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
  (flatten (mapcar #'(lambda (x) (:fdescription (symbol-value x))) (:things room))))



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
    appended by description of all items in current loc and set
    flag of room to :seen.
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
  "return if input is a 'look' command. If input is only a single 
    look, call describe-room function. If looked is a :thing in 
    current-location return :ldescription of item.(laptop in bedroom)
    If the object is mentioned in the description, for example bed in 
    bedroom, call the nothing-special-f function."
  (let ((len (length list)))
    (cond
      ((and (= 1 len)
	    (is-look-p (first list)))
       (describe-room (current-location)))
      ((and (> len 1)
	    (is-look-p (first list))
	    (find-synonym-in-location (last-element list)))
       (:sdescription
	(find-synonym-in-location (last-element list))))
      ((and (> len 1)
	    (is-look-p (first list))
	    (search (last-element list) 
		    (print-list
		     (append (:fdescription (current-location))
			     (describe-list-of-items-in-location (current-location))))))
       (nothing-special-f (last-element list)))
      (t nil))))

(defun not-here (list)
  "check if last item in list appears in :fdescription
    of current location or is a synonym for any of it's items."
  (not (or (search (last-element list)
		   (print-list (append (:fdescription (current-location)))))
	   (find-synonym-in-location (last-element list)))))

(defun is-look-p (exp)
  "return if command is member of synonyms for 'look'"
  (string-member exp '("look" "examine" "study" "view" "scan" "parse"
		       "explore" "l")))

(defun nothing-special-f (word)
  "concatenate inputed word with a random string. Needed for function 
    look-command-p"
  (list (concatenate 'string (random-string '("There is nothing special about the "
					      "It's just an ordinary "
					      "It's a ")))
	word))

(defun is-take-p (exp)
  "return if command is member of synonyms for 'take'"
  (string-member exp '("t" "take" "grab" "snatch" "get")))

(defun take-command (list)
  "if last element in list is a 'item' instance, check if 
    it has a :pick-up-v action stored in (:flags item). If yes, 
    call the according function. If :fixed in :flags print 
    you cannot take that. Else call take-object function."
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
    ((and (string-assoc (first list) verb-synonyms)
	  (find-synonym-in-location (last-element list)))
     (second (string-assoc (first list) verb-synonyms)))
    ((and (string-assoc (build-substring list) verb-synonyms)
	  (find-synonym-in-location (last-element list)))
     (second (string-assoc (build-substring list) verb-synonyms)))
    (t nil)))

(defun inventory-p (exp)
  "is expression a inventory command"
  (string-member exp '("i" "inventory")))

(defun print-inventory ()
  "print 'you are carrying ' + names of all items in 
   inventory"
  (let ((inv (loop for i in (:inventory *player*)
		collect (:name (symbol-value i)))))
    (concatenate 'string "You are carrying "
		 (substitute #\, #\.
			     (print-list (flatten inv)))
		 ".")))

(defun inventory-command-p (list)
  "Check if length of list is 1 and the first element
   is a inventory-p command. If yes, print list of 
   inventory items. "
  (if (and (= 1 (length list))
	   (inventory-p (first list)))
      (list (print-inventory ))))

(defun is-help-p (exp)
  "is expression a help command"
  (string-member exp '("h" "help")))

(defun print-help ()
  "print help screen."
  '("This game has not the most sophisticated parser in the history "
    "of sophisticated parser, but I hope you will manage.~%~%"
    "To change locations: ~%~%"
    "Enter the direction you want to go to:~%"
    "west northwest , abbreviations like s, n, se work too.~%~%"
    "To print the description of a location you are in:~%~%"
    "l or look~%~%"
    "To examine an object at your current location:~%~%"
    "look at or examine <object>~%~%"
    "to use an object:~%~%"
    "use <object>~%~%"
    "Pick up an item:~%~%"
    "take <object>~%~%"
    "There are of course more valid commands, for example~%~%"
    "put on clothes~%~%will exactly do that. "
    "~%~%Enjoy and good luck.~%~%"))

(defun no-object ()
  '("There is no such thing "))

(defparameter *questions-and-answers*
  '(("Who was the predecessor of Bruce Dickinson as frontman of Iron Maiden" 
     ("Di'Anno" "Di Anno" "DiAnno" "Paul Di'Anno" "Paul Di Anno"))
    ("Name of a swedish Band that got it's name from a volcano in Tolkien's Lord of the Rings." 
     ("Amon Amarth"))
    ("Tom Angelripper was founding member of which German Band?"
     ("Sodom"))
    ("What is the name of Black Sabbath's debut album?"
     ("Black Sabbath"))
    ("What was the stagename for Pantera's / Damageplan' tragically killed lead guitarist?"
     ("Dimebag Darrell" "Dimebag" "Darrell" "Darrel" "Darell"))
    ("What was Slayer's most commercially successful album?"
     ("South of Heaven"))
    ("What was the name of Testament's debut album?"
     ("The Legacy" "Legacy"))
    ("Who founded Death?"
     ("Chuck Schuldiner" "Schuldiner"))
    ("Who was the original drummer for Slayer"
     ("Dave Lombardo" "Lombardo"))
    ("Three bands were considered the \"big three of Teutonic Trash Metal\" Sodom, Kreator and?"
     ("Destruction"))
    ("Complete this song title of Morbid Angel: God of ..."
     ("Emptiness"))
    ("What is the first song on Metallica's Ride the Lightning?"
     ("Fight Fire with Fire"))
    ("What is the title of Iron Maiden's live album of 1985?"
     ("Live after death"))
    ("In which year was Metallica's Kill 'em all released? "
     ("1983" "83"))
    ("Which band was accused in 1990 for being responsible for the suicide attempts of 2 men in Sparks, Nevada, USA? "
     ("Judas Priest"))
    ("This English band, formed 1979 in Newcastle, are considered a major influence on thrash metal."
     ("Venom"))
    ("Of which Swedish band is Tomas Lindberg the frontman? "
     ("At the Gates"))
    ("Which band released 1988 the record \"Under the Influence\"? "
     ("Overkill"))))

(defun question-list (n)
  "build list of n random questions. "
  (let ((ql nil))
    (loop
       for i from 1
       while (< (length ql) n)
       do (pushnew (first (random-string *questions-and-answers*)) ql)
       finally (return ql))))

(defun answer-for-question (question)
  "return the answer for question in *questions-ans-answers*"
  (second (string-assoc question *questions-and-answers*)))

(defun correct-answer-p (question answer)
  "Is the answer correct for the given question?"
  (string-member answer (answer-for-question question)))





