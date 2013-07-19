
(defpackage #:world
  (:use #:cl )
  (:export loc item player *bedroom* *hallway* *frontdoor* *park-lane-east*
	   *park-entrance* *park-entrance-east* *laptop* *poster* *clothes*
	   *player* :fdescription :sdescription :ldescription :uexit *fish*
	   :nexit :cexit :flags :things :name :synonym  :action cexit-read-con
	   :flags *park-center* *park-lane-east* pond))
(in-package #:world)


(defclass loc ()
  ((name :initarg :name :initform '() :accessor :name)
   (fdescription :initarg :fdescription :initform '() :accessor :fdescription)
   (ldescription :initarg :ldescription :initform '() :accessor :ldescription)
   (sdescription :initarg :sdescription :initform '() :accessor :sdescription)
   (uexit :initarg :uexit :initform '() :accessor :uexit)
   (nexit :initarg :nexit :initform '() :accessor :nexit)
   (cexit :initarg :cexit :initform '() :accessor :cexit)
   (flags :initarg :flags :initform '() :accessor :flags)
   (things :initarg :things :initform '() :accessor :things)))


(defclass item ()
  ((name :initarg :name :initform '() :accessor :name)
   (synonym :initarg :synonym :initform '() :accessor :synonym)
   (fdescription :initarg :fdescription :initform '() :accessor :fdescription)
   (ldescription :initarg :ldescription :initform '() :accessor :ldescription)
   (sdescription :initarg :sdescription :initform '() :accessor :sdescription)
   (action :initarg :action :initform '() :accessor :action)
   (flags :initarg :flags :initform '() :accessor :flags)))

(defclass player ()
  ((location :initarg :location :accessor :location)
   (inventory :initarg :inventory :initform '() :accessor :inventory)))



(defparameter *bedroom*
  (make-instance 'loc 
		 :fdescription '("the bedroom. Very messy. Very tiny.")
		 :ldescription '("you are in your bedroom. You "
				 "should seriously think about "
				 "cleaning it up.")
   :cexit '(( "west"  *hallway* wear-clothes))
   
   :nexit '(( "east" ("did you seriously think about leaving by the window?
		    I know you had a rough night but please use the door
		    like other normal people.")))
   :things '(*laptop* *clothes* *poster*)
   :flags :notseen))


(defparameter *hallway*
  (make-instance 'loc
   :ldescription '("the hallway. A narrow thing leading from "
		   "your bedroom to the east to your frontdoor "
		   "leading into town to the west.")
   :uexit '(("east" *bedroom*)
	    ("west" *frontdoor*))))

(defparameter *frontdoor*
  (make-instance 'loc
   :ldescription '("You leave your house and find yourself at an "
		   "absolutely marvellous spring day. It is warm, "
		   "sunny and the birds are singing. Its exactly "
		   "the sort of day that makes nearly everyone happy.")
   :sdescription '("you stand outside of your house.")
   :uexit '(("east" *hallway*) ("west" *park-entrance*)
	    ("northwest" *main-road*))))

(defparameter *park-entrance-east*
  (make-instance 'loc 
   :fdescription '("This is the entrance to a beautiful little park. "
		   "A gorgeous english garden with some nice shady "
		   "spots and plenty of benches to rest.")
   :ldescription '("You are at the east entrance of a park.")
   :uexit '(("west" *frontdoor*) ("east" *park-lane-east*))))

(defparameter *park-lane-east*
  (make-instance 'loc
   :fdescription '("You are on a small footpath in a beautiful park. "
		   "Tall chestnut trees provide a welcome shadow on "
		   "this marvellous day. To the west you can reach "
		   "the center of this park, to the east leads a "
		   "path towards your house.")
   :ldescription '("you are on a small path in the park.")
   :sdescription '()
   :uexit '(("west" *park-center*)
	    ("east" *park-entrance-east*))
   :flags :notseen))

(defparameter *park-center*
  (make-instance 'loc
   :fdescription '("This is the centerpiece of this municipal "
		   "master piece. A wide english lawn inviting "
		   "you to lie down and have a nap, or to play "
		   "a round of some football.")
   :ldescription '("this is the center of the little city park.")
   :uexit '(("south" *pond* '("to the south you can see a little pond."))
	    ("east" *park-lane-east* '("There is a path leading from "
				       "east to west through the park."))
	    ("west" *park-lane-west*))
   :flags :notseen))

(defparameter pond
  (make-instance 'loc
   :fdescription '("You are at a tiny pond, holding very clear water, "
		   "so clear in fact, that you can count all its fish.")
   :ldescription '("You stand at a tiny little pond.")
   :uexit '(("north" *park-center* '("to the north you can get "
				     "back to the park center.")))
   :things '(*fish*)
   :flags :notseen))


(defparameter *laptop*
  (make-instance 'item 
   :name '(a laptop)
   :synonym '(notebook laptop computer )
   :fdescription '("on a table near the exit to the west is a laptop.")
   :ldescription '("your old sturdy laptop. Not the latest and shiniest
		   but money is very expensive so you still
		   make do with it." )
   :sdescription '("your laptop. It used to be black.
		   Whats the color of grime again?")
   :action '((:use-v  use-laptop-f)
	     (start-v power-on-laptop-f) (type-pass-v crack-password-p))
   :flags '(poweroff notseen)))

(defparameter *clothes*
  (make-instance 'item
   :name '(your clothes)
   :fdescription '("strewn all over the floor are your clothes.")
   :ldescription '("jeans and a t-shirt. nothing fancy.")
   :action '((wear-v put-on-clothes))
   :flags :notwearing))

(defparameter *poster*
  (make-instance 'item
   :name '(a poster)
   :fdescription '("On the wall you can see an old poster.")
   :sdescription '("It is a very old nearly completely faded poster."
		   " You can only make out a painted scene of rows "
		   "of white crosses in a field.")
   :ldescription '("Oh you joyful Master of Puppets. You mother "
		   "of all metal records.")
   :action '((look-closer-v describe-poster-f))))

(defparameter *fish*
  (make-instance 'item
   :name '("healty rainbow trout.")
   :fdescription '("There is one big trout in the pond.")
   :synonym '("tasty looking" "healthy")
   :ldescription '("you are looking at a very healthy and "
		   "most probably good tasting rainbow trout.")
   :flags '(("taken" 0))
   :action '((pick-up pick-up-trout-f))))

(defparameter *player*
  (make-instance 'player :location *bedroom*
		 :inventory '()))



