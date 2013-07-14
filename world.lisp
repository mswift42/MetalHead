
(defpackage #:world
  (:use #:cl )
  (:export loc item player *bedroom* *hallway* *frontdoor* *park-lane-east*
	   *park-entrance* *park-entrance-east* *laptop* *poster* *clothes*))
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
   (location :initarg :location :initform '() :accessor :location)
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
   :cexit '(( west  *hallway* wear-clothes))
   
   :nexit '(( east ("did you seriously think about leaving by the window?
		    I know you had a rough night but please use the door
		    like other normal people.")))
   :things '(*laptop* *clothes* *poster*)
   :flags :notseen))


(defparameter *hallway*
  (make-instance 'loc
   :ldescription '("the hallway. A narrow thing leading from "
		   "your bedroom to the east to your frontdoor "
		   "leading into town to the west.")
   :uexit '((east *bedroom*)
	    (west *frontdoor*))))


(defparameter *frontdoor*
  (make-instance 'loc
   :ldescription '("You leave your house and find yourself at an "
		   "absolutely marvellous spring day. It is warm, "
		   "sunny and the birds are singing. Its exactly "
		   "the sort of day that makes nearly everyone happy.")
   :sdescription '("you stand outside of your house.")
   :uexit '((east *hallway*) (west *park-entrance*) (nw *main-road*))))

(defparameter *park-entrance-east*
  (make-instance 'loc 
   :fdescription '("This is the entrance to a beautiful little park. "
		   "A gorgeous english garden with some nice shady "
		   "spots and plenty of benches to rest.")
   :ldescription '("You are at the east entrance of a park.")
   :uexit '((west *frontdoor*) (east *park-lane-east*))))

(defparameter *park-lane-east*
  (make-instance 'loc
   :fdescription '(You are in the town park. There is a path leading from east to west.)))


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
   :location '(*bedroom*)
   :action '((use-v  use-laptop-f)
	     (start-v power-on-laptop-f) (type-pass-v crack-password-p))
   :flags '(poweroff notseen)))

(defparameter *clothes*
  (make-instance 'item
   :name '(your clothes)
   :fdescription '("strewn all over the floor are your clothes.")
   :ldescription '("jeans and a t-shirt. nothing fancy.")
   :location '(*bedroom*)
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
   :location '(*bedroom*)
   :action '((look-closer-v describe-poster-f))))                                                       


