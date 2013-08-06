(defpackage #:world
  (:use #:cl )
  (:export loc item player *bedroom* *hallway* *frontdoor* *park-lane-east*
	   *park-entrance* *park-entrance-east* *laptop* *poster* *clothes*
	   *player* *housefront* :fdescription :sdescription :ldescription
	   :uexit *fish* :nexit :cexit :flags :things :name :synonym
	   :action cexit-read-con :flags *park-center* *park-lane-east*
	   *pond* *wear-clothes?* *bench* *park-lane-west*))
(in-package #:world)


(defclass loc ()
  ((name :initarg :name :initform '() :accessor :name)
   (fdescription :initarg :fdescription :initform '() :reader :fdescription)
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
		 :fdescription '("This is your bedroom. It has the "
				 "usual bedroom stuff, for example "
				 "there is a bed and other bedroom stuff.")
		 :ldescription '("you are in your bedroom. You "
				 "should seriously think about "
				 "cleaning it up.")
   :cexit '(("west" *hallway* wear-clothes nil ))
   
   :nexit '(("east" ("did you seriously think about leaving by the window?
		    I know you had a rough night but please use the door
		    like other normal people.")))
   :things '(:*laptop* :*clothes* :*poster*)
   :flags '(:notseen)))


(defparameter *hallway*
  (make-instance 'loc
   :fdescription '("You are in your hallway leading from your "
		   "bedroom in the east to the frontdoor in the west. "
		   "It's wallpaper has this lovely charming colormixture "
		   "of nicotine stain and sun faded cardboard. yikes.")
   :ldescription '("the hallway. A narrow thing leading from "
		   "your bedroom to the east to your frontdoor "
		   "leading into town to the west.")
   :uexit '(("east" *bedroom*)
	    ("west" *housefront*))
   :flags '(:notseen)))



(defparameter *housefront*
  (make-instance 'loc
    :fdescription '("You leave your house and find yourself at an "
		    "absolutely marvellous spring day. It is warm, "
		    "sunny an the birds are singing. It's exactly "
		    "the sort of day that makes everyone happy, the "
		    "kind of day where heros are made (you know "
		    "in the bees and flowers having sex sort of "
		    "way).~%It is also of course the kind of day that "
		    "makes you crave for a cool pint of Guiness with "
		    "it's foamy head, water slowly condensing on the "
		    "glass and the glorious sensation of a "
		    "first sip, which tastes, as some say, like angels "
		    "pissing in your mouth.")
    :ldescription '("You stand outside of your house.")
    :uexit '(("east" *hallway*
	      '("To the east you can get to your"
		"hallway "))
	     ("west" *park-entrance-east* '("and to west you can see "
					    "the entrace to a small park."))
	     ("northwest" *main-road* '("In the northwest you can see "
					"this towns main road.")))
    :flags '(:notseen)))

(defparameter *main-road*
  (make-instance 'loc
   :fdescription '("You are at a busy stretch of your tiny towns "
		   "main road. ")
   :ldescription '("this is the southern part of the main road.")
   :uexit '(("north" *main-road-north* '("to the north you can reach "
					 "the city center.")))))

(defparameter *main-road-north*
  (make-instance 'loc
   :fdescription '("You are in the center of your mediocre "
		   "home town. Like many others of its brethren "
		   "it certainly has seen better times, but not all is "
		   "bad, hey, atleast it is not ,... (insert name "
		   "of your rivaltown/most disliked town).")
   :ldescription '("This is the north part of your towns main road.")
   :uexit '(("south" *main-road* '("the road leads from south to north.")))))


(defparameter *pedestrian-street-west*
  (make-instance 'loc
   :fdescription '("A nicely paved pedestrian street marks the "
		   "beginning of the part of ")))

(defparameter *park-entrance-east*
  (make-instance 'loc 
   :fdescription '("This is the entrance to a beautiful little park. "
		   "A gorgeous english garden with some nice shady "
		   "spots and plenty of benches to rest.")
   :ldescription '("You are at the east entrance of a park.")
   :uexit '(("west" *park-lane-east*) ("east" *housefront*))))

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
   :flags '(:notseen)))

(defparameter *park-center*
  (make-instance 'loc
   :fdescription '("This is the centerpiece of this municipal "
		   "master piece. A wide english lawn inviting "
		   "you to lie down and have a nap, or to play "
		   "a round of some football.\n"
		   "To the south you can see a little pond and a path "
		   "leads through the park from east to west.")
   :ldescription '("this is the center of the little city park."
		   "To the south you can see a little pond and a path "
		   "leads through the park from east to west.")
   :uexit '(("south" *pond*)
	    ("east" *park-lane-east* )
	    ("west" *park-lane-west*))
   :things '(*bench*)
   :flags '(:notseen)))

(defparameter *pond*
  (make-instance 'loc
   :fdescription '("You are at a tiny pond, holding very clear water, "
		   "so clear in fact, that you can count all its fish.")
   :ldescription '("You stand at a tiny little pond.")
   :uexit '(("north" *park-center* '("to the north you can get "
				     "back to the park center.")))
   :things '(:*fish*)
   :flags '(:notseen)))

(defparameter *park-lane-west*
  (make-instance 'loc
   :fdescription '("This is the western part of this inner city park. "
		   "Plenty of trees provide a cooling shadow, "
		   "the flowers are in quite remarkable shape and "
		   "the air smells fresh and clean. This all helps in "
		   "rising your spirits, while your mp3 player blasts "
		   "\"Sepulturas' Roots Bloody Roots\"")
   :ldescription '("You are in the western part of the inner city park. "
		   "")))


(defparameter *laptop*
  (make-instance 'item 
   :name '("laptop")
   :synonym '("notebook" "laptop" "computer")
   :fdescription '("on a table near the exit to the west is a laptop.")
   :ldescription '("your old sturdy laptop. Not the latest and shiniest "
		   "but money is very expensive so you still "
		   "make do with it." )
   :sdescription '("your laptop. It used to be black. "
		   "Whats the color of grime again?")
   :action '((:use-v  :use-laptop-f)
	     (:start-v :power-on-laptop-f)
	     (:type-pass-v :crack-password-p)
	     (:pick-up-v :take-laptop-f))                        
   :flags '(:poweroff :notseen)))

(defparameter *clothes*
  (make-instance 'item
   :name '("your clothes")
   :synonym '("clothes")
   :fdescription '("strewn all over the floor are your clothes.")
   :ldescription '("jeans and a t-shirt. nothing fancy.")
   :action '((:wear-v :put-on-clothes :pick-up-v :take-clothes-f))
   :flags '(:notwearing)))

(defparameter *poster*
  (make-instance 'item
   :name '("a poster")
   :synonym '("poster")
   :fdescription '("On the wall you can see an old poster.")
   :ldescription '("It is a very old nearly completely faded poster."
		   " You can only make out a painted scene of rows "
		   "of white crosses in a field.")
   :sdescription '("Oh you joyful Master of Puppets. You mother "
		   "of all metal records.")
   :flags '(:fixed)
   :action '((:look-closer-v :describe-poster-f))))

(defparameter *fish*
   (make-instance 'item
   :name '("healty rainbow trout.")
   :fdescription '("There is one big trout in the pond.")
   :synonym '("tasty looking" "healthy")
   :ldescription '("you are looking at a very healthy and "
		   "most probably good tasting rainbow trout.")
   :flags '(("taken" 0))
   :action '((:pick-up-v :pick-up-trout-f))))
 
(defparameter *bench*
  (make-instance 'item
   :name '("ridiculously comfortable looking bench.")
   :fdescription '("You can see a ridiculously comfortable looking "
		   "bench here.")
   :ldescription '("this is a very comfortable looking bench. ")
   :sdescription '("As you examine the bench you notice that there's "
	           "something scratched into the wood.")
   :synonym '("comfortable" "cosy" "comfy" "inviting")
   :flags '((:fixed))
   :action '(:look-closer-v :read-inscription-f)))

(defparameter *outside-shop*
  (make-instance 'loc
   :fdescription '("You stand outside of an small off-licnce shop."
		   "")))

(defparameter *player*
  (make-instance 'player :location *bedroom*
		 :inventory '()))




