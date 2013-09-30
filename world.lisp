;;; world.lisp - All locations and items in game-world.

(in-package #:metalhead)

;; Classes for rooms, items and player.
;; fdescription is for inital description of a location or item,
;; ldescriptin describes a location which has been visited before.
;; uexit is an unconditinal exit, i.e. can be visited always.
;; nexit is not an exit, but instead of printing "no exit" it prints
;; a more interesting text.
;; cexit is a conditional exit, i.e, for locations you need a key, or have
;; accomplished some task previously.
;; flags are for storing information about a location, i.e. has this location
;; been visited before.
;; things is a list of items in a location.
;; the organisation of these classes has been heavily influenced by
;; a pdf called learning ZIL (http://www.xlisp.org/zil.pdf) which explains how to 
;; write interactive fiction. And yes, I'm well aware that MetalHead, especially
;; parser wise, cannot compare to the good old Infocom classics.


(defclass loc ()
  ((name :initarg :name :initform '() :reader :name)
   (fdescription :initarg :fdescription :initform '() :reader :fdescription)
   (ldescription :initarg :ldescription :initform '() :reader :ldescription)
   (uexit :initarg :uexit :initform '() :accessor :uexit)
   (nexit :initarg :nexit :initform '() :accessor :nexit)
   (cexit :initarg :cexit :initform '() :accessor :cexit)
   (flags :initarg :flags :initform '() :accessor :flags)
   (things :initarg :things :initform '() :accessor :things)))

;; Nearly identical to class loc.
;; synonym stores different synonyms for an object. i.e. synonym for *laptop*
;; are "laptop" "notebook" "computer", thus program understands take computer.
;; action describes what can be done with an object. A laptop can be started
;; picked up, used, ...

(defclass item ()
  ((name :initarg :name :initform '() :reader :name)
   (synonym :initarg :synonym :initform '() :reader :synonym)
   (fdescription :initarg :fdescription :initform '() :reader :fdescription)
   (ldescription :initarg :ldescription :initform '() :reader :ldescription)
   (sdescription :initarg :sdescription :initform '() :reader :sdescription)
   (action :initarg :action :initform '() :accessor :action)
   (flags :initarg :flags :initform '() :accessor :flags)))


(defclass player ()
  ((location :initarg :location :accessor :location)
   (inventory :initarg :inventory :initform '() :accessor :inventory))
  (:documentation "current location of player and list of inventory items."))

(defparameter *intro*
  '("You wake up. As you are very often wont do, you regret last "
    "night. It's not that you remember having embarrassed yourself, "
    "because, frankly, you hardly remember anything of what happened "
    "after 10 p.m., but you've got one sensational headache, and I'd "
    "rather not get started talking about the taste in your mouth. "
    "Slowly, you get up, drink about 4 pints of water and you are "
    "ready to start the day.~%~%~%"))


(defparameter *bedroom*
  (make-instance 'loc
		 :name '("bedroom~%~%")
		 :fdescription '("This is your bedroom. It has the "
				 "usual bedroom stuff, for example "
				 "there is a bed and other bedroom stuff."
				 "To the west is the hallway. ")
		 :ldescription '("you are in your bedroom. You "
				 "should seriously think about "
				 "cleaning it up. The hallway is to "
				 "the west. ")
   :cexit '(("west" *hallway* wear-clothes nil ))
   :things '(*laptop* *clothes* *poster*)
   :flags '(:notseen)))


(defparameter *hallway*
  (make-instance 'loc
   :name '("hallway~%~%")
   :fdescription '("You are in your hallway leading from your "
		   "bedroom in the east to the front door in the west. "
		   "It's wallpaper has this lovely charming mixture "
		   "of nicotine stain and sun faded cardboard. ")
   :ldescription '("the hallway. A narrow thing leading from "
		   "your bedroom to the east to your front door "
		   "leading into town to the west.")
   :uexit '(("east" *bedroom*)
	    ("west" *housefront*))
   :flags '(:notseen)))


(defparameter *housefront*
  (make-instance 'loc
    :name '("Outside your house~%~%")
    :fdescription '("You leave your house and find yourself at an "
		    "absolutely marvellous spring day. It is warm, "
		    "sunny, and  birds are singing. It's exactly "
		    "the sort of day that makes everyone happy, the "
		    "kind of day where heroes are made (you know "
		    "in the bees and flowers having sex sort of "
		    "way).~%It is also of course the kind of day that "
		    "makes you crave for a cool pint of Guinness with "
		    "it's foamy head, water slowly condensing on the "
		    "glass and the glorious sensation of a "
		    "first sip, which tastes, as some say, like angels "
		    "pissing in your mouth.~%"
		    "To the east you can get to your hallway. To the "
		    "west is the entrance to a park. The main road is "
		    "in the northwest from you and to the southwest you "
		    "can see an off licence shop. ")
    :ldescription '("You stand outside of your house. To the east "
		    "is you can get back inside, to the west is a park "
		    "entrance, to the northwest is the main road "
		    "and to the southwest is a off-licence shop. ")
    :uexit '(("east" *hallway*)
	     ("west" *park-entrance-east* )
	     ("northwest" *main-road* )
	     ("southwest" *off-licence* ))
    :flags '(:notseen)))

(defparameter *off-licence*
  (make-instance 'loc
   :name '("Outside Finnegan's Off-licence~%~%")
   :fdescription '("You stand outside of one of those typical "
		   "Off-licence / convenience stores. "
		   "A big sign promises fantastic bargains and "
		   "weekly special promotions. Well, hopefully "
		   "they have one of those plenty of beer for very "
		   "little money weeks.~%"
		   "To the south is the entrance to Finnegan's and "
		   "the street leads from  your house in the "
		   "northeast to the library in " "the west. ")
   :ldescription '("You are outside of Finnegan's off-licence. To "
		   "the south is the entrance to the shop and the "
		   "street leads from your house in the northeast "
		   "to the library in the west. ")
   :uexit '(("south" *finnegans*) ("west" *library*)
	    ("northeast" *housefront*))
   :nexit '()
   :flags '(:notseen)))

(defparameter *beer*
  (make-instance 'item
   :name '("beer. ")
   :synonym '("beer" "fuerstenberg" "fürstenberg")
   :fdescription '("You bought a fantastic pilsner beer. ")
   :sdescription '("It is a very nice pilsner beer "
		   "a very clean, smooth slightly bitter taste, "
		   "very wonderful. ")))



(defparameter *finnegans*
  (make-instance 'loc
   :name '("Finnegan's Off-licence~%~%")
   :fdescription '("This is Finnegan's off-licence and convenience "
		   "store. Shelves packed with everyday goods, and "
		   "a very nice selection of refreshing and "
		   "overwhelmingly alcoholic beverages.~%"
		   "The exit is to the north. ")
   :ldescription '("You inside Finnegan's off-licence. The exit "
		   "is to the north. ")
   :things '(*store-assistant*)
   :uexit '("north" *off-licence*)
   :flags '(:notseen)))

(defparameter *store-assistant*
  (make-instance 'item
   :name '("Susan")
   :synonym '("susan" "store assistant" "girl"
	      "woman" "assistant")
   :fdescription '("Behind the counter a young woman is "
		   "looking at you. Her name tag identifies her "
		   "as Susan, which you noticed because you were "
		   "staring at her fantastic t-shirt (You did think "
		   "I'd write tits, admit it, you dirty pig.). "
		   "You really shouldn't stare like this, it's rude, "
		   "although I have to admit, her body is nothing "
		   "short of spectacular. ")
   :ldescription '("Susan, the store assistant is looking "
		   "expectantly at you. ")
   :sdescription '("Susan is one very cute lady. Dark long hair, "
		   "very cute dimples when she smiles, and luckily she "
		   "smiles a lot. ")
   :action '((:talk-to-v :talk-to-susan-f))))

(defparameter *main-road*
  (make-instance 'loc
   :name '("main road~%~%")
   :fdescription '("You are at a busy stretch of your towns "
		   "main road. "
		   "Guys are flaunting their cars in a "
		   "\"Look at me, I've got a car\" sort "
		   "of way, elbows planted on the door, their "
		   "radios stating what music they are into. "
		   "(It's always shit by the way. Have you ever "
		   "wondered, why people, who generously decide to "
		   "letting you participate in their enjoyment of music "
		   "by blasting it through their phone's speaker only, "
		   "I repeat, only listen to crap?)~%Anyway "
		   "a lot of traffic here, so you should "
		   "probably move along. You can reach the end of the "
		   "main road to the north and you can "
		   "get back to your house to the southeast. ")
   :ldescription '("this is the southern part of the main road. "
		   "It ends to to the north and your house is to "
		   "the southeast. ")
   :uexit '(("north" *main-road-north* ) ("southeast" *housefront*))
   :flags '(:notseen)))

(defparameter *main-road-north*
  (make-instance 'loc
   :name '("main road north~%~%")
   :fdescription '("You are in the centre of your mediocre "
		   "home town. Like many others of its brethren "
		   "it certainly has seen better times, but not all is "
		   "bad, hey, at least it is not ,... (insert name "
		   "of your rival town/most disliked town)."
		   "The main road continues to the south and to the "
		   "northwest is the beginning of a pedestrian street. ")
   :ldescription '("This is the northern part of the main road. It "
		   "continues to the south and to the northwest begins "
		   "the pedestrian street. ")
   :uexit '(("south" *main-road* ) ("northwest" *pedestrian-street-east*))
   :flags '(:notseen)))


(defparameter *pedestrian-street-east*
  (make-instance 'loc
   :name '("pedestrian street~%~%")
   :fdescription '("This is what they call the \"High Street\""
		   "A cobblestone road and loads of stores and "
		   "betting shops. There is an arcade hall to "
		   "the north, to the southeast is the main road "
		   "and the pedestrian street continues to the west. ")
   :ldescription '("The road leads from the main road in the "
		   "southeast and it continues to the west. ")
   :uexit '(("southeast" *main-road-north*) ("north" *arcade-hall*)
	    ("west" *pedestrian-street-west*))
   :flags '(:notseen)))

(defparameter *arcade-hall*
  (make-instance 'loc
   :name '("Arcade Hall~%~%")
   :fdescription '("If you had too much money, you could lose it "
		   "here. Slot machines, those stupid video roulette "
		   "games, and some classic arcade machines. "
		   "At least it smells bad. The exit is to the south. ")
   :ldescription '("A silly arcade hall, dominated by silly slot "
		   "machines. The exit is to the south. ")
   :uexit '(("south" *pedestrian-street-east*))
   :flags '(:notseen)))

(defparameter *ticket-office*
  (make-instance 'loc
   :name '("Ticket Office~%~%")
   :fdescription '("You stand outside the local concert hall's "
		   "ticket office. A metal box, about 3 x 2 "
		   "meters big, it is, except for a small glass "
		   "window, completely covered with posters "
		   "depicting recent and upcoming events. "
		   "To the north is the concert hall and in "
		   "the southeast you can see your friends house. ")
   :ldescription '("The ticket office. "
		   "To the north is the concert hall and in "
		   "the southeast you can see your friends house. ")
   :uexit '(("north" *concert-hall*)
	    ("southeast" *friends-house*))
   :flags '(:notseen)
   :things '(*band-poster*)))

(defparameter *band-poster*
  (make-instance 'item
   :name '("poster")
   :synonym '("posters" "poster" "band poster"
	      "band posters")
   :fdescription '()
   :ldescription '()
   :action '((:look-closer-v :look-band-poster-f)
	     (:pick-up-v :take-band-poster-f))))

(defparameter *concert-hall*
  (make-instance 'loc
   :name '("Concert Hall~%~%")
   :fdescription '("This is the town's concert hall. A modern "
		   "looking sort of Bauhaus style building, "
		   "it holds some 8000 people, when fully "
		   "sold out.  Westwards is it's entrance, "
		   "south is the ticket office and to the northeast "
		   "you can see a pedestrian street. To the "
		   "northwest is a small back alley. ") 
   :ldescription '("This is the town's concert hall. "
		   "Westwards is it's entrance, south is the ticket office and "
		   "to the northeast you can see a pedestrian "
		   "street. A small back alley is to the northwest. ")
   :uexit '(("south" *ticket-office*) ("northeast" *pedestrian-street-west*)
	    ("northwest" *back-alley*))
   :nexit '(("west" ("You can get only in there with a valid ticket. "
		      "You don't have one. Ergo, you cannot go there. ")))
   :flags '(:notseen)))

(defparameter *back-alley*
  (make-instance 'loc
   :name '("Concert Hall, Back Alley~%~%")
   :fdescription '("This is a small back alley, belonging to the "
		   "concert hall. To the south-side of the concert hall, "
		   "you can see a  open window. You can get back to the concert "
		   "hall front to the southeast. To the southwest is a staircase "
		   "leading downwards. ")
   :ldescription '("You are in a small back alley of the concert hall. "
		   "To the south-side of the concert hall, "
		   "you can see a  open window. You can get back to the concert"
		   " hall to the southeast and southwestwards is a "
		   "staircase leading downwards. ")
   :uexit '(("southeast" *concert-hall*) ("southwest" *staircase*))
   :nexit '(("south" ("It is a very small window, there's no way "
		       "you would fit through that.")))
   :flags '(:notseen)
   :things '(*rubble*)))

(defparameter *rubble*
  (make-instance 'item
   :name '("Rubble")
   :synonym '("rubble" "pile" "palettes")
   :fdescription '("One one side of the alley is a big pile of, well, "
		   "rubble, discarded wooden palettes, building a huge "
		   "stack of wood. ")
   :sdescription '("Just your ordinary pile of rubble. You kick it around "
		   "a bit, but quickly loose interest. ")
   :action '((:burn-v :burn-rubble-f)
	     (:look-closer-v :look-rubble-f))
   :flags '(:fixed)))

(defparameter *staircase*
  (make-instance 'loc
   :name '("Concert Hall Staircase~%~%")
   :fdescription '("You walk down a stairway ending at a "
		   "massive iron door to your south. "
		   "To the northeast is an alleyway. ")
   :ldescription '("You are on the landing of a stairway "
		   "leading from a back alley northeast to a massive "
		   "iron door to the south ")
   :uexit '(("northeast" *back-alley*))
   :cexit '(("south" *cellar* has-key nil))
   :flags '(:notseen)))

(defparameter *cellar*
  (make-instance 'loc
   :name '("Concert Hall Cellar~%~%")
   :fdescription '("You enter a dimly lit cellar. "
		   "On one side of it are rows of chairs "
		   "stacked up neatly upon another. "
		   "Suddenly you realise a gigantic German Shepherd "
		   "is running towards you. Completely stunned "
		   "you stand there, your heart racing, watching "
		   "the dog approach you very quickly, saliva "
		   "flying from its huge mouth. Boy, do these "
		   "teeth look sharp. As the dog is in leaping "
		   "distance from you, he jumps, hits your chest "
		   "in full stride and knocks you over. "
		   "Instantly, he is on top of you ~%"
		   "... and starts licking your face very excitedly. "
		   "Appearances can be so deceiving.~%"
		   "Finally, you get up, and after some five minutes "
		   "of dog petting, you start looking for a way "
		   "up into the concert area. You find a stairway "
		   "southwestwards and the exit is to the north. ")
   :ldescription '("You are in the concert hall's cellar. "
		   "Northwestwards is an exit leading outside "
		   "and you can see a stairway to the southwest. ")
   :uexit '(("north" *staircase*) ("southwest" *corridor*))
   :things '(*dog*)
   :flags '(:notseen)))

(defparameter *dog*
  (make-instance 'item
   :name '("dog")
   :synonym '("dog" "German shepherd" "shepherd")
   :ldescription '("There is a German Shepherd here, happily "
		   "wagging his tail and looking at you "
		   "with big hopeful eyes. ")
   :sdescription '("It's a big friendly dog with very big ears. ")
   :action '((:pick-up-v :take-dog-f))))

(defparameter *corridor*
  (make-instance 'loc
   :name '("Concert Hall Corridor~%~%")
   :fdescription '("As you ascend the stairs, you find yourself "
		   "in a wide and long corridor, lit by those "
		   "lovely black neon lights. It seems that "
		   "you have made it into the inner sanctum "
		   "of the concert hall, because you can now "
		   "hear the excited buzz of a big crowd to the "
		   "west. There are open doors to the southwest "
		   "and southeast, a locked fire door to the west "
		   "and a fierce looking bouncer is blocking a "
		   "door to the east. Northeastwards is a stairway "
		   "leading into the cellar. ")
   :ldescription '("This is a big corridor with open doors to the "
		   "southwest and southeast, a stairway is to "
		   "the northeast, a locked fire door is to the west "
		   "and a bouncer is blocking a door to your east. ")
   :uexit '(("northeast" *cellar*) ("southwest" *smoking-room*)
	    ("southeast" *toilets*))
   :nexit '(("west" ("What is it about the term \"locked\" "
		      "that you don't understand? ")))
   :cexit '(("east" *vip-area* back-stage-pass-f nil))))

(defparameter *toilets*
  (make-instance 'loc
   :name '("Concert Hall Toilet~%~%")
   :fdescription '("This is one of those typical mass toilets. "
		   "A room, dominated by a huge stainless steel "
		   "trough, two stalls, and two wash basins. "
		   "A fabulous place to meet fellow metal heads, "
		   "exchange your favourite record titles, "
		   "to hear made up stories of encounters with "
		   "the other sex and to take in that ridiculously "
		   "bad stench of urine, stale beer and sweat. "
		   "The exit is to the north. ")
   :ldescription '("The toilet, nothing special to see here, please "
		   "move along west to the exit. ")
   :uexit '(("north" *corridor*))
   :flags '(:notseen)))

(defparameter *smoking-room*
  (make-instance 'loc
   :name '("Concert Hall Smoking Room~%~%")
   :fdescription '("You enter the smoking lounge. By habit, you "
		   "start rolling a cigarette. With great poise "
		   "you lick the paper and light your roll-up. "
		   "Relieved, you take a deep drag, slowly exhale "
		   "while savouring the taste. What a disgusting "
		   "habit.~%Through all the smoke you can make out "
		   "the exit to the north. ")
   :ldescription '("You are in the smoking lounge. The exit is "
		   "to the north. ")
   :things '(*litterbin*)
   :flags '(:notseen)
   :uexit '(("north" *corridor*))))

(defparameter *litterbin*
  (make-instance 'item
   :name '("litterbin")
   :synonym '("litterbin" "bin" "bin" "litter bin")
   :fdescription '("In one corner of the room you can see a "
		   "fancy looking litterbin")
   :action '((:look-closer-v :look-litterbox-f))
   :flags '(:fixed)))

(defparameter *back-stage-pass*
  (make-instance 'item
   :name '("A Backstage Pass ")
   :synonym '("pass" "backstage pass" "card" "laminated card")
   :fdescription '("In one corner of the room, behind the litter bin, "
		   "is some card lying on the floor. ")
   :sdescription '("It says:~%~%BACKSTAGE PASS - Unrestricted Access~%~%"
		   "Oh boy, this is going to be great. ")
   :action '((:look-closer-v :look-back-stage-pass-f)
	     (:pick-up-v :take-pass-f))))


(defparameter *vip-area*
  (make-instance 'loc
   :name '("Concert Hall Vip Area~%~%")
   :fdescription '("Proud like you a little kid, who just got "
		   "his first Heavy Metal record (in the Authors "
		   "case this was Iron Maiden's Killers), "
		   "you show the bouncer your backstage pass. "
		   "With a \"Have a good one, mate\" he opens "
		   "the door and ushers you inside. ~%"
		   "Noticing that you are alone in this room, "
		   "you realise that the show is about to start. "
		   "To the west is a corridor and to the north "
		   "is the backstage area. ")
   :ldescription '("This is the vip room. Westwards is a corridor "
		   "and the backstage area is to the north.")
   :things '(*food*)
   :uexit '(("west" *corridor*))
   :cexit '(("north" *backstage-area* :end-f))))

(defparameter *backstage-area*
  (make-instance 'loc
   :name '("Concert Hall Backstage Area~%~%")
   :fdescription '("At the precise moment you enter "
		   "the backstage area, You hear the opening "
		   "of \"Master of Puppets\" (if you don't know "
		   "the song, it goes like this: don...don don don) "
		   "~%Congratulations, You made it. Well done, "
		   "Sir, or Madam. Please imagine a gigantic "
		   "display of fireworks.~%~%"
		   "THE END~%~%")
   :ldescription '()
   :nexit '(("south" ()))))


(defparameter *food*
  (make-instance 'item
   :name '("food")
   :synonym '("food" "drink" "alcohol")
   :fdescription '("Tables stacked with food and drink line"
		   " one side of the room. ")
   :sdescription '("It's the usual assortment of sandwiches, "
		   "pasta, fish stuff and beverages. ")
   :action '((:pick-up-v :take-food-f))))



(defparameter *pedestrian-street-west*
  (make-instance 'loc
   :name '("Outside the Happy Goose~%~%")
   :fdescription '("You are at the end of your town's High Street. "
		   "People are milling about, doing their shopping "
		   "or just having a walk, while your eyes get "
		   "invariably drawn to a pub in the north, where "
		   "they are already setting up tables outside. "
		   "To the southwest is the concert hall and the "
		   "pedestrian street ends to the east. ")
   :ldescription '("This is a pedestrian road going from east to "
		   "the concert hall in the southwest. "
		   "In the north you can see a pub. ")
   :uexit '(("east" *pedestrian-street-east*) ("southwest" *concert-hall*))
   :cexit '(("north" *pub* :pub-open-v nil))
   :flags '(:notseen)) )

(defparameter *pub*
  (make-instance 'loc
   :name '("The Happy Goose~%~%")
   :fdescription '("You enter the happy goose, a nice and "
		   "clean looking pub, with plenty of benches "
		   "to rest ones troubled feet.~%"
		   "With remarkable determination you walk up "
		   "to the bar and order a pint of Guinness. "
		   "After having paid and waited for the Guinness "
		   "to settle, you lovingly look at the dark-violet "
		   "to black of its body and the bright white foam. "
		   "Slowly, you take a first sip, letting the "
		   "assortment of flavours explode on your tongue. "
		   "Aah, the smokiness, the full body, the slight "
		   "bitterness, oh Mother's Milk, this is wonderful. "
		   "To the south is the exit and to the north are "
		   "the toilets. ")
   :ldescription '("You are inside the happy goose, enjoying your "
		   "pint(s) of Guinness. The toilets are to the north "
		   "and the exit is to the south. ")
   :things '(*ticket-table*)
   :uexit '( ("south" *pedestrian-street-west*))
   :cexit '(("north" *pub-toilets* :pub-quiz-played-f nil))
   :flags '(:notseen)))

(defparameter *pub-toilets*
  (make-instance 'loc
   :name '("The Happy Goose, toilets~%~%")
   :fdescription '("Tiled floor and walls greet you when you "
		   "open the door to the toilets. "
		   "A surprisingly clean affair, it even smells nice. "
		   "There are 2 urinals here, a wash basin and to the "
		   "east is one toilet stall. The exit is to the south. ")
   :ldescription '("You are in a very clean pub bathroom. The exit "
		   "is to the south and to the east is a toilet "
		   "stall. ")
   :uexit '(("south" *pub*) ("east" *toilet-stall*))
   :flags '(:notseen)))

(defparameter *toilet-stall*
  (make-instance 'loc
   :name '("The Happy Goose, toilet stall~%~%")
   :fdescription '("You enter the toilet stall. It's walls are "
		   "made from cheap plywood, however it is as "
		   "clean as the rest of the bathroom. "
		   "The exit is to the west. ")
   :ldescription '("A small toilet stall, with it's exit to the west. ")
   :things '(*cistern* *toilet-brush* *toilet-paper*)
   :flags '(:notseen)
   :uexit '(("west" *pub-toilets*))))

(defparameter *cistern*
  (make-instance 'item
   :name '("cistern")
   :synonym '("cistern")
   :fdescription '("On top of the toilet, is a plastic cistern. ")
   :action '((:look-closer-v :look-cistern-f))
   :flags '(:fixed)))

(defparameter *toilet-brush*
  (make-instance 'item
   :name '("toiletbrush")
   :synonym '("toilet brush" "brush" "toiletbrush")
   :fdescription '("To one side of the toilet is a toilet brush "
		   "mounted in it's holder. ")
   :sdescription '("It is a toilet brush. People use it to clean "
		   "toilets. ")
   :action '((:pick-up-v :take-brush-f))))

(defparameter *toilet-paper*
  (make-instance 'item
   :name '("toiletpaper")
   :synonym '("toilet paper" "toiletpaper" "paper")
   :fdescription '("Sitting on top of the cistern is a roll "
		   "of toilet paper. ")
   :sdescription '("It is a roll of paper. It's white. It's "
		   "useful for wiping your ass. I'm sure you have "
		   "seen one before. ")
   :action '((:pick-up-v :take-paper-f))))

(defparameter *key*
  (make-instance 'item
   :name '("a key ")
   :synonym '("key" "iron key")
   :fdescription '("On one edge of the cistern is a key. ")
   :sdescription '("It looks like any other key. It probably "
		   "opens a door. ")
   :action '((:pick-up-v :take-key-f))))

(defparameter *ticket-table*
  (make-instance 'item
   :name '("Ticket Table")
   :synonym '("table" "ticket counter" "ticket table"
	      "ticket")
   :fdescription '("At the entrance is a small table where you "
		   "can buy a ticket to participate at today's "
		   "Pub Quiz. The Price, you are asking? "
		   "A ticket for tonight's Metallica concert. ")
   :sdescription '("It's a table where you can buy a ticket for
                    today's pub quiz")
   :action '((:buy-v :buy-pub-quiz-ticket-f))
   :flags '(:fixed)))


(defparameter *park-entrance-east*
  (make-instance 'loc
   :name '("park entrance east~%~%")
   :fdescription '("This is the entrance to a beautiful little park. "
		   "A gorgeous English garden with some nice shady "
		   "spots and plenty of benches to rest.")
   :ldescription '("You are at the east entrance of a park.")
   :uexit '(("west" *park-lane-east*) ("east" *housefront*))
   :flags '(:notseen)))

(defparameter *park-lane-east*
  (make-instance 'loc
   :name '("park lane east~%~%")
   :fdescription '("You are on a small footpath in a beautiful park. "
		   "Tall chestnut trees provide a welcome shadow on "
		   "this marvellous day. To the west you can reach "
		   "the centre of this park, to the east leads a "
		   "path towards your house.")
   :ldescription '("you are on a small path in the park. "
		   "East is the park entrance and to the west is the "
		   "centre of this little park. ")
   :uexit '(("west" *park-center*)
	    ("east" *park-entrance-east*))                                   
   :flags '(:notseen)))

(defparameter *park-center*
  (make-instance 'loc
   :name '("park centre~%~%")
   :fdescription '("This is the centrepiece of this municipal "
		   "master piece. A wide English lawn inviting "
		   "you to lie down and have a nap, or to play "
		   "a round of some football.~%"
		   "To the south you can see a little pond and a path "
		   "leads through the park from east to west. ")
   :ldescription '("this is the centre of the little city park. "
		   "To the south you can see a little pond and a path "
		   "leads through the park from east to west. ")
   :uexit '(("south" *pond*)
	    ("east" *park-lane-east* )
	    ("west" *park-lane-west*))
   :things '(*bench*)
   :flags '(:notseen) ))

(defparameter *pond*
  (make-instance 'loc
   :name '("pond~%~%")
   :fdescription '("You are at a tiny pond, holding very clear water, "
		   "so clear in fact, that you can count all its fish. "
		   "You can get back to the centre of this park in the north. ")
   :ldescription '("You stand at a tiny little pond. To the north is the "
		   "park centre. ")
   :uexit '(("north" *park-center* ))
   :things '(*fish*)
   :flags '(:notseen)))


(defparameter *park-lane-west*
  (make-instance 'loc
   :name '("park lane west~%~%")
   :fdescription '("This is the western part of this inner city park. "
		   "Plenty of trees provide a cooling shadow, "
		   "the flowers are in quite remarkable shape and "
		   "the air smells fresh and clean. This all helps in "
		   "rising your spirits, while your mp3 player blasts "
		   "\"Sepulturas' Roots Bloody Roots\". "
		   "In your east is the park centre and to the west "
		   "is a park exit.")
   :ldescription '("You are in the western part of the inner city park. "
		   "This is a small path leading from the centre of "
		   "the park in the east to an park exit to the west.")
   :flags '(:notseen)
   :uexit '(("east" *park-center*)
	    ("west" *park-entrance-west*))))

(defparameter *park-entrance-west*
  (make-instance 'loc
   :name '("park entrance west~%~%")
   :fdescription '("You are at the park's west entrance. "
		   "It's iron doors are open and covered with "
		   "ivy. A footpath leads to the east towards "
		   "the park centre and to the west is the "
		   "concert hall. ")
   :ldescription '("This is the park's west entrance. "
		   "Westwards is the concert hall and "
		   "a footpath leads towards to the park centre. ")
   :uexit '(("west" *concert-hall*)
	    ("east" *park-lane-west*))
   :flags '(:notseen)))

(defparameter *library*
  (make-instance 'loc
   :name '("Library~%~%")
   :fdescription '("This is your towns library, a tall "
		   "rather old looking building. Built "
		   "at the beginning of the 19th century, but "
		   "recently renovated, it looms before you in "
		   "all its glory. To the south you can see its "
		   "huge doors stand invitingly "
		   "open.~%To to west is your best friends house and "
		   "in the east you can see a small off-licence shop.")
   :ldescription '("This is your towns library. Southwards, its wide doors are open ."
		   "To the west is your best friends house and in the "
		   "east you can see a small off-licence shop.")
   :uexit '(("west" *friends-house*) ("east" *off-licence*))
   :nexit '(("south" ("Really? , I don't know whether you have "
		      "understood the premise of this game "
		      "You are supposed to attend the live concert of "
		      "the year!.~%"
		      "This game is called~%~%"
		      "\"Metalhead, the single most fun heavy-metal "
		      "Mystery Romance Text Adventure\".~%~%"
		      "It is not, I repeat, It is not called: ~%"
		      "Book fan, read a book at the local library.~%"
		      "Although, now that I think about it, this sounds "
		      "like a terrific idea for a sequel to this game.")))
   :flags '(:notseen)))

(defparameter *friends-house*
  (make-instance 'loc
   :name '("friends house~%~%")
   :fdescription '("This is the small house your friend bought "
		   "a couple of years ago. Some 30 years old, "
		   "it is a cute, picturesque estate with a tiny "
		   "patch of garden in front of it.~%"
		   "To the east is the library and to the "
		   "west, you can see the ticket office.")
   :ldescription '("This is your friends house. "
		   "To the east is the library and to the "
		   "west is the concert hall ticket office.")
   :uexit '(("east" *library*) ("west" *ticket-office*))
   :things '(*doorbell*)
   :flags '(:notseen)))


(defparameter *friends-hallway*
  (make-instance 'loc
   :name '("hallway~%~%")
   :fdescription '("You enter a lengthy hallway. "
		   "If you had a coat, you could "
		   "hang it on a coat rack next to the "
		   "door, however, as it's August, you "
		   "naturally are not wearing one, and I "
		   "have just wasted 38 words telling you about "
		   "it. While we are talking, the other day, I saw "
		   "a guy, maybe 20 years old, wearing the stupidest "
		   "moustache you could imagine. It was hilariously "
		   "horrible, like he spent a lot of effort and years "
		   "in growing it, so he couldn't admit failure "
		   "and cut the damn thing off. Even if you were a "
		   "moustache aficionado, and I seriously hope you "
		   "are not, you'd advise him to shave it, or better "
		   "still, burn it off. Honestly, it looked like a "
		   "cheap glue-it-on fake moustache, and to add insult "
		   "to injury, it was in no way symmetrical, "
		   "completely crooked, and had the appearance of "
		   "hair coloured weeds.~%Well, enough ranting, "
		   "you are still in a hallway, to the north is the exit "
		   "and Tony is waiting for you to the south in his "
		   "living room. ")
   :ldescription '("A lengthy hallway. To the south is a living room "
		   "and the north is the exit to the street. ")
   :things '(*coatrack*)
   :cexit '(("south" *living-room* bought-beer-v nil))
   :uexit '(("north" *friends-house*))
   :flags '(:notseen)))

(defparameter *living-room*
  (make-instance 'loc
   :name '("Living room~%~%")
   :fdescription '("This is Tony's living room. "
		   "A spacious 20 sqm, dominated by a huge "
		   "flatscreen mounted on the wall to the east. "
		   "You sit down on a big couch, nearly drowning "
		   "in a rather unmanly amount of pillows. ")
   :ldescription '("Tony's living room. A big couch and a big "
		   "telly. To the north is the hallway. ")
   :things '(*tony*)
   :uexit '(("north" *friends-hallway*))
   :flags '(:notseen)))

(defparameter *tony*
  (make-instance 'item
   :name '("Tony")
   :synonym '("Tony" "best friend" "friend")
   :fdescription '("Tony is sitting on the couch and looking at "
		   "you expectantly. ")
   :sdescription '("It's you best friend Tony. Sporting a t-shirt "
		   "showing Maiden's Powerslave cover, and looking "
		   "thirsty. ")
   :flags '(:fixed)
   :action '(:talk-to-v :talk-to-tony-f)))


(defparameter *laptop*
  (make-instance 'item 
   :name '("laptop")
   :synonym '("notebook" "laptop" "computer" "email")
   :fdescription '("On a table near the exit to the west is a laptop. ")
   :sdescription '("your old sturdy laptop. Not the latest and shiniest "
		   "but money is very expensive so you still "
		   "make do with it." )
   :action '((:use-v  :use-laptop-f)
	     (:start-v :power-on-laptop-f)
	     (:pick-up-v :take-laptop-f)
	     (:read-v :read-email-f))                        
    :flags '(:poweroff)))

(defparameter *clothes*
  (make-instance 'item
   :name '("your clothes")
   :synonym '("clothes")
   :fdescription '("Strewn all over the floor are your clothes. ")
   :sdescription '("jeans and a t-shirt. nothing fancy.")
   :action '((:wear-v :put-on-clothes :pick-up-v :take-clothes-f))
   :flags '(:notwearing)))

(defparameter *poster*
  (make-instance 'item
   :name '("a poster")
   :synonym '("poster")
   :fdescription '("On the wall you can see an old poster.")
   :sdescription '("It is a very old nearly completely faded poster."
		   " You can only make out a painted scene of rows "
		   "of white crosses in a field.")
   :flags '(:fixed)
   :action '((:look-closer-v :describe-poster-f))))

(defparameter *fish*
   (make-instance 'item
   :name '("a healthy rainbow trout ")
   :synonym '("trout" "fish")
   :fdescription '("There is one big trout in the pond.")
   :sdescription '("you are looking at a very healthy and "
		   "most probably good tasting rainbow trout.")
   :flags '(("taken" 0))
   :action '((:pick-up-v :pick-up-trout-f))))
 
(defparameter *bench*
  (make-instance 'item
   :name '("ridiculously comfortable looking bench.")
   :fdescription '("You can see a ridiculously comfortable looking "
		   "bench here.")
   :ldescription '("there is a very comfortable looking bench here. ")
   :sdescription '("As you examine the bench you notice that there's "
	           "something scratched into the wood.")
   :synonym '("comfortable" "cosy" "comfy" "inviting" "bench" "park bench")
   :flags '((:fixed))
   :action '((:look-closer-v :read-inscription-f)
	     (:sit-down-v :sit-on-bench-f))))

(defparameter *doorbell*
  (make-instance 'item
   :name '("brass doorbell")
   :synonym '("doorbell" "brass doorbell" "bell")
   :fdescription '("You can see a big brass doorbell here.")
   :sdescription '("One of those doorbells. You press it and "
		   "via some magic it makes a sound. ")
   :flags '(:fixed)
   :action'((:use-v :press-doorbell-f)
	    (:look-closer-v :look-doorbell-f))))

(defparameter *coatrack*
  (make-instance 'item
   :name '("coatrack")
   :synonym '("coatrack" "rack")
   :flags '(:fixed)))





(defparameter *player*  (make-instance 'player :location *bedroom*
		 :inventory '()))




 
