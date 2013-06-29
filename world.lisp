(load "~/quicklisp/setup.lisp")
(ql:quickload "clunit")
(load "util.lisp")

(defpackage :world
  (:use :cl :clunit :utilities))

(in-package :world)


(defstruct (room)
  "Holds a initial and a later descriptions for locations.
   Unconditional exits, conditional exits (a door you need a key for)
   non-exits (paths that lead nowhere but warrant a non default explanation
   things - describable items in a location"
  (name '())
  (fdescription '())
  (ldescription '())
  (sdescription '())
  (uexit '())
  (nexit '())
  (cexit '())
  (things '()))

(defstruct (item)
  "Holds inventory items in the game."
  (name '())
  (synonym '())
  (fdescription '())
  (ldescription '())
  (sdescription '())
  (location '())
  (action '())
  (flags '()))

(defparameter *location* *bedroom*)

(defparameter *inventory* '())


(defparameter *bedroom*
  (make-room
   :fdescription '(the bedroom. Very messy. Very tiny.)
   :ldescription '(you are in your bedroom. You should seriously think
		   about cleaning it up.)
   :uexit '(( west  *hallway*))
   
   :nexit '(( east (did you seriously think about leaving by the window?
		    I know you had a rough night but please use the door
		    like other normal people.)))
   :things '(*laptop* *clothes* *poster*)))


(defparameter *hallway*
  (make-room
   :ldescription '(the hallway. A narrow thing leading from your bedroom
		   to the east to your frontdoor leading into town to the
		   west.)
   :uexit '((east *bedroom*)
	    (west *frontdoor*))))


(defparameter *frontdoor*
  (make-room
   :ldescription '(You leave your house and find yourself at an absolutely
		   marvellous spring day. It is warm sunny and the birds are singing.
		   Its exactly the sort of day that makes nearly everyone happy.)
   :sdescription '(you stand outside of your house.)
   :uexit '((east *hallway*) (west *park-entrance*) (nw *main-road*))))

(defparameter *park-entrance-east*
  (make-room
   :fdescription '(This is the entrance to a beautiful little park. A gorgeous
		   english garden with some nice shady spots and plenty of
		   benches to rest.)
   :ldescription '(You are at the east entrance of a park.)
   :uexit '((west *frontdoor*) (east *park-lane-east*))))

(defparameter *park-lane-east*
  (make-room
   :fdescription '(You are in the town park. There is a path leading from east to west.)))



(defparameter *laptop*
  (make-item 
   :name '(a laptop)
   :synonym '(notebook laptop computer )
   :fdescription '(on a table near the exit to the west is a laptop.)
   :ldescription '(your old sturdy laptop. Not the latest and shiniest
		   but money is very expensive so you still
		   make do with it. )
   :sdescription '(your laptop. It used to be black.
		   Whats the color of grime again?)
   :location '(*bedroom*)
   :action '((use-v  use-laptop)
	     (start-v power-on-laptop) (type-pass-v crack-password-p))
   :flags '(poweroff )))

(defparameter *clothes*
  (make-item
   :name '(your clothes)
   :fdescription '(strewn all over the floor are your clothes.)
   :ldescription '(jeans and a t-shirt. nothing fancy.)
   :location '(*bedroom*)
   :action '((wear-v put-on-clothes))
   :flags '(not-wearing)))

(defparameter *poster*
  (make-item
   :name '(a poster)
   :fdescription '(On the wall you can see an old poster.)
   :sdescription '(It is a very old nearly completely faded poster. You can only
		   make out a painted scene of rows of white crosses in a field.)
   :ldescription '(Oh you joyful Master of Puppets. You mother of all metal records.)
   :location '(*bedroom*)
   :action '((look-closer-v describe-poster-f))))





(defun u-exits (room)
  (slot-value room 'uexit))

(defun use-laptop-f ()
  (if (equal 'poweroff (first (item-flags *laptop*)))
      "Your laptop is turned off"
      "you could browse your favorite websites all day, you good old procrastinator, however
       I'd propose you simply check your Email."))

(defun power-on-laptop-f ()
  (setf (item-flags *laptop*
		    ) '(poweron))
  "You press the power button. You hear some funny noises, and it actually starts booting.
   One Cup of Tee later, and you start at the login screen. I hope you haven't forgotten
   the password.")


(defun take-laptop-f ()
  "You cannot take it. It's too heavy, the battery is not working and it's highly unlikely 
   that it would survive any form of transport.")

(defun describe-poster ()
  (item-sdescription *poster*))

(defparameter verb-synonyms
  '((use use-v)
    (utilize use-v)
    (start start-v)
    (power start-v))
  "association list to lookup the fitting functions in an object to its verb")

(defun return-synonym (verb)
  (first ( rest (assoc verb verb-synonyms))))


(defun read-directions (room)
  "Return a list of all possible directions in a location."
  (append (room-uexit room) (room-cexit room) (room-nexit room)))

(defun uexits-next-location (direction uexit-lst)
  "Takes a direction and the list of uexits in a location.
   Returns either the next room if the desired direction is 
   a member of uexits-lst or nil."
  (cond
    ((null uexit-lst) nil)
    ((member direction (first uexit-lst)) (second (first uexit-lst)))
    (t (uexits-next-location direction (rest uexit-lst)))))

(defun nexit-next-location (direction nexit-lst)
  "return possible nexit of direction in a location."
  (cond
    ((null nexit-lst) nil)
    ((member direction (first nexit-lst)) (second (first nexit-lst)))
    (t (nexit-next-location direction (rest nexit-lst)))))

(defun walk-direction (direction room)
  "Return next location of a entered direction in a location."
  (let ((ue (room-uexit room))
	(ne (room-nexit room)))
    (cond
      ((uexits-next-location direction ue) (uexits-next-location direction ue))
      ((nexit-next-location direction ne) (nexit-next-location direction ne))
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

(defparameter *allowed-commands* '(look go take move get pick start))

(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
      '(I do not know this command.)))

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



(defun describe-list-of-items-in-location (room)
  "Return list of descriptions of all items in a room."
  (mapcar #'(lambda (x) (item-fdescription (symbol-value x))) (room-things room))) 

(defun describe-list-of-items-in-location-later (room)
  "Return the ldescription of all itemns in a room."
  (mapcar #'(lambda (x) (item-ldescription (symbol-value x))) (room-things room)))

(defun describe-room (time room)
  "Use lol's game-print function to print first the description of the
   room you are in, then describe all items in the location."
  (if (equal time 'initial)
      (progn
	(game-print (room-fdescription room))
	(game-print (flatten ( describe-list-of-items-in-location room))))
      (progn
	(game-print (room-ldescription room))
	(game-print (flatten (describe-list-of-items-in-location-later room))))))

(defun items-in-room (room)
  "Return all items in a location."
  (room-things room))


(clunit:defsuite Room-suite ())
(clunit:defsuite Parse-suite ())


(clunit:deftest test-u-exits (Room-suite)
  (clunit:assert-equal '((west *hallway*)) (u-exits *bedroom*))
  (clunit:assert-equal '((east *bedroom*) (west *frontdoor*)) (u-exits *hallway*)))

(deftest test-items-in-room (Room-suite)
  (clunit:assert-equal '(*laptop* *clothes* *poster*) (items-in-room *bedroom*)))

(deftest test-uexits-next-location (Room-suite)
  (clunit:assert-equal '*bedroom* (uexits-next-location 'east (room-uexit *hallway*))))

(deftest test-describe-list-of-items-in-location (Room-suite)
  (clunit:assert-equal '((ON A TABLE NEAR THE EXIT TO THE WEST IS A LAPTOP.) (STREWN ALL OVER THE FLOOR ARE YOUR CLOTHES.) (ON THE WALL YOU CAN SEE AN OLD POSTER.)) (describe-list-of-items-in-location *bedroom*)))

(deftest test-walk-direction (Room-suite)
  (clunit:assert-equal '*hallway* (walk-direction 'west *bedroom*)))

(clunit:deftest test-return-synonym (Parse-suite)
  (clunit:assert-equal 'start-v (return-synonym 'power))
  (clunit:assert-equal 'use-v (return-synonym 'use)))



(clunit:run-suite 'Room-suite)
(clunit:run-suite 'Parse-suite)
