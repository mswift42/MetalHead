
(ql:quickload "clunit")

(defpackage :world
  (:use :cl ))

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
  (flags '())
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

(defparameter *location*
  '*bedroom*
  "location of the player character in the game world.")

(defparameter *inventory* '())

(defparameter  *directions-synonyms*
    '((e  east) (w  west) (s  south) (n  north) (d  down)
    (u  up) (se  southeast) (sw  southwest) (ne  northeast) (nw  northwest))
  "alist for abbreviations of directions.")

(defparameter *directions*
  '(east west south north down up southeast southwest northeast northwest))

(defparameter *bedroom*
  (make-room
   :fdescription '(the bedroom. Very messy. Very tiny.)
   :ldescription '(you are in your bedroom. You should seriously think
		   about cleaning it up.)
   :cexit '(( west  *hallway* wear-clothes))
   
   :nexit '(( east (did you seriously think about leaving by the window?
		    I know you had a rough night but please use the door
		    like other normal people.)))
   :things '(*laptop* *clothes* *poster*)
   :flags :notseen))


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
   :action '((use-v  use-laptop-f)
	     (start-v power-on-laptop-f) (type-pass-v crack-password-p))
   :flags '(poweroff notseen)))

(defparameter *clothes*
  (make-item
   :name '(your clothes)
   :fdescription '(strewn all over the floor are your clothes.)
   :ldescription '(jeans and a t-shirt. nothing fancy.)
   :location '(*bedroom*)
   :action '((wear-v put-on-clothes))
   :flags :notwearing))

(defparameter *poster*
  (make-item
   :name '(a poster)
   :fdescription '(On the wall you can see an old poster.)
   :sdescription '(It is a very old nearly completely faded poster. You can only
		   make out a painted scene of rows of white crosses in a field.)
   :ldescription '(Oh you joyful Master of Puppets. You mother of all metal records.)
   :location '(*bedroom*)
   :action '((look-closer-v describe-poster-f))))                                                       

(defparameter verb-synonyms
  '((use use-v)
    (utilize use-v)
    (start start-v)
    (power start-v))
  "association list to lookup the fitting functions in an object to its verb")

(defun return-synonym (verb)
  (first ( rest (assoc verb verb-synonyms)))) 

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

(defparameter *allowed-commands* '( use-laptop-f))

(defun game-eval (sexp)
  (if (member sexp *allowed-commands*)
      (funcall sexp)
      '(I do not know this command.)))

(defun game-reader (exp)
  "Evaluate player input"
  (cond
    ((walk-direction exp (symbol-value *location*)) (change-location  exp))
    ((assoc exp (actions-for-location)) (funcall (second (assoc exp (actions-for-location)))))
    (t nil)))

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




;; ;;; This snippet is taken from Stackoverflow: it exports all symbols in package :world
;; ;;; i.e. I can access the fields of room and item from  other packages like I do in this package.
;; (let ((pack (find-package :world)))
;;   (do-all-symbols (sym pack) (when (eql (symbol-package sym) pack) (export sym))))
