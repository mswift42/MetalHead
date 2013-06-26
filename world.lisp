(load "~/quicklisp/setup.lisp")
(ql:quickload "clunit")

(defpackage :world
  (:use :cl :clunit))

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


(defparameter *bedroom*
  (make-room
   :ldescription '(the bedroom. Very messy. Very tiny. There is a big poster
		   on the wall. Your clothes are strewn all over the floor.
		   Near the exit to the west is your laptop.)
   :sdescription '(you are in your bedroom. You should seriously think
		   about cleaning it up.)
   :uexit '(( west hallway))
   
   :nexit '(east (did you seriously think about leaving by the window?
		  I know you had a rough night but please use the door
		  like other normal people.))
   :things '(*laptop* *clothes*)))


(defparameter *hallway*
  (make-room
   :ldescription '(the hallway. A narrow thing leading from your bedroom
		   to the east to your frontdoor leading into town to the
		   west.)
   :uexit '((east bedroom)
	    (west frontdoor))))

(defparameter *laptop*
  (make-item 
   :name '(a laptop)
   :synonym '(notebook laptop computer )
   :fdescription '(on a table near the exit to the west is a laptop.)
   :ldescription '(your old sturdy laptop. Not the latest and shiniest
		   model but money is very expensive so you still
		   make do with it.)
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

(defun non-exits (room)
  (first ( rest (room-nexit room))))

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

(defparameter *allowed-commands* '(look go take move get pick))

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

(defun describe-room (room)
  (loop for i in (room-ldescription room)
        do (format t "~A " i))
  )

(defun items-in-room (room)
  "Return all items in a location."
  (room-things room))


(clunit:defsuite Room-suite ())
(clunit:defsuite Parse-suite ())

(clunit:deftest test-non-exits (Room-suite)
  (clunit:assert-equal '(did you seriously think about leaving by the window?
			 I know you had a rough night but please use the door
			 like other normal people.) (non-exits *bedroom*)))

(clunit:deftest test-u-exits (Room-suite)
  (clunit:assert-equal '((west hallway)) (u-exits *bedroom*))
  (clunit:assert-equal '((east bedroom) (west frontdoor)) (u-exits *hallway*)))

(deftest test-items-in-room (Room-suite)
  (clunit:assert-equal '(*laptop* *clothes*) (items-in-room *bedroom*)))

(deftest test-describe-list-of-items-in-location (Room-suite)
  (clunit:assert-equal '((ON A TABLE NEAR THE EXIT TO THE WEST IS A LAPTOP.) (STREWN ALL OVER THE
 FLOOR ARE YOUR CLOTHES.)) (describe-list-of-items-in-location *bedroom*)))

(clunit:deftest test-return-synonym (Parse-suite)
  (clunit:assert-equal 'start-v (return-synonym 'power))
  (clunit:assert-equal 'use-v (return-synonym 'use)))



(clunit:run-suite 'Room-suite)
(clunit:run-suite 'Parse-suite)
