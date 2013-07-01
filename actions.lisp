(load "world.lisp")
(load "util.lisp")

(defpackage :action
  (:use :cl  :world :utilities))



(in-package :action)

(defun read-direction (input)
  "look up entered direction in directions-synonyms and directions.
   If synonym return full name. If full name entered return it."
  (cond
    ((member input *directions*) input)
    ((assoc input *directions-synonyms*) (second (assoc input *directions-synonyms*)))
    (t nil)))

(defun power-on-laptop-f ()
  (setf (item-flags *laptop*
		    ) '(poweron))
  "You press the power button. You hear some funny noises, and it actually starts booting.
   One Cup of Tee later, and you start at the login screen. I hope you haven't forgotten
   the password.")

(defun use-laptop-f ()
  (if (equal 'poweroff (first (item-flags *laptop*)))
      "Your laptop is turned off"
      "you could browse your favorite websites all day, you good old procrastinator, however
       I'd propose you simply check your Email."))


(defun take-laptop-f ()
  "You cannot take it. It's too heavy, the battery is not working and it's highly unlikely 
   that it would survive any form of transport.")


(defun wear-clothes ()
  "if not wearing clothes, print out text . Else change location to hallway."
  (if (eq (symbol-value (item-flags *clothes*)) :notwearing)
      '(you are not wearing any clothes. I am terribly sorry but you should not inflict
	your gross naked body on other people. There are plenty beautiful sights in this
	world. You are not one of them.
	When God made you he was either drunk or bored. Maybe he was just spiteful
	but for Fuck Sake please put on some clothes.)
      (progn
	(setf *location* *hallway*)
	(describe-room *hallway*))))

(defun put-on-clothes ()
  (princ'(with the grace of a young gazelle you put on your clothes. Within seconds your appearance
	  changes from ugly as hell to well below average handsome. Well done.))
  (setf (item-flags *clothes*) :wearing))

(defun describe-poster ()
  (item-sdescription *poster*))

(defun object-action-list (itemlist)
  "Return a list of all possible actions of all items
   for one location. (Helper Function for actions-for-location."
  (cond
    ((null itemlist) nil)
    (t (append (item-action (symbol-value (first itemlist)))
	       (object-action-list (rest itemlist))))))

(defun actions-for-location ()
  "Return alist for possible actions in the present location."
  (object-action-list (room-things (symbol-value *location*))))


(defun read-directions (room)
  "Return a list of all possible directions in a location."
  (append (room-uexit room) (room-cexit room) (room-nexit room)))


(defun cexit-read-condition (direction)
  "return predicate necessary to use conditional exit."
  (third (assoc direction (room-cexit (symbol-value *location*)))))

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
      ((and ( cexit-read-condition direction)) (funcall ( cexit-read-condition direction)))
      ((uexits-next-location direction ue) (uexits-next-location direction ue))
      ((nexit-next-location direction ne) (nexit-next-location direction ne))        
      (t nil))))

(defun change-location ( direction)
  "When changing locations, set global-variable *location* to new location.
   Describe room either with first or later description."
  (setf *location* (walk-direction direction (symbol-value *location*)))
  (describe-room  (symbol-value *location*)))

(defun describe-list-of-items-in-location (room)
  "Return list of descriptions of all items in a room."
  (mapcar #'(lambda (x) (item-fdescription (symbol-value x))) (room-things room))) 

(defun describe-list-of-items-in-location-later (room)
  "Return the ldescription of all itemns in a room."
  (mapcar #'(lambda (x) (item-ldescription (symbol-value x))) (room-things room)))

(defun describe-room ( room)
  "Use lol's game-print function to print first the description of the
   room you are in, then describe all items in the location."
  (if (eq (symbol-value (room-flags room)) :notseen)
      (progn
	(game-print (room-fdescription room))
	(game-print (flatten ( describe-list-of-items-in-location room)))
	
	(setf ( room-flags room) :seen))
      (progn
	(game-print (room-ldescription room))
	(game-print (flatten (describe-list-of-items-in-location-later room))))))

(defun items-in-room (room)
  "Return all items in a location."
  (room-things room))

(defun u-exits (room)
  (slot-value room 'uexit))



(let ((pack (find-package :world)))
  (do-all-symbols (sym pack) (when (eql (symbol-package sym) pack) (export sym))))
