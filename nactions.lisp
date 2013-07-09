(load "world.lisp")

(defpackage :nactions
  (:use :cl )
  (:import-from :world *bedroom* *laptop* *hallway* *poster* *clothes*
		*location* 
		room-things room-nexit room-uexit room-cexit room-flags
		room-ldescription room-fdescription item-flags item-name
		item-fdescription item-sdescription describe-room))

(in-package :nactions)



(defun read-directions (room)
  "Return a list of all possible directions in a location."
  (append (room-uexit room) (room-cexit room) (room-nexit room)))


(defun u-exits (room)
  (slot-value room 'uexit))

(defun use-laptop-f ()
  (if (equal 'poweroff (first (item-flags *laptop*)))
      "Your laptop is turned off"
      "you could browse your favorite websites all day, you good old 
       procrastinator, however I'd propose you simply check your Email."))
(defun power-on-laptop-f ()
  (setf (item-flags *laptop*
		    ) '(poweron))
  "You press the power button. You hear some funny noises, and it actually 
   starts booting. One Cup of Tee later, and you start at the login 
   screen. I hope you haven't forgotten the password.")

(defun wear-clothes ()
  "if not wearing clothes, print out text . Else change location to hallway."
  (if (eq (symbol-value (item-flags *clothes*)) :notwearing)
      '(you are not wearing any clothes. I am terribly sorry but you should
	not inflict your gross naked body on other people. There
	are plenty beautiful sights in this
	world. You are not one of them.
	When God made you he was either drunk or bored.
	Maybe he was just spiteful
	but for Fuck Sake please put on some clothes.)
      (progn
	(setf *location* *hallway*)
	(describe-room *hallway*))))

(defun put-on-clothes ()
  (princ'(with the grace of a young gazelle you put on your clothes. Within
	  seconds your appearance changes from ugly as hell to well
	  below average handsome. Well done.))
  (setf (item-flags *clothes*) :wearing))


(deftest test-walk-direction (Room-suite)
  (clunit:assert-equal '*bedroom* (walk-direction 'east *hallway*)))



