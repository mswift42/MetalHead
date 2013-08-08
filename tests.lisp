(load "actions.lisp")
(ql:quickload "fiveam")

(defpackage :tests
  (:use :cl :fiveam :actions :world))

(in-package :tests)

(test test-u-exits 
  (is (equal '(("east" *bedroom*) ("west" *housefront*))
	     (:uexit *hallway*))))


(test test-is-direction-p
  (is-true (is-direction-p '("go" "north")))
  (is-true (is-direction-p '("west")))
  (is-true (is-direction-p '("move" "se")))
  (is-false (is-direction-p '("eat" "salad"))))

(test test-is-look-p
  (is-true (is-look-p "EXAMINE"))
  (is-true (is-look-p "StuDy")))


(test test-describe-list-of-items-in-location                      
  (is (equal '("on a table near the exit to the west is a laptop."
	       "strewn all over the floor are your clothes."
	       "On the wall you can see an old poster.")
	     (describe-list-of-items-in-location *bedroom*))))

(test test-exit-lst
  (is (equal '(actions::CE "west" *HALLWAY* WORLD::WEAR-CLOTHES)
	     (exit-lst *bedroom* "west")))
  (is (equal '(actions::UE "east" *bedroom*)
	     (exit-lst *hallway* "east"))))

(test test-return-synonym
  (is (equal :start-v (return-synonym 'power)))
  (is (equal  :use-v (return-synonym 'use))))

(test test-read-direction
  (is (equal "up" (read-direction "u")))
  (is (equal "west" (read-direction "west")))
  (is (equal "northeast" (read-direction "ne"))))


(test test-print-list
  (is-true (typep (print-list '("hello" " you" " yes" " you")) 'string)))

(test test-find-synonym
  (is-true (find-synonym *laptop* "notebook"))
  (is-true (find-synonym *laptop* "LAPTOP")))

(test test-find-synonym-in-location
  (is (equal '("a poster") (:name (find-synonym-in-location "poster"))))
  (is (equal '("laptop") (:name (find-synonym-in-location "laptop")))))

(test test-look-command
  (is (equal '("your old sturdy laptop. Not the latest and shiniest "
 "but money is very expensive so you still " "make do with it.")
	     (look-command-p '("examine" "laptop"))))
  (is (equal '("It is a very old nearly completely faded poster."
 " You can only make out a painted scene of rows "
 "of white crosses in a field.")
	     (look-command-p '("study" "poster")))))

(test test-is-action-p
  (is (eq :wear-v (is-action-p '("put" "on" "clothes"))))
  (is (eq :wear-v (is-action-p '("wear" "clothes")))))


(fiveam:run!)

