
(in-package #:metalhead)

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
  (is (equal '("On a table near the exit to the west is a laptop. "
	       "Strewn all over the floor are your clothes. "
	       "On the wall you can see an old poster.")
	     (describe-list-of-items-in-location *bedroom*))))

(test test-exit-lst
  (is (equal '(CE "west" *HALLWAY* WEAR-CLOTHES)
	     (exit-lst *bedroom* "west")))
  (is (equal '(UE "east" *bedroom*)
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
	     (look-command-p '("study" "poster"))))
  (is-true (look-command-p '("look" "at" "exit")))
  (is-true (look-command-p '("look" "at" "floor"))))

(test test-is-action-p
  (is (eq :wear-v (is-action-p '("put" "on" "clothes"))))
  (is (eq :wear-v (is-action-p '("wear" "clothes")))))

(test test-inventory-p
  (is-true (inventory-p "inVenTory")))

(test test-answer-for-question
  (is (equal '("Di'Anno" "Di Anno" "DiAnno" "Paul Di'Anno" "Paul Di Anno")
	     (answer-for-question "Who was the predecessor of Bruce Dickinson as frontman of Iron Maiden"))))

(test test-correct-answer-p
  (is-true (correct-answer-p "Who was the predecessor of Bruce Dickinson as frontman of Iron Maiden" "Di'Anno"))
  (is-true (correct-answer-p "Who was the predecessor of Bruce Dickinson as frontman of Iron Maiden" "di'anno"))
  (is-true (correct-answer-p "Who was the predecessor of Bruce Dickinson as frontman of Iron Maiden" "diAnNo"))
  (is-true (correct-answer-p "Name of a swedish Band that got it's name from a volcano in Tolkien's Lord of the Rings." "amOn AMArTh"))
  (is-true (correct-answer-p "Tom Angelripper was founding member of which German Band?" "soDOM")))






