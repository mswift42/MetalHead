(ql:quickload "clunit")
(load "world.lisp")
(load "actions.lisp")



(defpackage :tests
  (:use :cl :clunit :world :action))

(in-package :tests)

(clunit:defsuite Room-suite ())
(clunit:defsuite Parse-suite ())


(clunit:deftest test-u-exits (Room-suite)
  (clunit:assert-equal '((east *bedroom*) (west *frontdoor*)) (u-exits *hallway*)))

(deftest test-items-in-room (Room-suite)
  (clunit:assert-equal '(*laptop* *clothes* *poster*) (action::items-in-room *bedroom*)))

(deftest test-uexits-next-location (Room-suite)
  (clunit:assert-equal '*bedroom* (action::uexits-next-location 'east (room-uexit *hallway*))))

(deftest test-cexit-read-condition (Room-suite)
  (clunit:assert-equal 'wear-clothes (action::cexit-read-condition 'west)))

(deftest test-describe-list-of-items-in-location (Room-suite)                         
  (clunit:assert-equal '((ON A TABLE NEAR THE EXIT TO THE WEST IS A LAPTOP.)
			 (STREWN ALL OVER THE FLOOR ARE YOUR CLOTHES.)
			 (ON THE WALL YOU CAN SEE AN OLD POSTER.))
      (action::describe-list-of-items-in-location *bedroom*)))

(deftest test-walk-direction (Room-suite)
  (clunit:assert-equal '*bedroom* (walk-direction 'east *hallway*)))

(clunit:deftest test-return-synonym (Parse-suite)
  (clunit:assert-equal 'start-v (return-synonym 'power))
  (clunit:assert-equal 'use-v (return-synonym 'use)))

(clunit:deftest test-read-direction (Parse-suite)
  (clunit:assert-equal 'up (read-direction 'u))
  (clunit:assert-equal 'west (read-direction 'west))
  (clunit:assert-equal 'northeast (read-direction 'ne)))

(clunit:run-suite 'Room-suite)
(clunit:run-suite 'Parse-suite)

