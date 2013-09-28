**MetalHead**
=============

The single most exciting and fun Heavy-Metal Mystery Romance Text Adventure.
----------------------------------------------------------------------------


Being made for  [LispinSummerProjects](http://lispinsummerprojects.org/).


Synopsis
--------

The world's best live band is playing in your town and you need to be there. Unfortunately it is sold out, but you can't let that stop you.


Installation Instructions
-------------------------

    install sbcl
	git clone https://github.com/mswift42/MetalHead
	cd MetalHead
	sbcl --load save-core.lisp
	./metalhead

If you don't want to install sbcl and you have already installed a lisp implementation, you can run the game by changing the directory to MetalHead, run your lisp implementation, (I have tested this with sbcl and ccl) and:

    (load "MetalHead.asd")
	(ql:quickload "metalhead")

How to play
-----------

This is a classical text adventure, you need to enter the commands what the player is supposed to do. If you type 'help' you can read the in-game help screen.


TODO
----

Add more questions to pub-quiz.








