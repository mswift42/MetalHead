;;;; MetalHead.asd

(asdf:defsystem #:metalhead
  :serial t
  :description "Describe MetalHead here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:fiveam
               #:ltk)
  :components ((:file "package")
               (:file "util")
	       (:file "world")
	       (:file "actions")
	       (:file "gui" )
	       (:file "tests")))

