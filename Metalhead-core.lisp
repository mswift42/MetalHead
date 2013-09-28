
(load "MetalHead.asd")
(asdf:oos 'asdf:load-op :metalhead)
(defun main ()
  (load "gui.lisp"))
