(load "runner.lisp")
(in-package :kallup)

(defun kallup::setup-controls()
    (print "121212"))

;; -----------------------------------------------
;; this is the main object for our application ...
;; -----------------------------------------------
(defvar *myapp* (make-instance
    'application-gui
    :name "MyQt5Application"))
(print (exec *myapp*))

(main)
(sb-ext:quit)

