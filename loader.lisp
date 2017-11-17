(load "runner.lisp")
(in-package :kallup)

(defun setup-controls()
(let ((menu1  (make-menu '("Datei"   "F10"   )))
      (menu1A (make-menu '("Öffnen"  "Alt+O" )))
      (menu1B (make-menu '("--"      ""      )))
      (menu1C (make-menu '("Beenden" "Alt+X" ))))
  (add-menu menu1 menu1A)
  (add-menu menu1 menu1B)
  (add-menu menu1 menu1C)
(traverse menu1)menu1)(terpri))

;; -----------------------------------------------
;; this is the main object for our application ...
;; -----------------------------------------------
(defvar *myapp* (make-instance
    'application-gui
    :name "MyQt5Application"))
(init *myapp*)(setup-controls)
(exec *myapp*)

(main)
(sb-ext:quit)

