;; A little toy to test Lisp if it strong enough to
;; simulate a electricaly system.
;;
;; (c) 2017 by Jens Kallup
;;
;; credits to: clnoobs.irc.freenode folk

(require 'cffi)
(defpackage :kallup
    (:use :cl :cffi))

(in-package :kallup)
(define-foreign-library libkallup
       (:unix    "./kallup/libkallup.so")
    (t (:default "./kallup/libkallup.so")))

(load-foreign-library 'libkallup)

;; Funktionen
(defcfun ("kallup_init_app"    kallup-init-app)    :pointer)
(defcfun ("kallup_init_window" kallup-init-window) :pointer)
(defcfun ("kallup_exec_app"    kallup-exec-app)    :int    (ptr :pointer))

(defcvar ("kallup_application_id" *application-id*) :pointer)
(defcvar ("kallup_main_window"    *application-id-window*) :pointer)

;; ----------------------------------------------------------------
;; SuperApplicationClass
;;
;; Variabeln
(defvar *application-id-return* 0)
(funcall #'setup-controls)

(defclass application()
    ((name :initarg :name :initform (error "ApplikationName muss angegeben werden."))
     (args :initarg :land :initform "init1 init2")))
;;
(defclass application-gui(application)
    ((window :initarg :window :initform *application-id-window*)
     (pid    :initarg :pid    :initform *application-id*)))
;;
;; run/exec application
(defgeneric exec (app))
(defmethod  exec ((app application-gui))
    (slot-value app 'pid)
    (progn (setq *application-id* (kallup-init-app))
           (setq *application-id-window* (kallup-init-window)) (setup-controls)
           (setq *application-id-return* (kallup-exec-app *application-id*))))

;; ----------------------------------------------------------------
;; SuperDeviceClass = Europa
(defclass device-europa()
    ((name :initarg :name :initform (error "Elementname muss angegeben werden."))
     (land :initarg :land :initform :europa)))
;;
(defclass power-source-device(device-europa)
    ((plus-input  :initarg :plus-input  :initform 220)
     (minus-input :initarg :minus-input :initform 220)))
;;
;; power source
(defparameter *power-source*
    (make-instance
        'power-source-device
        :name        :quelle-1
        :land        :europa
        :plus-input  220
        :minus-input 220))

;; default variables, that are not declared automaticaly
;; in sbcl ...
(defvar out-state 0)

;; ------------------------------------------------------
;; our top level object: the electrical board part(s) ...
;; ------------------------------------------------------
(defun make-board (item)
    (cons (cons item nil) nil))

;; -----------------------------------------------------------------------

;; -------------------------------
;; decimal to hex converter ...
;; -------------------------------
(defun dec2hex ( num bitlen )
    (print (format t "0x~(~D~,'0x~)" bitlen num))
)

;; --------------------------------
;; a simple power source ...
;; --------------------------------
(defun power-source ( inp inm )
    (if (= inp 1)
        (if (= inm 1)
            (setq out-state 1)
            (setq out-state 0)
        )
    )
    (if (= inp 0)
        (if (= inm 1)
            (setq out-state 0)
            (setq out-state 0)
        )
    )
    (print out-state)
)

;; --------------------------------
;; a simple AND circuit ...
;; --------------------------------
(defun and-circuit ( inp inm )
(let (out-state)
    (if (= inp 0)
        (if (= inm 0)
            (setq out-state 0)
            (setq out-state 0)
        )
    )
    (if (= inp 1)
        (if (= inm 1)
            (setq out-state 1)
            (setq out-state 0)
        )
    )
    (print out-state)
))

;; ------------------------------
;; a simple NOR circuit ...
;; ------------------------------
(defun nor-circuit ( inp inm )
    (if (= inp 0)
        (if (= inm 0)
            (setq out-state 1)
            (setq out-state 0)
        )
    )
    (if (= inp 1)
        (if (= inm 0)
            (setq out-state 0)
            (setq out-state 0)
        )
    )
    (print out-state)
)

;; ------------------------------
;; a simple NAND circuit ...
;; ------------------------------
(defun nand-circuit ( inp inm )
    (if (= inp 0)
        (if (= inm 0)
            (setq out-state 1)
            (setq out-state 1)
        )
    )
    (if (= inp 1)
        (if (= inm 0)
            (setq out-state 1)
            (setq out-state 0)
        )
    )
    (print out-state)
)

;; ------------------------------
;; a simple OR circuit ...
;; ------------------------------
(defun or-circuit ( inp inm )
    (if (= inp 0)
        (if (= inm 0)
            (setq out-state 0)
            (setq out-state 1)
        )
    )
    (if (= inp 1)
        (if (= inm 0)
            (setq out-state 1)
            (setq out-state 1)
        )
    )
    (print out-state)
)

;; ------------------------------
;; a simple XOR circuit ...
;; ------------------------------
(defun or-circuit ( inp inm )
    (if (= inp 0)
        (if (= inm 0)
            (setq out-state 0)
            (setq out-state 1)
        )
    )
    (if (= inp 1)
        (if (= inm 0)
            (setq out-state 1)
            (setq out-state 0)
        )
    )
    (print out-state)
)

;; ------------------------------
;; a simple XNOR circuit ...
;; ------------------------------
(defun or-circuit ( inp inm ing )
    (if (= inp 0)
        (if (= inm 0)
            (setq out-state 0)
            (setq out-state 1)
        )
    )
    (if (= inp 1)
        (if (= inm 0)
            (if (= ing 1)
                (setq out-state 0)
            )
        )
        (if (= inm 1)
            (if (= ing 0)
                (setq out-state 1)
            )
        )
    )
    (print out-state)
)

;; ------------------------------
;; a simple NOT circuit ...
;; ------------------------------
(defun or-circuit ( inp )
    (if (= inp 0)
        (setq out-state 0)
        (setq out-state 1)
    )
    (if (= inp 1)
        (setq out-state 0)
        (setq out-state 1)
    )
    (print out-state)
)

;; main
(defun main ()

;    (print (slot-value *power-source* 'land))
    
)

