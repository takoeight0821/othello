(in-package :cl-user)
(defpackage othello
  (:use :clack :cl :othello.server))
(in-package :othello)

(defparameter *server* nil)

(defun start ()
  (setf *server* (clack:clackup othello.server::*app*)))

(defun stop ()
  (clack:stop *server*))
