(in-package :cl-user)
(defpackage othello
  (:use :clack :cl :othello.server))
(in-package :othello)

(defparameter *server* nil)

(defun start ()
  (setq *server* (clack:clackup othello.server::*app*)))

(defun stop ()
  (clack:stop *server*))
