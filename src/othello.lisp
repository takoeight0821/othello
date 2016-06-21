(in-package :othello)

(defparameter *server* nil)

(defun start-server ()
  (setf *server* (clack:clackup #'othello-handler)))

(defun stop-server ()
  (clack:stop *server*))
