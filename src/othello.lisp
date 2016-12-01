(in-package :othello)

(defparameter *server* nil)

(defun start-server (&key (port 5000))
  (setf *server* (clack:clackup #'othello-handler :port port)))

(defun stop-server ()
  (clack:stop *server*))

