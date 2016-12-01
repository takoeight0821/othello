(in-package :othello)

(defparameter *port* 5000)
(defun run-app ()
  (start-server :port *port*)
  (let ((window (ceramic:make-window :url (format nil"http://localhost:~D/" *port*))))
    (ceramic:show window)))

(ceramic:define-entry-point :othello ()
  (run-app))
