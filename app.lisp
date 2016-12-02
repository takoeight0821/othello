(in-package #:cl-user)
(defpackage #:othello/app
  (:use #:cl)
  (:import-from #:othello/server
                #:othello-handler))
(in-package #:othello/app)
(annot:enable-annot-syntax)

(defparameter *server* nil)

@export
(defun start-server (&key (port 5000))
  (setf *server* (clack:clackup #'othello-handler :port port)))

@export
(defun stop-server ()
  (clack:stop *server*))

@export
(defun run-app ()
  (start-server)
  (let ((window (ceramic:make-window :url "http://localhost:5000/")))
    (ceramic:show window)
    window))

@export
(defun stop-app ()
  (ceramic:stop)
  (stop-server))

(ceramic:define-entry-point :othello ()
  (run-app))
