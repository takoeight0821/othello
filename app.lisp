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

