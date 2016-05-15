(in-package :cl-user)
(defpackage othello
  (:use :cl :othello.server))
(in-package :othello)

(defparameter *server* nil)

