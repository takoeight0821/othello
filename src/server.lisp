(in-package :cl-user)
(defpackage othello.server
  (:use :cl :cl-annot :clack :ningle))
(in-package :othello.server)
(annot:enable-annot-syntax)

;; The othello game server

(defvar *app* (make-instance 'ningle:<app>))

(setf (ningle:route *app* "/")
      "Welcome to ningle!")

(setf (ningle:route *app* "/put/:pos")
      (lambda (params)
        (format nil "~a" (cdr (assoc :pos params :test #'string=)))))
(clack:clackup *app*)
