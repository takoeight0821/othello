(in-package :cl-user)
(defpackage othello.server
  (:use :cl :cl-annot :clack :ningle :othello.engine))
(in-package :othello.server)
(annot:enable-annot-syntax)

;; The othello game server

(defvar *app* (make-instance 'ningle:<app>))

(defparameter *board* (othello.engine::initial-board))
(setq *random-state* (make-random-state))

(defun htmlize-board (board)
  (with-output-to-string (*standard-output*)
    (with-input-from-string (in (with-output-to-string (*standard-output*)
                                  (othello.engine::print-board board)))
      (loop for line = (read-line in nil nil)
            while line
            do (progn (princ line)
                      (princ "<br>"))))))

(setf (ningle:route *app* "/" :accept '("text/html" "text/xml"))
      (lambda (params)
        (declare (ignore params))
        (htmlize-board *board*)))
