(in-package :cl-user)
(defpackage othello.server
  (:use :cl :cl-annot :clack :ningle :othello.engine :othello.svg)
  (:import-from :othello.svg
   :svg
   :draw-board-svg))
(in-package :othello.server)
(annot:enable-annot-syntax)

;; The othello game server

(defvar *app* (make-instance 'ningle:<app>))

(defparameter *board* (othello.engine::initial-board))
(setq *random-state* (make-random-state))

(defparameter *current-player* othello.engine:black)

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
        (with-output-to-string (*standard-output*)
          (svg (* 50 10) (* 50 10) (draw-board-svg *board* *current-player*)))
        ;; (setq *current-player* (othello.engine:opponent *current-player*))
        ))
