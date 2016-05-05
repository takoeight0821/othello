(in-package :cl-user)
(defpackage othello.server
  (:use :cl :cl-annot :clack :ningle :othello.engine :othello.svg)
  (:import-from :othello.svg
   :svg :draw-board-svg)
  (:import-from :othello.engine
   :othello-a-step))
(in-package :othello.server)
(annot:enable-annot-syntax)

;; The othello game server

(defvar *app* (make-instance 'ningle:<app>))

(defparameter *board* (othello.engine::initial-board))
(setq *random-state* (make-random-state))

(defparameter *current-player* othello.engine:black)

(setf (ningle:route *app* "/" :accept '("text/html" "text/xml"))
      (lambda (params)
        (with-output-to-string (*standard-output*)

          (if (assoc "chosen" params :test #'string=)
              (let ((pos (read-from-string (cdr (assoc "chosen" params :test #'string=)))))

                (othello-a-step *board* *current-player* (lambda (player board) pos))
                (setq *current-player* (othello.engine:next-to-play *board* *current-player* nil))))

          (svg (* 50 10) (* 50 10) (draw-board-svg *board* *current-player*))
          (terpri)
          (princ (if (equal 1 *current-player*) "Black" "White"))

          (when (null *current-player*)
            (format t "<br>The game is over.<br>Final result: Black ~a / White ~a.<br>"
                    (count 1 *board*) (count 2 *board*))
            (format t (if (> (count 1 *board*) (count 2 *board*))
                          "Black win!"
                          (if (= (count 1 *board*) (count 2 *board*))
                              "Draw..."
                              "White win!")))))))

