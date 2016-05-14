(in-package :cl-user)
(defpackage othello.server
  (:use :cl :cl-annot :clack :othello.util :othello.engine :othello.svg)
  (:import-from :othello.svg
   :svg :draw-board-svg)
  (:import-from :othello.engine
   :othello-a-step))
(in-package :othello.server)
(annot:enable-annot-syntax)

;; The othello game server

(defparameter *board* (othello.engine::initial-board))
(setq *random-state* (make-random-state))

(let ((p othello.engine:black))
  (defun current-player () p)
  (defun switch-player () (setq p (othello.engine:next-to-play *board* p nil))))

(defun draw-othello (pos)
  (with-output-to-string (*standard-output*)
    (if pos
        (othello-a-step *board* (current-player) (lambda (player board) pos))
        (switch-player))

    (svg (* 50 10) (* 50 10) (draw-board-svg *board* (current-player)))
    (terpri)
    (princ (if (equal 1 (current-player)) "Black" "White"))

    (when (null (current-player))
      (format t "<br>The game is over.<br>Final result: Black ~a / White ~a.<br>"
              (count 1 *board*) (count 2 *board*))
      (format t (if (> (count 1 *board*) (count 2 *board*))
                    "Black win!"
                    (if (= (count 1 *board*) (count 2 *board*))
                        "Draw..."
                        "White win!"))))))

(defun parse (param)
  (if param
      (read-from-string (subseq param 7))
      nil))

(defun othello-handler (env)
  (let ((pos (parse (cdr (assoc :query-string (othello.util:pairs env))))))
    
    `(200 (:content-type "text/html")
          (,(draw-othello pos)))))

;; (setf (ningle:route *app* "/")
;;       (lambda (params)
;;         `(200 (:content-type "text/html")
;;               (,
;;                (draw-othello (assoc "chosen" params :test #'string=))
;;                ))))

