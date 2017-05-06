(in-package #:cl-user)
(defpackage #:othello/server
  (:use #:cl #:othello/svg)
  (:import-from #:othello/engine
                #:initial-board
                #:black
                #:white
                #:next-to-play
                #:alpha-beta-searcher
                #:weighted-squares
                #:othello-a-step
                #:print-board)
  (:export #:*human-player*
           #:*cpu-strategy*))
(in-package #:othello/server)
(cl-annot:enable-annot-syntax)

;; The othello game server

(defparameter *board* (initial-board))
(setq *random-state* (make-random-state))

(defparameter *human-player* black)
(let ((p black))
  (defun current-player () p)
  (defun switch-player () (setq p (next-to-play *board* p nil)))
  (defun set-player (p2) (setq p p2)))
(defparameter *cpu-strategy* (alpha-beta-searcher 6 #'weighted-squares))

(defparameter *game-log* '())
(defun reset-game ()
  (setf *game-log* '())
  (setf *board* (initial-board))
  (set-player black))

(defun draw-othello (pos &optional (size 50))
  (with-output-to-string (*standard-output*)
    ;; It's dirty code!
    (if (eq (current-player) *human-player*)
        ;; カレントPLが人間
        (progn
          (when pos
            (othello-a-step *board* (current-player)
                            (lambda (player board)
                              (declare (ignore player board))
                              pos))
            (switch-player))

          (svg (* size 10) (* size 10) (draw-board-svg *board* (current-player) size
                                                       (or (null (current-player))
                                                           (eq (current-player) *human-player*))))
          (terpri))
        ;; カレントPLがコンピュータ
        (progn
          (othello-a-step *board* (current-player) *cpu-strategy*)
          (switch-player)
          (svg (* size 10) (* size 10) (draw-board-svg *board* (current-player) size
                                                       (or (null (current-player))
                                                           (eq (current-player) *human-player*))))
          (terpri)))

    (setf *game-log* (cons (list *board*
                                 (with-output-to-string (*standard-output*)
                                   (print-board *board*))
                                 (current-player))
                           *game-log*))
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

@export
(defun othello-handler (env)
  (let ((pos (parse (assoc-utils:aget (assoc-utils:plist-alist env) "query-string"))))
    `(200 (:content-type "text/html")
          (,(draw-othello pos)))))
