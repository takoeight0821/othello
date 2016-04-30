(in-package :cl-user)
(defpackage othello.engine
  (:use :cl :othello.util))
(in-package :othello.engine)

(defparameter *all-directions* '(-11 -10 -9 -1 1 9 10 11))

(defconstant empty 0 "An empty square")
(defconstant black 1 "A black piece")
(defconstant white 2 "A white piece")
(defconstant outer 3 "Marks squares outside the 8x8 board")

(deftype piece () `(integer ,empty ,outer))

(defun char-of (piece) (char ".@0?" piece))

(defun opponent (player) (if (eql player black) white black))

(deftype board () '(simple-array piece (100)))

(defun bref (board square) (aref board square))
(defsetf bref (board square) (val)
  `(setf (aref ,board ,square) ,val))

(defun copy-board (board)
  (copy-seq board))

(defparameter *all-squares*
  (loop for i from 11 to 88 when (<= 1 (mod i 10) 8) collect i))

(defun initial-board ()
  "Return a board, empty except for four pieces in the middle."
  ;; ボードは100要素のベクターから、要素11-88が使用される
  ;; その他はボード外のOUTERとしてマーク付けされる。
  ;; 最初に、中央の4つのマス目に石が置かれ、その他は空とする。
  (let ((board (make-array 100 :element-type 'piece
                               :initial-element outer)))
    (dolist (square *all-squares*)
      (setf (bref board square) empty))
    (setf (bref board 44) white) (setf (bref board 45) black)
    (setf (bref board 54) black) (setf (bref board 55) white)
    board))

(defun count-difference (player board)
  "Count player's pieces minus opponent's pieces."
  (- (count player board)
     (count (opponent player) board)))

(defun print-board (board)
  "Print a board, along with some statistics."
  (format t "~2&     1 2 3 4 5 6 7 8  [~c=~2a ~c=~2a (~@d)]"
          (char-of black) (count black board)
          (char-of white) (count white board)
          (count-difference black board))
  (loop for row from 1 to 8 do
    (format t "~&  ~d " (* 10 row))
    (loop for col from 1 to 8
          for piece = (bref board (+ col (* 10 row)))
          do (format t "~c " (char-of piece))))
  (format t "~2&"))

(defun valid-p (move)
  "Valid moves are numbers in the range 11-88 that end in 1-8."
  (and (integerp move) (<= 11 move 88) (<= 1 (mod move 10) 8)))

(defun legal-p (move player board)
  "A Legal move must be into an empty square, and it must flip at least one opponent piece."
  (and (eql (bref board move) empty)
       (some (lambda (dir) (would-flip? move player board dir))
             *all-directions*)))

(defun make-move (move player board)
  "Update board to reflect move by player."
  ;; 最初に手を作成して次に反転を行う
  (setf (bref board move) player)
  (dolist (dir *all-directions*)
    (make-flips move player board dir))
  board)

(defun make-flips (move player board dir)
  "Make any flips in the given direction."
  (let ((bracketer (would-flip? move player board dir)))
    (when bracketer
      (loop for c from (+ move dir) by dir until (eql c bracketer)
            do (setf (bref board c) player)))))

(defun would-flip? (move player board dir)
  "Would this move result in any flips in this direction?
  If so, return the square number of the bracketing piece."
  ;; 隣接するマス目から開始して、プレイヤの石によって挟まれる
  ;; 少なくとも1つの敵の石があるならば反転が起こる
  (let ((c (+ move dir)))
    (and (eql (bref board c) (opponent player))
         (find-bracketing-piece (+ c dir) player board dir))))

(defun find-bracketing-piece (square player board dir)
  "Return the square number of the bracketing piece."
  (cond ((eql (bref board square) player) square)
        ((eql (bref board square) (opponent player))
         (find-bracketing-piece (+ square dir) player board dir))
        (t nil)))

(defun othello (bl-strategy wh-strategy &optional (print t))
  "Play a game of othello. Return the score, where a positive difference means black, the first player, wins."
  (setq *random-state* (make-random-state))
  (let ((board (initial-board)))
    (loop for player = black
            then (next-to-play board player print)
          for strategy = (if (eql player black)
                             bl-strategy
                             wh-strategy)
          until (null player)
          do (get-move strategy player board print))
    (when print
      (format t "~&The game is over. Final result:")
      (print-board board))
    (count-difference black board)))

(defun next-to-play (board previous-player print)
  "Compute the player to move next, or NIL if nobody can move."
  (let ((opp (opponent previous-player)))
    (cond ((any-legal-move? opp board) opp)
          ((any-legal-move? previous-player board)
           (when print
             (format t "~&~c has no moves and must pass."
                     (char-of opp)))
           previous-player)
          (t nil))))

(defun any-legal-move? (player board)
  "Does player have any legal moves in this position?"
  (some (lambda (move) (legal-p move player board))
        *all-squares*))

(defun get-move (strategy player board print)
  "Call the player's strategy function to get a move.
  Keep calling until a legal move is made."
  (when print (print-board board))
  (let ((move (funcall strategy player (copy-board board))))
    (cond
      ((and (valid-p move) (legal-p move player board))
       (when print
         (format t "~&~c moves to ~d." (char-of player) move))
       (make-move move player board))
      (t (warn "Illegal move: ~d" move)
         (get-move strategy player board print)))))

(defun human (player board)
  "A human player for the game of Othello"
  (declare (ignore board))
  (format t "~&~c to move: " (char-of player))
  (read))

(defun random-strategy (player board)
  "Make any legal move."
  (random-elt (legal-moves player board)))

(defun legal-moves (player board)
  "Returns a list of legal moves for player"
  (loop for move in *all-squares*
        when (legal-p move player board) collect move))

(defun maximize-difference (player board)
  "A strategy that maximizes the difference in pieses."
  (funcall (maximizer #'count-difference) player board))

(defun maximizer (eval-fn)
  "Return a strategy that will consider every legal move,")
