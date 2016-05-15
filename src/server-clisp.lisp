(in-package :cl-user)
(defpackage othello.server
  (:use :cl :othello.svg :othello.engine :socket))
(in-package :othello.server)

;; The original code from Land of Lisp

(defun http-char (c1 c2 &optional (default #\Space))
  "Encode the hex"
  (let ((code (parse-integer
               (coerce (list c1 c2) 'string)
               :radix 16
               :junk-allowed t)))
    (if code
        (code-char code)
        default)))

(defun decode-param (s)
  "Decode HTTP-encoded string"
  (labels ((f (lst)
             (when lst
               (case (car lst)
                 (#\% (cons (http-char (cadr lst) (caddr lst))
                            (f (cdddr lst))))
                 (#\+ (cons #\Space (f (cdr lst))))
                 (otherwise (cons (car lst) (f (cdr lst))))))))
    (coerce (f (coerce s 'list)) 'string)))

(defun parse-params (s)
  (let ((i1 (position #\= s))
        (i2 (position #\& s)))
    (cond (i1 (cons (cons (intern (string-upcase (subseq s 0 i1)))
                          (decode-param (subseq s (1+ i1) i2)))
                    (and i2 (parse-params (subseq s (1+ i2))))))
          ((equal s "") nil)
          (t s))))

(defun parse-url (s)
  (let* ((url (subseq s
                      (+ 2 (position #\space s))
                      (position #\space s :from-end t)))
         (x (position #\? url)))
    (if x
        (cons (subseq url 0 x) (parse-params (subseq url (1+ x))))
        (cons url nil))))

(defun get-header (stream)
  (let* ((s (read-line stream))
         (h (let ((i (position #\: s)))
              (when i
                (cons (intern (string-upcase (subseq s 0 i)))
                      (subseq s (+ i 2)))))))
    (when h
      (cons h (get-header stream)))))

(defun get-content-params (stream header)
  (let ((length (cdr (assoc 'content-length header))))
    (when length
      (let ((content (make-string (parse-integer length))))
        (read-sequence content stream)
        (parse-params content)))))

(defun serve (request-handler)
  (let ((socket (socket:socket-server 8080)))
    (unwind-protect
         (loop (with-open-stream (stream (socket:socket-accept socket))
                 (let* ((url (parse-url (read-line stream)))
                        (path (first url))
                        (header (get-header stream))
                        (params (append (rest url)
                                        (get-content-params stream header)))
                        (*standard-output* stream))
                   (funcall request-handler path header params))))
      (socket:socket-server-close socket))))

(defparameter *board* (othello.engine::initial-board))
(setq *random-state* (make-random-state))

(let ((p othello.engine:black))
  (defun current-player () p)
  (defun switch-player () (setq p (othello.engine:next-to-play *board* p nil))))

(defun draw-othello (pos)
  (with-output-to-string (*standard-output*)
    (when pos
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

(defun hello-request-handler (path header params)
  (if (equal path "greeting")
      (if params
          (format t "<html><body>Nice to meet you, ~a!</body></html>" (cdar params))
          (princ "<html><form>What is your name?<input name='name' /></form></html>"))
      (princ "Sorry... I don't know that page.")))


(defun othello-handler (path header params)
  (declare (ignore path header))
  (format t "HTTP/1.0 200 OK~%Content-Type: text/html~2%")
  (princ (if params
             (draw-othello (parse-integer (cdar params)))
             (draw-othello nil))))
