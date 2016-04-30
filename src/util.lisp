(in-package :cl-user)
(defpackage othello.util
  (:use :cl))
(in-package :othello.util)

(defun random-elt (choices)
  "Choose an element from a list at randam."
  (elt choices (random (length choices))))

(defmacro split (val yes no)
  (let ((g (gensym)))
    `(let ((,g ,val))
       (if ,g
           (let ((head (car ,g))
                 (tail (cdr ,g)))
             ,yes)
           ,no))))

(defun pairs (list)
  (labels ((f (list acc)
             (split list
                    (if tail
                        (f (cdr tail) (cons (cons head (car tail)) acc))
                        (reverse acc))
                    (reverse acc))))
    (f list nil)))
