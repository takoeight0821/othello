(in-package :cl-user)
(defpackage othello.util
  (:use :cl :cl-annot))
(in-package :othello.util)

(annot:enable-annot-syntax)

;; The original codes from "実用Common Lisp" and "Land of Lisp"

@export
(defun random-elt (choices)
  "Choose an element from a list at randam."
  (elt choices (random (length choices))))

@export
(defmacro split (val yes no)
  "Split val to head and tail."
  (let ((g (gensym)))
    `(let ((,g ,val))
       (if ,g
           (let ((head (car ,g))
                 (tail (cdr ,g)))
             ,yes)
           ,no))))

@export
(defun pairs (list)
  "Example: '(a b c d) -> '((a . b) (c . d))"
  (labels ((f (list acc)
             (split list
                    (if tail
                        (f (cdr tail) (cons (cons head (car tail)) acc))
                        (reverse acc))
                    (reverse acc))))
    (f list nil)))
