(in-package :cl-user)
(defpackage othello.util
  (:use :cl)
  (:export
   :random-elt
   :split
   :pairs))
(in-package :othello.util)

;; The original codes from "実用Common Lisp" and "Land of Lisp"

(defun random-elt (choices)
  "Choose an element from a list at randam."
  (elt choices (random (length choices))))

(defmacro split (val yes no)
  "Split val to head and tail."
  (let ((g (gensym)))
    `(let ((,g ,val))
       (if ,g
           (let ((head (car ,g))
                 (tail (cdr ,g)))
             ,yes)
           ,no))))

(defun pairs (list)
  "Example: '(a b c d) -> '((a . b) (c . d))"
  (labels ((f (list acc)
             (split list
                    (if tail
                        (f (cdr tail) (cons (cons head (car tail)) acc))
                        (reverse acc))
                    (reverse acc))))
    (f list nil)))
