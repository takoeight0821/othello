(in-package :cl-user)
(defpackage othello-test
  (:use :cl
        :othello
        :prove))
(in-package :othello-test)

;; NOTE: To run this test file, execute `(asdf:test-system :othello)' in your Lisp.

(plan nil)

;; blah blah blah.

(finalize)
