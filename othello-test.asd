#|
  This file is a part of othello project.
  Copyright (c) 2016 Kono Yuya (takohati0821@gmail.com)
|#

(in-package :cl-user)
(defpackage othello-test-asd
  (:use :cl :asdf))
(in-package :othello-test-asd)

(defsystem othello-test
  :author "Kono Yuya"
  :license "MIT license"
  :depends-on (:othello
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "othello"))))
  :description "Test system for othello"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
