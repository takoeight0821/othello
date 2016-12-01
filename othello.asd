#|
  This file is a part of othello project.
  Copyright (c) 2016 Kono Yuya (takohati0821@gmail.com)
|#

#|
  Author: Kono Yuya (takohati0821@gmail.com)
|#

(in-package :cl-user)
(defpackage othello-asd
  (:use :cl :asdf))
(in-package :othello-asd)

(defsystem othello
  :version "0.1"
  :author "Kono Yuya"
  :license "MIT license"
  :depends-on (:assoc-utils
               :hunchentoot
               :clack
               :ceramic
               :lucerne
               )
  :components ((:module "src"
                :components
                        (
                         (:file "package")
                         (:file "engine")
                         (:file "svg")
                         (:file "server")
                         (:file "othello")
                         (:file "app")
                         )))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op othello-test))))
