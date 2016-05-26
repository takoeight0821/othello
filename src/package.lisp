(in-package :cl-user)
(defpackage othello
  (:use :cl
   #+clisp :socket
   #-clisp :clack))

