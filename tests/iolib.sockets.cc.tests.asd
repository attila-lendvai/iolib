;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem :iolib.sockets.cc.tests
  :description "Tests for :iolib.sockets.cc"
  :author "Attila Lendvai  <attila.lendvai@gmail.com>"
  :maintainer "Attila Lendvai  <attila.lendvai@gmail.com>"
  :licence "MIT"
  :depends-on (:iolib.sockets.cc :hu.dwim.stefil)
  :defsystem-depends-on (:iolib.asdf :iolib-grovel)
  :pathname "sockets.cc/"
  :serial t
  :components
  ((:file "package")
   (:file "reverser")))
