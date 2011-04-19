;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem :iolib.sockets.cc
  :description "Extension to net.sockets that enables handling thousands of connection using delimited continuations."
  :author "Attila Lendvai  <attila.lendvai@gmail.com>"
  :maintainer "Attila Lendvai  <attila.lendvai@gmail.com>"
  :licence "MIT"
  :defsystem-depends-on (:iolib.asdf :iolib-grovel)
  :depends-on (:iolib.sockets :cl-cont :iolib.streams :iolib.multiplex :alexandria :metabang-bind :bordeaux-threads)
  :default-component-class :iolib-source-file
  :pathname "sockets.cc/"
  :serial t
  :components
  ((:file "package")
   (:file "duplicates")
   (:file "classes")
   (:file "connection")
   (:file "cc-primitives")
   (:file "multiplexer")
   (:file "acceptor")))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system :iolib.sockets.cc))))
  (asdf:operate 'asdf:load-op :iolib.sockets.cc.tests)
  (in-package :sockets/cc-tests)
  (declaim (optimize (debug 3)))
  (warn "(declaim (optimize (debug 3))) was issued to help later C-c C-c'ing")
  (eval (read-from-string "(stefil:funcall-test-with-feedback-message 'sockets/cc-tests::test)"))
  (values))
