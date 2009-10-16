;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; Copyright (C) 2006-2008, Attila Lendvai  <attila.lendvai@gmail.com>
;;;
;;; This code is free software; you can redistribute it and/or
;;; modify it under the terms of the version 2.1 of
;;; the GNU Lesser General Public License as published by
;;; the Free Software Foundation, as clarified by the
;;; preamble found here:
;;;     http://opensource.franz.com/preamble.html
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General
;;; Public License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;;; Boston, MA 02110-1301, USA

(in-package :common-lisp-user)

(asdf:defsystem :iolib.sockets.cc
  :description "Extension to net.sockets that enables handling thousands of connection using delimited continuations."
  :author "Attila Lendvai  <attila.lendvai@gmail.com>"
  :maintainer "Attila Lendvai  <attila.lendvai@gmail.com>"
  :licence "LLGPL-2.1"
  :depends-on (:iolib.sockets :cl-cont :iolib.streams :iolib.multiplex :alexandria :metabang-bind :bordeaux-threads)
  :pathname (merge-pathnames #p"sockets.cc/" *load-truename*)
  :serial t
  :components
  ((:file "package")
   (:file "duplicates")
   (:file "classes")
   (:file "connection")
   (:file "cc-primitives")
   (:file "multiplexer")
   (:file "acceptor")))

(asdf:defsystem :iolib.sockets.cc.tests
  :description "Tests for :iolib.sockets.cc"
  :author "Attila Lendvai  <attila.lendvai@gmail.com>"
  :maintainer "Attila Lendvai  <attila.lendvai@gmail.com>"
  :licence "LLGPL-2.1"
  :depends-on (:iolib.sockets.cc :stefil)
  :pathname (merge-pathnames #p"tests/iolib.sockets.cc/" *load-truename*)
  :serial t
  :components
  ((:file "package")
   (:file "reverser")))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system :iolib.sockets.cc))))
  (asdf:operate 'asdf:load-op :iolib.sockets.cc.tests)
  (in-package :sockets/cc-tests)
  (declaim (optimize (debug 3)))
  (warn "(declaim (optimize (debug 3))) was issued to help later C-c C-c'ing")
  (eval (read-from-string "(stefil:funcall-test-with-feedback-message 'sockets/cc-tests::test)"))
  (values))

;; TODO this should eventually be dropped when asdf gets the relevant OPERATION-DONE-P patch
(defmethod asdf:operation-done-p ((op asdf:test-op) (system (eql (asdf:find-system :iolib.sockets.cc))))
  nil)
