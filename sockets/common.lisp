;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   Copyright (C) 2006 by Stelian Ionescu                                 ;
;                                                                         ;
;   This program is free software; you can redistribute it and/or modify  ;
;   it under the terms of the GNU General Public License as published by  ;
;   the Free Software Foundation; either version 2 of the License, or     ;
;   (at your option) any later version.                                   ;
;                                                                         ;
;   This program is distributed in the hope that it will be useful,       ;
;   but WITHOUT ANY WARRANTY; without even the implied warranty of        ;
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         ;
;   GNU General Public License for more details.                          ;
;                                                                         ;
;   You should have received a copy of the GNU General Public License     ;
;   along with this program; if not, write to the                         ;
;   Free Software Foundation, Inc.,                                       ;
;   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (declaim (optimize (speed 3) (safety 1) (space 1) (debug 1)))
(declaim (optimize (speed 0) (safety 2) (space 0) (debug 2)))

(in-package #:net.sockets)

(defmacro with-alien-saps ((&rest vars) &body body)
  `(sb-sys:with-pinned-objects ,(mapcar #'second vars)
     (let (,@(mapcar #'(lambda (pair)
                         `(,(first pair) (sb-alien:alien-sap ,(second pair))))
                     vars))
       ,@body)))

(defmacro with-pinned-aliens ((&rest vars) &body body)
  `(sb-alien:with-alien ,vars
     (sb-sys:with-pinned-objects ,(mapcar #'first vars)
       ,@body)))

(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(deftype ub8 () `(unsigned-byte 8))
(deftype ub16 () `(unsigned-byte 16))
(deftype ub32 () `(unsigned-byte 32))
(deftype sb8 () `(signed-byte 8))
(deftype sb16 () `(signed-byte 16))
(deftype sb32 () `(signed-byte 32))

(defun string-or-parsed-number (val)
  (let ((tmpval val)
        type)
    (etypecase val
      (ub16
       (values :number val))
      (string
       (multiple-value-bind (parsed pos)
           (parse-integer val :junk-allowed t)
         (if (and parsed
                  (eql pos (length val))) ; the entire string is a number
             (progn
               (setf type :number)
               (setf tmpval parsed))
             (setf type :string))
         (values type tmpval))))))

(defun c->lisp-bool (val)
  (if (zerop val) nil t))

(defun lisp->c-bool (val)
  (if val 1 0))

(defmacro addrerr-value (keyword)
  `(et:alien-enum-value et:addrinfo-errors ,keyword))

(defmacro unixerr-value (keyword)
  `(et:alien-enum-value et:errno-values ,keyword))

;;;
;;; Byte-swap functions
;;;

(defun htons (short)
#+little-endian
  (let ((newshort 0))
    (declare (type ub16 newshort)
             (type ub16 short))
    (setf (ldb (byte 8 0) newshort) (ldb (byte 8 8) short))
    (setf (ldb (byte 8 8) newshort) (ldb (byte 8 0) short))
    newshort)
#+big-endian short)

(defun ntohs (short)
  (htons short))

(defun htonl (long)
#+little-endian
  (let ((newlong 0))
    (declare (type ub32 newlong)
             (type ub32 long))
    (setf (ldb (byte 8  0) newlong) (ldb (byte 8 24) long))
    (setf (ldb (byte 8 24) newlong) (ldb (byte 8  0) long))
    (setf (ldb (byte 8  8) newlong) (ldb (byte 8 16) long))
    (setf (ldb (byte 8 16) newlong) (ldb (byte 8  8) long))
    newlong)
#+big-endian long)

(defun ntohl (long)
  (htonl long))

(defun copy-simple-array-ub16-to-alien-vector (lisp-vec alien-vec)
  (declare (type (simple-array ub16 (8)) lisp-vec)
           (type (alien (array et:uint16-t 8)) alien-vec))
  (dotimes (i 8)
    (setf (deref alien-vec i)
          (htons (aref lisp-vec i)))))

(defun map-ipv4-vector-to-ipv6 (addr)
  (declare (type (simple-array ub8 (*)) addr))
  (let ((ipv6addr (make-array 8 :element-type 'ub16
                                :initial-element 0)))
    ;; setting the IPv4 marker
    (setf (aref ipv6addr 5) #xFFFF)
    ;; setting the first two bytes
    (setf (aref ipv6addr 6) (+ (ash (aref addr 0) 8)
                               (aref addr 1)))
    ;; setting the last two bytes
    (setf (aref ipv6addr 7) (+ (ash (aref addr 2) 8)
                               (aref addr 3)))

    ipv6addr))

;; From CLOCC's PORT library
(defun vector-to-ipaddr (vector)
  (coerce vector '(simple-array ub8 (4)))
  (+ (ash (aref vector 0) 24)
     (ash (aref vector 1) 16)
     (ash (aref vector 2) 8)
     (aref vector 3)))

(defun make-sockaddr-in (sin ub8-vector &optional (port 0))
  (declare (type (alien (* et:sockaddr-in)) sin))
  (et:memset sin 0 et::size-of-sockaddr-in)
  (setf (slot sin 'et:family) et:af-inet)
  (setf (slot sin 'et:address) (htonl (vector-to-ipaddr ub8-vector)))
  (setf (slot sin 'et:port) (htons port))
  sin)

(defun make-sockaddr-in6 (sin6 ub16-vector &optional (port 0))
  (declare (type (alien (* et:sockaddr-in6)) sin6))
  (et:memset sin6 0 et::size-of-sockaddr-in6)
  (setf (slot sin6 'et:family) et:af-inet6)
  (let ((u16-vector (slot (slot (slot sin6 'et:address)
                                'et:in6-u)
                          'et::addr16)))
    (copy-simple-array-ub16-to-alien-vector ub16-vector u16-vector)
    (setf (slot sin6 'et:port) (htons port)))
  sin6)

(defun make-sockaddr-un (sun string)
  (declare (type (alien (* et:sockaddr-un)) sun)
           (type string string))
  (et:memset sun 0 et::size-of-sockaddr-un)
  (setf (slot sun 'et:family) et:af-local)
  (let ((buff (sb-ext:string-to-octets string))
        (path (slot sun 'et:path)))
    (loop
       :for off :below (min (length buff)
                            (1- et:unix-path-max))
       :do (setf (deref path off) (aref buff off))))
  sun)

(defun make-vector-u8-4-from-in-addr (in-addr)
  (declare (type ub32 in-addr))
  (let ((vector (make-array 4 :element-type 'ub8)))
    (setf in-addr (ntohl in-addr))
    (setf (aref vector 0) (ldb (byte 8 24) in-addr))
    (setf (aref vector 1) (ldb (byte 8 16) in-addr))
    (setf (aref vector 2) (ldb (byte 8  8) in-addr))
    (setf (aref vector 3) (ldb (byte 8  0) in-addr))

    vector))

(defun make-vector-u16-8-from-in6-addr (in6-addr)
  (declare (type (alien (* et:in6-addr)) in6-addr))
  (let ((newvector (make-array 8 :element-type 'ub16))
        (u16-vector (slot (slot in6-addr 'et:in6-u)
                          'et::addr16)))
    (dotimes (i 8)
      (setf (aref newvector i) (ntohs (deref u16-vector i))))

    newvector))

(defun make-netaddr-from-sockaddr-in (sin)
  (declare (type (alien (* et:sockaddr-in)) sin))
  (make-address :ipv4 (make-vector-u8-4-from-in-addr
                       (slot sin 'et:address))))

(defun make-netaddr-from-sockaddr-in6 (sin6)
  (declare (type (alien (* et:sockaddr-in6)) sin6))
  (make-address :ipv6 (make-vector-u16-8-from-in6-addr
                       (addr (slot sin6 'et:address)))))

(defun make-netaddr-from-sockaddr-un (sun)
  (declare (type (alien (* et:sockaddr-un)) sun))
  (let ((path (slot sun 'et:path))
        (name (make-string (1- et:unix-path-max)))
        (abstract nil))
    (if (zerop (deref path 0))
        ;; abstract address
        (progn
          (setf path (cast path (array (unsigned 8) 0)))
          (setf abstract t)
          (loop
             :for sindex :from 0 :below (1- et:unix-path-max)
             :for pindex :from 1 :below et:unix-path-max
             :do (setf (schar name sindex)
                       (code-char (deref path pindex)))))
        ;; address is in the filesystem
        (setf name (cast path c-string)))
    (make-instance 'localaddr
                   :name name
                   :abstract abstract)))

(defun make-netaddr-from-sockaddr (sa)
  (declare (type (alien (* et:sockaddr)) sa))
  (ecase (slot sa 'et:family)
    (#.et:af-inet
     (make-netaddr-from-sockaddr-in (cast sa (* et:sockaddr-in))))
    (#.et:af-inet6
     (make-netaddr-from-sockaddr-in6 (cast sa (* et:sockaddr-in6))))
    (#.et:af-local
     (make-netaddr-from-sockaddr-un (cast sa (* et:sockaddr-un))))))

(defun make-netaddr-from-sockaddr-storage (sa)
  (declare (type (alien (* et:sockaddr-storage)) sa))
  (ecase (slot sa 'et:family)
    (#.et:af-inet
     (make-netaddr-from-sockaddr-in (cast sa (* et:sockaddr-in))))
    (#.et:af-inet6
     (make-netaddr-from-sockaddr-in6 (cast sa (* et:sockaddr-in6))))
    (#.et:af-local
     (make-netaddr-from-sockaddr-un (cast sa (* et:sockaddr-un))))))