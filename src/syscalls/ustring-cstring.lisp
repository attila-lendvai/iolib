;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Strings used for decoding Unix pathnames: invalid UTF8 octets
;;;     are encoded in the invalid range #x110000 - #x1100FF.
;;;

(in-package :iolib.syscalls)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +cstring-path-max+ 65535))

(defun ustring-to-cstring (ustring c-ptr)
  (let ((index 0))
    (flet ((output-octet (octet)
             (setf (cffi:mem-aref c-ptr :unsigned-char index) octet)
             (incf index)))
      (declare (inline output-octet))
      (loop :for code :across ustring :do
            (cond
              ((< code #x80)
               (output-octet code))
              ((< code #x800)
               (output-octet (logior #xC0 (ldb (byte 5 6) code)))
               (output-octet (logior #x80 (ldb (byte 6 0) code))))
              ((< code #x10000)
               (output-octet (logior #xE0 (ldb (byte 4 12) code)))
               (output-octet (logior #x80 (ldb (byte 6 6) code)))
               (output-octet (logior #x80 (ldb (byte 6 0) code))))
              ((< code #x110000)
               (output-octet (logior #xF0 (ldb (byte 3 18) code)))
               (output-octet (logior #x80 (ldb (byte 6 12) code)))
               (output-octet (logior #x80 (ldb (byte 6 6) code)))
               (output-octet (logior #x80 (ldb (byte 6 0) code))))
              ((<= code uchar-code-limit)
               (output-octet (logand code #xFF)))
              (t (error "BUG! Uchars should not be larger than ~S" uchar-code-limit)))
            :finally (output-octet 0))
      (values c-ptr index))))

(defun count-ustring-octets (ustring)
  (loop :for code :across ustring
        :sum (cond
               ((<  code #x80)             1)
               ((<  code #x800)            2)
               ((<  code #x10000)          3)
               ((<  code #x110000)         4)
               ((<= code uchar-code-limit) 1)
               (t (error "BUG! Uchars should not be larger than ~S"
                         uchar-code-limit)))))

(defun cstring-alloc (ustring)
  "Allocate a null-terminated foreign buffer containing USTRING."
  (let* ((length (count-ustring-octets ustring))
         (ptr (foreign-alloc :uchar :count (1+ length))))
    (ustring-to-cstring ustring ptr)))

(defmacro with-ustring-to-cstring ((var ustring &optional size-var) &body body)
  `(multiple-value-bind (,var ,@(when size-var (list size-var)))
       (cstring-alloc ,ustring)
     (unwind-protect
          (progn ,@body)
       (foreign-free ,var))))

(deftype cstr-offset ()
  `(integer 0 ,(1+ +cstring-path-max+)))

(defun utf8-extra-bytes (code)
  (declare (type (unsigned-byte 8) code)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let ((vec (load-time-value
              (coerce #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                        0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                        2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2  3 3 3 3 3 0 0 0 0 0 0 0 0 0 0 0)
                      '(simple-array (unsigned-byte 8) (256))))))
    (aref (the (simple-array (unsigned-byte 8) (256)) vec) code)))

(defun offsets-from-utf8 (extra-bytes)
  (declare (type (mod 4) extra-bytes)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let ((vec (load-time-value
              (coerce #(#x00000000 #x00003080 #x000E2080 #x03C82080)
                      '(simple-array (unsigned-byte 26) (4))))))
    (aref (the (simple-array (unsigned-byte 26) (4)) vec) extra-bytes)))

(defun legal-utf8-cstring (ptr start len)
  (declare (type cstr-offset start len)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let ((end (+ start len))
        (srchr (mem-aref ptr :unsigned-char start))
        c)
    (flet ((getch ()
             (mem-aref ptr :unsigned-char (decf (the (unsigned-byte 17) end)))))
      (declare (inline getch))
      (when (=  len 4) (setf c (getch)) (unless (<= #x80 c #xBF) (return* nil)))
      (when (>= len 3) (setf c (getch)) (unless (<= #x80 c #xBF) (return* nil)))
      (when (>= len 2) (setf c (getch)) (unless (<= #x00 c #xBF) (return* nil))
        (case srchr
          (#xE0 (when (< c #xA0) (return* nil)))
          (#xED (when (> c #x9F) (return* nil)))
          (#xF0 (when (< c #x90) (return* nil)))
          (#xF4 (when (> c #x8F) (return* nil)))
          (t    (when (< c #x80) (return* nil)))))
      (when (>= len 1) (when (<= #x80 srchr #xC1) (return* nil)))
      (when (> srchr #xF4) (return* nil))
      t)))

(defun cstring-to-ustring (c-ptr &optional (c-len (1+ +cstring-path-max+)))
  (declare (type cstr-offset c-len)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let ((index 0) (uindex 0)
        (ustring (make-array c-len :element-type 'uchar)))
    (declare (type cstr-offset index uindex)
             (type simple-ustring ustring))
    (flet ((input-char ()
             (prog1 (mem-aref c-ptr :unsigned-char index)
               (incf index)))
           (output-uchar (code)
             (setf (aref ustring uindex) code) (incf uindex)))
      (declare (inline input-char output-uchar))
      (loop :for byte0 := (mem-aref c-ptr :unsigned-char index)
            :until (or (>= index c-len) (zerop byte0)) :do
            (block decode-one-char
              (let* ((code 0)
                     (extra-bytes (min (utf8-extra-bytes byte0)))
                     (legalp (and (legal-utf8-cstring c-ptr index (1+ extra-bytes))
                                  (< extra-bytes (- c-len index)))))
                (declare (type (mod 4) extra-bytes)
                         (type (unsigned-byte 27) code))
                (labels ((finish-seq (extra-bytes offset)
                           (decf code (the (unsigned-byte 26) (offsets-from-utf8 extra-bytes)))
                           (output-uchar (+ code offset)))
                         (legalchk ()
                           (unless legalp (finish-seq 0 #x110000) (return-from decode-one-char))))
                  (when (>= extra-bytes 3) (setf code (ash (+ code (input-char)) 6)) (legalchk))
                  (when (>= extra-bytes 2) (setf code (ash (+ code (input-char)) 6)) (legalchk))
                  (when (>= extra-bytes 1) (setf code (ash (+ code (input-char)) 6)) (legalchk))
                  (when (>= extra-bytes 0) (setf code (ash (+ code (input-char)) 0)) (legalchk))
                  (finish-seq extra-bytes 0))))))
    (shrink-vector ustring uindex)))

(defmacro with-cstring-to-ustring ((var size &optional size-var) &body body)
  `(with-foreign-pointer (,var ,size ,size-var)
     (progn ,@body
       (cstring-to-ustring ,var ,size-var))))


;;; Automatic Conversion of Foreign Strings to Ustrings
;;; Initially copied from cffi/src/string.lisp

(define-foreign-type cstring-type ()
  (;; Should we free after translating from foreign?
   (free-from-foreign :initarg :free-from-foreign
                      :reader cstring-free-from-foreign-p
                      :initform nil :type boolean)
   ;; Should we free after translating to foreign?
   (free-to-foreign :initarg :free-to-foreign
                    :reader cstring-free-to-foreign-p
                    :initform t :type boolean))
  (:actual-type :pointer)
  (:simple-parser ustring))

;; TODO: use EXPAND-TO-FOREIGN
(defmethod translate-to-foreign (s (type cstring-type))
  (let ((s (etypecase s
             (ustring s)
             (string (string-to-ustring s)))))
    (values (cstring-alloc s)
            (cstring-free-to-foreign-p type))))

(defmethod translate-from-foreign (ptr (type cstring-type))
  (unwind-protect
       (if (null-pointer-p ptr)
           nil
           (cstring-to-ustring ptr))
    (when (and (cstring-free-from-foreign-p type)
               (not (null-pointer-p ptr)))
      (foreign-free ptr))))

(defmethod free-translated-object (ptr (type cstring-type) free-p)
  (when free-p
    (foreign-free ptr)))
