;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.resource.implementation; -*-


(in-package :de.setf.resource.implementation)

(:documentation
  "This file defines a package-based uri-encoded-symbol reader for the `de.setf.resource` CLOS linked data library."
 
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
  "'de.setf.resource' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.resource' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.resource' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/)."))

(defvar *lock-vocabulary-packages* t)

(defun |{-reader|
       (stream char
               &aux
               (namespace (make-array 32 :adjustable t :fill-pointer 0 :element-type 'character))
               (local-part (make-array 32 :adjustable t :fill-pointer 0 :element-type 'character)))
  "RDF-directed symbol reader"

  ;; read and intern a qname value
  (setf (fill-pointer namespace) 0
        (fill-pointer local-part) 0)
  (loop (setf char (read-char stream))
        (when (char= char #\})
          (setf namespace (or (find-symbol namespace :keyword)
                              (intern (subseq namespace 0) :keyword)))
          (return))
        (vector-push-extend char namespace))
  (cond ((eql #\| (peek-char t stream))
         ;; where explicitly escaped allow non-name characters
         (read-char stream)
         (loop (setf char (read-char stream nil nil))
               (unless char (return))
               (when (eql #\| char)
                 (return))
               (vector-push-extend char local-part)))
        (t
         (loop (setf char (read-char stream nil nil))
               (unless char (return))
               (when (or (member char '(#\space #\tab #\return #\linefeed))
                         (multiple-value-bind (function non-breaking-p)
                                              (get-macro-character char)
                           (and function (not non-breaking-p))))
                 (unread-char char stream)
                 (return))
               (vector-push-extend char local-part))))

  ;; search for the namespace in both the dynamic stack and the static definitions. the former by prefix,
  ;; the latter by full name; then first look for an existing name and if not found cons a new local part and
  ;; intern a new name.
  ;; if in a #- mode, return at least NIL, in order that the likely '{xx}yyy complete the '-readers'
  ;; constraints
  (if *read-suppress*
    nil
    (let* ((namespace (or (find-package namespace)
                          (error "Package not found: ~s." namespace)))
           (uname (cond ((find-symbol local-part namespace))
                        (*lock-vocabulary-packages*
                         (cerror "Create a new symbol."
                                 "The symbol is not present in the vocabulary: ~s: ~s."
                                 local-part namespace)
                         (intern (subseq local-part 0) namespace))
                        (t
                         (intern (subseq local-part 0) namespace)))))
      uname)))

(defun install-|{-reader| ()
  #-clisp
  (set-macro-character #\{ '|{-reader| t)

  #+clisp
  (progn
    (defun single-value-|{-reader| (stream char &aux result)
      (setf result (|{-reader| stream char))
      result)
    (set-macro-character #\{ 'single-value-|{-reader| t)))

(install-|{-reader|)


