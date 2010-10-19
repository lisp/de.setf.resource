;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.resource.implementation; -*-


(in-package :de.setf.resource.implementation)

(:documentation
  "This file defines model global parameters  for the `de.setf.resource` CLOS linked data library."
 
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
  "'de.setf.resource' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.resource' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.resource' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/)."))


(defparameter *urn.cl-package* (find-package :urn.cl))

(defparameter *uri-pathname-root*
  (make-pathname :host "LIBRARY"
                 :directory '(:absolute "de" "setf" "resource" "namespaces")))

(defvar *prototype-slot-name* :prototype)

(defvar *property-slot-name* :properties)

(defparameter *repository-mediator.default* '(wilbur-mediator))


(defvar +unbound-marker+ (list :unbound))


(defvar *uri-separators* (make-hash-table :test 'equal)
  "A global registry for separators for vocabulari URI which have no intrinsic separator.
 The initialization protocol for vocabulary assert one if no intrinsic separator is present.")


(defvar *default-uri-separator* #\/
  "The separator character to use when vocabulary URI has neither an intrinsic nor a declared
 extrinsic spearator. The initial value is '/'.")


(defvar *vocabularies* (make-hash-table :test 'equal)
  "A global registry for vocabularies by vocabulary URI.")


;;;
;;; given multiple threads, each should rebind these:

(defparameter *vector-input-protocol* nil)

(defparameter *vector-output-protocol* nil)

(defun call-with-global-bindings (function)
  (let ((*vector-input-protocol* nil)
        (*vector-output-protocol* nil))
    (initialize-global-bindings)
    (funcall function)))

(defun initialize-global-bindings ()
  ;; the vector protocols are instantiated on-demand
  )

