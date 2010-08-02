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

(defparameter *uri-pathname-root* "LIBRARY:de;setf;xml;namespaces;")

(defvar *prototype-slot-name* :prototype)

(defvar *property-slot-name* :properties)

(defparameter *repository-mediator.default* '(wilbur-mediator))

(defvar +unbound-marker+ (list :unbound))

(defvar *vocabularies* (make-hash-table :test 'equal))


