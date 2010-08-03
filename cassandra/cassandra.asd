;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: cl-user; -*-

;;; This file is the cassandra module definition for the `de.setf.resource` Common Lisp library.
;;; `de.setf.resource` is a Common Lisp library for CLOS-based RDF work.
;;;
;;; Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved
;;; `de.setf.resource` is free software: you can redistribute it and/or modify it under the terms of version 3
;;; of the the GNU Affero General Public License as published by the Free Software Foundation.
;;;
;;; `de.setf.resource` is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
;;; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;; See the the Affero General Public License for more details.
;;;
;;;  A copy of the GNU Affero General Public License should be included with `de.setf.resource`, as `agpl.txt`.
;;; If not, see the GNU [site](http://www.gnu.org/licenses/).


(in-package :cl-user)

(asdf:defsystem :de.setf.resource.cassandra
  :version "0.1"
  :depends-on (:de.setf.resource
               :de.setf.cassandra)
  :description "The cassandra module for de.setf.resource"
  :components ((:file "cassandra-mediator")
               (:file "rdfrb-mediator" :depends-on ("cassandra-mediator"))
               (:file "spoc-mediator" :depends-on ("cassandra-mediator")))
  :long-description
  "This `de.setf.resource` module implements a specialized resource mediator for cassandra.
 It defines versions for rdf-rb and spoc indices.")


