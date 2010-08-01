;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.resource.implementation; -*-


(in-package :de.setf.resource.implementation)

(:documentation
  "This file serves as a compilation stub for allegrograph."
 
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
  "'de.setf.resource' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.resource' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.resource' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/)."))

(defpackage :db.agraph
  (:use )
  (:export :*db*
           :add-triple
           :create-triple-store
           :cursor-next-p
           :cursor-next-row
           :delete-triple
           :delete-triples
           :future-part
           :get-triples
           :get-triples-list
           :intern-resource
           :intern-typed-literal
           :subject
           :predicate
           :object
           :graph
           :triple-id))

(defparameter  db.agraph:*db* nil)

(defclass db.agraph:future-part () ())

(defun agraph-not-present-error ()
  (error "This implementation does not provide allegrograph."))

(defun db.agraph:create-triple-store (name &key)
  (declare (ignore name))
  (agraph-not-present-error))


(defun db.agraph:add-triple (s p o &key g db preserve-string)
  (declare (ignore s p o g db preserve-string))
  (agraph-not-present-error))

(defun db.agraph:cursor-next-p (cursor)
  (declare (ignore cursor))
  (agraph-not-present-error))

(defun db.agraph:cursor-next-row (cursor)
  (declare (ignore cursor))
  (agraph-not-present-error))

(defun db.agraph:delete-triple (id &key db)
  (declare (ignore id db))
  (agraph-not-present-error))

(defun db.agraph:delete-triples (&key s p o g db)
  (declare (ignore s p o g db))
  (agraph-not-present-error))

(defun db.agraph:get-triples (&key s p o g db)
  (declare (ignore s p o g db))
  (agraph-not-present-error))

(defun db.agraph:get-triples-list (&key s p o g db)
  (declare (ignore s p o g db))
  (agraph-not-present-error))

(defun db.agraph:intern-resource (subject &key db)
  (declare (ignore subject db))
  (agraph-not-present-error))

(defun db.agraph:intern-typed-literal (string &optional type)
  (declare (ignore string type))
  (agraph-not-present-error))


(defun db.agraph:triple-id (triple)
  (declare (ignore triple))
  (agraph-not-present-error))
(defun db.agraph:subject (triple)
  (declare (ignore triple))
  (agraph-not-present-error))
(defun db.agraph:predicate (triple)
  (declare (ignore triple))
  (agraph-not-present-error))
(defun db.agraph:object (triple)
  (declare (ignore triple))
  (agraph-not-present-error))
(defun db.agraph:graph (triple)
  (declare (ignore triple))
  (agraph-not-present-error))



