;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.resource.implementation; -*-


(in-package :de.setf.resource.implementation)

(:documentation
  "This file mediates access to allegrograph RDF stores for the `de.setf.resource` CLOS linked data library."
  
  (copyright
   "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
   "'de.setf.resource' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.resource' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.resource' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/).")
  
  (description "Implement data, identifier, and instance mediation for a allegrograph rdf repository.

 - use the subject node instance as the identity cache
 - wrap the triple acessors
 - support find for subject based on the node-keyed cache
 - support select for resource-object and triple instances
 - delete based on db.agraph:triple and rdf:triple instances"))

(defvar *agraph-db-path* #P"LIBRARY:de;setf;resource;rdf;agraph")

(defun agraph-db ()
  (unless db.agraph:*db*
    (db.agraph:create-triple-store *agraph-db-path*))
  db.agraph:*db*)

(defclass agraph-mediator (repository-mediator)
  ((repository
    :initarg :db :initform (agraph-db))
   (persistent
    :initform t :allocation :class
    :documentation "allegrograph is backed by a remote storage service or filesystem storage")
   (readable
    :initform t :allocation :class
    :documentation "allegrograph supports read/write operations.")
   (writable
    :initarg :writable :initform t :allocation :class
    :documentation "allegrograph supports read/write operations.")))


(defmethod rdf:delete-statement ((mediator agraph-mediator) (triple vector))
  (db.agraph:delete-triple (db.agraph:triple-id triple) :db (mediator-repository mediator)))


(defmethod rdf:delete-statement ((mediator agraph-mediator) (triple rdf:triple))
  (db.agraph:delete-triples :db (mediator-repository mediator)
                            :s (rdf:repository-value mediator (triple-subject triple))
                            :p (rdf:repository-value mediator (triple-predicate triple))
                            :o (rdf:repository-value mediator (triple-object triple))))


(defmethod rdf:find-instance ((mediator agraph-mediator) (subject t))
  (rdf:find-instance mediator (rdf:repository-value mediator subject)))

(defmethod rdf:find-instance ((mediator agraph-mediator) (designator db.agraph:future-part))
  (gethash designator (repository-instance-cache mediator)))


(defmethod rdf:graph ((statement vector))
  (db.agraph:graph statement))


(defmethod rdf:identifier-p ((object db.agraph:future-part))
  t)

(defmethod rdf:identifier-p ((object vector))
  (typep object '(simple-array (unsigned-byte 8) (12))))


(defmethod rdf:object ((statement vector))
  (db.agraph:object statement))


(defmethod rdf:predicate ((statement vector))
  (db.agraph:predicate statement))

(defmethod rdf:id ((statement vector))
  (db.agraph:triple-id statement))

(defmethod rdf:project-graph ((quad rdf:quad) (destination agraph-mediator))
  (db.agraph:add-triple  (rdf:repository-value destination (quad-subject quad))
                         (rdf:repository-value destination (quad-predicate quad))
                         (rdf:repository-value destination (rdf:quad-object quad))
                         :g (rdf:repository-value destination (rdf:quad-context quad))
                         :db (mediator-repository destination)))

(defmethod rdf:project-graph ((statement vector) (object resource-object))
  "Given a vector, treat it as an opaque statement in order to support allegrograph.
 Consistent with which, statement sequences must be represented as lists."
  (when (rdf:equal (db.agraph:subject statement) (uri object))
    (rdf:insert-statement statement object)))


(defmethod rdf:query ((mediator agraph-mediator) &key subject predicate object graph continuation)
  (if continuation
    (let ((cursor (db.agraph:get-triples :db (mediator-repository mediator)
                                         :s (rdf:repository-value mediator subject)
                                         :p (rdf:repository-value mediator predicate)
                                         :o (rdf:repository-value mediator object)
                                         :g (rdf:repository-value mediator graph))))
      (loop (unless (db.agraph:cursor-next-p cursor) (return))
            (funcall continuation (db.agraph:cursor-next-row cursor))))
    (db.agraph:get-triples-list :db (mediator-repository mediator) :s subject :p predicate :o object :g graph)))


(defmethod repository-uri ((mediator agraph-mediator) (uri t))
  (rdf:repository-value mediator uri))

(defmethod repository-uri ((mediator agraph-mediator) (uri string))
  (db.agraph:intern-remediator uri))


(defmethod rdf:repository-value ((mediator t) (value db.agraph:future-part))
  value)

(defmethod rdf:repository-value ((mediator agraph-mediator) (value string))
  (db.agraph:intern-typed-literal value (db.agraph:intern-resource "http://www.w3.org/2001/XMLSchema#string")))

(defmethod rdf:repository-value ((mediator agraph-mediator) (value float))
  (db.agraph:intern-typed-literal (princ-to-string value)
                                  (db.agraph:intern-resource "http://www.w3.org/2001/XMLSchema#float")))

(defmethod rdf:repository-value ((mediator agraph-mediator) (value double-float))
  (db.agraph:intern-typed-literal (princ-to-string value)
                                  (db.agraph:intern-resource "http://www.w3.org/2001/XMLSchema#double")))

(defmethod rdf:repository-value ((mediator agraph-mediator) (value integer))
  (db.agraph:intern-typed-literal (princ-to-string value)
                                  (db.agraph:intern-resource "http://www.w3.org/2001/XMLSchema#integer")))

(defmethod rdf:repository-value ((mediator agraph-mediator) (value symbol))
  (flet ((canonicalize (symbol) (canonicalize-identifier mediator symbol)))
    (declare (dynamic-extent #'canonicalize))
    (symbol-uri-namestring value #'canonicalize)))

(defmethod rdf:repository-value ((mediator agraph-mediator) (identifier uuid:uuid))
  ;;;??? should this be transformed into a URI to make the node?
  (repository-uri mediator (uri-namestring identifier)))

#+de.setf.xml
(defmethod rdf:repository-value ((mediator agraph-mediator) (identifier xqdm:uname))
  (let ((uri-base (xqdm:namespace-name (xqdm:namespace identifier))))
    (repository-uri mediator (concatenate 'string uri-base
                                          (unless (uri-has-separator-p uri-base) "/")
                                          (xqdm:local-part identifier)))))



(defmethod rdf:statement-p ((object vector))
  "allegrograph represents statements as 56-byte vectors."
  (typep object '(SIMPLE-ARRAY (UNSIGNED-BYTE 8) (56))))


(defmethod rdf:subject ((statement vector))
  (db.agraph:subject statement))

