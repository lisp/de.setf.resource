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

 (description "Implement data, identifier, and instance mediation for a allegrograph rdf store.

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

(defclass agraph-mediator (resource-mediator)
  ((store
    :initarg :db :initform (agraph-db)))


(defmethod rdf:delete-statement ((source agraph-mediator) (triple vector))
  (db.agraph:delete-triple (db.agraph:triple-id triple) :db (repository-store source)))


(defmethod rdf:delete-statement ((source agraph-mediator) (triple rdf:triple))
  (db.agraph:delete-triples :db (repository-store source)
                            :s (rdf:repository-value source (triple-subject triple))
                            :p (rdf:repository-value source (triple-predicate triple))
                            :o (rdf:repository-value source (triple-object triple))))


(defmethod rdf:find-instance ((source agraph-mediator) (subject t))
  (rdf:find-instance source (rdf:repository-value source subject)))

(defmethod rdf:find-instance ((source agraph-mediator) (designator db.agraph:future-part))
  (gethash designator (repository-instance-cache source)))


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


(defmethod rdf:project-graph ((quad rdf:quad) (destination agraph-mediator))
  (db.agraph:add-triple  (rdf:repository-value destination (quad-subject quad))
                         (rdf:repository-value destination (quad-predicate quad))
                         (rdf:repository-value destination (rdf:quad-object quad))
                         :g (rdf:repository-value destination (rdf:quad-context quad))
                         :db (repository-store destination)))

(defmethod rdf:project-graph ((statement vector) (object resource-object))
  "Given a vector, treat it as an opaque statement in order to support allegrograph.
 Consistent with which, statement sequences must be represented as lists."
  (when (rdf:equal (db.agraph:subject statement) (uri object))
    (rdf:insert-statement statement object)))


(defmethod rdf:query ((source agraph-mediator) &key subject predicate object graph continuation)
  (if continuation
    (let ((cursor (db.agraph:get-triples :db (repository-store source)
                                         :s (rdf:repository-value source subject)
                                         :p (rdf:repository-value source predicate)
                                         :o (rdf:repository-value source object)
                                         :g (rdf:repository-value source graph))))
      (loop (unless (db.agraph:cursor-next-p cursor) (return))
            (funcall continuation (db.agraph:cursor-next-row cursor))))
    (db.agraph:get-triples-list :db (repository-store source) :s subject :p predicate :o object :g graph)))(defmethod rdf:repository-persistent? ((repository wilbur-mediator))
  "The wilbur store in an in-memory cache. In order to persist its state, use save-repository."
  nil)


(defmethod rdf:repository-persistent? ((repository agraph-mediator))
  "The cassadra store is backed by a remote storage service"
  t)

(defmethod rdf:repository-readable? ((repository agraph-mediator))
  "The cassadra store is backed."
  t)

(defmethod rdf:repository-transient? ((repository agraph-mediator))
  "The allegrograph store is backed by filesystem storage"
  nil)

(defmethod rdf:repository-writable? ((repository agraph-mediator))
  "allegrograph supports read/write operations."
  t)     


(defmethod store-uri ((source agraph-mediator) (uri t))
  (rdf:repository-value source uri))

(defmethod store-uri ((source agraph-mediator) (uri string))
  (db.agraph:intern-resource uri))


(defmethod rdf:repository-value ((source t) (value db.agraph:future-part))
  value)

(defmethod rdf:repository-value ((source agraph-mediator) (value string))
  (db.agraph:intern-typed-literal value (db.agraph:intern-resource "http://www.w3.org/2001/XMLSchema#string")))

(defmethod rdf:repository-value ((source agraph-mediator) (value float))
  (db.agraph:intern-typed-literal (princ-to-string value)
                                  (db.agraph:intern-resource "http://www.w3.org/2001/XMLSchema#float")))

(defmethod rdf:repository-value ((source agraph-mediator) (value double-float))
  (db.agraph:intern-typed-literal (princ-to-string value)
                                  (db.agraph:intern-resource "http://www.w3.org/2001/XMLSchema#double")))

(defmethod rdf:repository-value ((source agraph-mediator) (value integer))
  (db.agraph:intern-typed-literal (princ-to-string value)
                                 (db.agraph:intern-resource "http://www.w3.org/2001/XMLSchema#integer")))

(defmethod rdf:repository-value ((source resource-mediator) (value symbol))
  (flet ((canonicalize (symbol) (canonicalize-identifier source symbol)))
    (declare (dynamic-extent #'canonicalize))
    (symbol-uri-namestring value #'canonicalize)))

(defmethod rdf:repository-value ((source agraph-mediator) (identifier uuid:uuid))
  ;;;??? should this be transformed into a URI to make the node?
  (store-uri source (uri-namestring identifier)))

#+de.setf.xml
(defmethod rdf:repository-value ((source agraph-mediator) (identifier xqdm:uname))
  (let ((uri-base (xqdm:namespace-name (xqdm:namespace identifier))))
    (store-uri source (concatenate 'string uri-base
                                   (unless (uri-has-separator-p uri-base) "/")
                                   (xqdm:local-part identifier)))))



(defmethod rdf:statement-p ((object vector))
  "allegrograph represents statements as 56-byte vectors."
  (typep object '(SIMPLE-ARRAY (UNSIGNED-BYTE 8) (56))))


(defmethod rdf:subject ((statement vector))
  (db.agraph:subject statement))

