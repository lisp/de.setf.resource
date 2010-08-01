;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.resource.implementation; -*-


(in-package :de.setf.resource.implementation)

(:documentation
  "This file defines a fixtures for the de.setf.resource tests."
 
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
  "'de.setf.resource' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.resource' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.resource' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/)."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *foaf-vocabulary* (rdfs:require "http://xmlns.com/foaf/0.1/"))
  (defvar *rel-vocabulary* (rdfs:require "http://purl.org/vocab/relationship/")))

(setf (logical-pathname-translations "SPIRA")
      '(("**;*.*" "LIBRARY:com;github;datagraph;spira;**;*.*")))

(defpackage "http://example.com" (:use ))

;;; clozure lacks a generic function change-class
(rdfs:defaccessor person-name :property '{foaf}firstName :name name :type string)
(rdfs:defaccessor person-age :property '{foaf}age :name age :type number)
(rdfs:defaccessor person-parents :property '{rel}childof :name parents :type (cons person))
(rdfs:defaccessor person-height :property height :name height :type string)
(rdfs:defaccessor person-weight :property weight :name weight :type string)
(rdfs:defaccessor person-gender :property gender :name gender :type string)

(defclass person (resource-object)
  ((name
    :initarg :name
    :accessor person-name
    :type string :predicate {foaf}firstName :datatype {xsd}string)
   (age
    :initarg :age
    :accessor person-age
    :type integer :predicate {foaf}age :datatype {xsd}integer)
   (parents
    :initarg :parents
    :accessor person-parents
    :type (or (cons person) null) :predicate {rel}childof :datatype {rdfs}Resource))
  (:metaclass resource-class)
  (:datatype {foaf}Person)

  (:documentation "A test class to model {foaf}Person. really need to include that as a superclass."))

(defclass adult (person)
  ()
  (:metaclass resource-class))

(c2mop:finalize-inheritance (find-class 'resource-object))
(c2mop:finalize-inheritance (find-class 'person))
(c2mop:finalize-inheritance (find-class 'adult))

;;; (inspect #'(setf person-height))
;;; (describe (find-class 'person))
;;; (inspect (c2mop:class-slots (find-class 'person)))


(defun load-graph (store source &optional (clear-p t))
  (when clear-p
    (clear-store store)
    (maphash #'(lambda (k v) (declare (ignore k))
                 (when (typep v 'resource-object)
                   (unbind-property-slots v)))
             (source-instance-cache store)))
  (rdf:project-graph source store))

;;;

(defparameter *test.family-graph*
  (let ((uri-child (uuid:make-v1-uuid))
        (uri-father (uuid:make-v1-uuid))
        (uri-mother (uuid:make-v1-uuid)))
    (list (rdf:triple uri-child '{rdf}type '{foaf}Person)
          (rdf:triple uri-child '{foaf}firstName "baby")
          (rdf:triple uri-child '{foaf}age 1)
          (rdf:triple uri-child 'height 50)
          (rdf:triple uri-child 'weight 10)

          (rdf:triple uri-father '{rdf}type '{foaf}Person)
          (rdf:triple uri-father '{foaf}firstName "dad")
          (rdf:triple uri-father '{foaf}age 21)
          (rdf:triple uri-father 'height 200)
          (rdf:triple uri-father 'weight 70)

          (rdf:triple uri-mother '{rdf}type '{foaf}Person)
          (rdf:triple uri-mother '{foaf}firstName "mom")
          (rdf:triple uri-mother '{foaf}age 22)
          (rdf:triple uri-mother 'height 180)
          (rdf:triple uri-mother 'weight 60)

          (rdf:triple uri-child '{rel}childof uri-mother)
          (rdf:triple uri-child '{rel}childof uri-father))))

(defun load-family (source &optional (clear-p t))
  (load-graph source *test.family-graph* clear-p))

;;; (load-family (make-instance 'wilbur-mediator))
;;; (map nil 'print (wilbur:db-triples wilbur:*db*))
