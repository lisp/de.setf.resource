;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: cl-user; -*-

;;; This file defines the packages for the `de.setf.resource` Common Lisp / RDF library.
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
;;;
;;; de.setf.resource uses several packages:
;;; - de.setf.resource (rdf) is the api package
;;; - de.setf.bert comprises the operators for BERT encoding
;;; - de.setf.resource.implementation is the general implementation package

(in-package :cl-user)

(defpackage :de.setf.resource
  (:use )
  (:nicknames :de.setf.rdf :rdf
              :de.setf.resource.schema :rdfs)          ; these last are temporary

  (:documentation "The de.setf.resource (rdf) package comprises the names for API components of the
 de.setf.resource linked-data[1][2][3] library. The terms follow from a number of sources:
 - W3C conceptual models for linked data[4]
 - Datagraph's RDF.rb and Spira.rb libraries
 - wilbur
 - allegrograph
 - swclos

 The terms are chosen to reflect the intended operands in that an operator which applies to a certain
 abstract context or operand class includes a erspective designator as a prefix, while operators which are
 universal do not.
 For example the operator rdf:save is universal, while the rdf:save-resource and rdf:save-repository
 operators intend the respective abstract class of operand. or the statement query operator
 intended to apply both to resource-object instances and repository instances is named simply
 'rdf:has-statement?'

 The library can be partitioned into several aspects:
 - declarations
 - instantiation
 - query
 - manipulation

 property-value, (setf property-value), property-boundp,
 property-exists-p, property-makunbound, get-statements, insert-statement, or remove-statement

 ---
 [1]: http://www.w3.org/standards/semanticweb/data "the Semantic Web @w3c.org"
 [2]: http://en.wikipedia.org/wiki/Linked_Data "Linked Data @ Wikipedia"
 [3]: http://linkeddata.org/ \"linkeddata.org\"
 [4]: http://www.w3.org/standards/techs/rdf#w3c_all
 ")

  (:export
   :*class.archetypal-property-definition*
   :*class.prototypal-property-definition*
   :*class.repository-mediator*
   :*class.resource*
   :add-statement*
   :agraph-mediator
   :archetypal-property-definition
   :archetypal-property-definition-p
   :class-base-url
   :class-direct-source
   :class-direct-base-url
   :class-fragment-character
   :class-not-found-error
   :class-not-found
   :class-property-slots
   :class-repository
   :class-uri-function
   :clean
   :clean-persistent
   :commit
   :context
   :defaccessor
   :defclass
   :defvocabulary
   :delete
   :delete-object
   :delete-predicate
   :delete-statement
   :delete-statement*
   :delete-statement-using-slot
   :delete-subject
   :deleted
   :deleted-new
   :deleted-persistent
   :do-collection
   :ensure-class
   :ensure-instance
   :ensure-vocabulary
   :ensure-uri-package
   :enumeration
   :equal
   :evict
   :feb-timeout-error
   :find-instance
   :find-class
   :find-vocabulary
   :graph
   :has-statement?
   :has-context?
   :has-object?
   :has-predicate?
   :has-subject?
   :hollow
   :id
   :identifier
   :identifier-p
   :insert-statement
   :insert-statement-using-slot
   :instance-not-found
   :instance-not-found-error
   :intern-uri
   :invalid-state-error
   :load-resource
   :load-repository
   :load-repository-as
   :load-vocabulary
   :make-archetypal-property-definition
   :make-persistent
   :make-prototypal-property-definition
   :make-transient
   :make-repository-value
   :make-repository-mediator
   :make-resource
   :map-collection
   :map-contexts
   :map-objects
   :map-property-predicates
   :map-property-slots
   :map-property-values
   :map-predicates 
   :map-statements
   :map-statements*
   :map-subjects
   :mediator-state
   :mediator-repository
   :mediator-default-context
   :model-value
   :modify
   :modified
   :modified-persistent
   :modified-transient
   :namestring
   :new
   :new-persistent
   :non-transactional
   :object
   :object-value
   :persistent
   :predicate
   :predicate-value
   :namespace-equal
   :persistent-slot-reader
   :persistent-slot-writer
   :project-graph
   :property-missing
   :property-missing-error
   :property-read-only
   :property-read-only-error
   :property-value
   :property-value-using-class
   :prototypal-property-definition
   :prototypal-property-definition-p
   :prototypal-property-value
   :prototype
   :quad
   :quad-p
   :quad-id
   :quad-subject
   :quad-predicate
   :quad-object
   :quad-context
   :query
   :rdf-direct-slot-definition
   :rdf-effective-slot-definition
   :rdf-slot-definition
   :rdf-tuple
   :rdf-slot-writer
   :rdf-slot-reader
   :read-properties
   :refresh
   :repository-clear
   :repository-close
   :repository-count
   :repository-empty?
   :repository-indelible?
   :repository-persistent?
   :repository-readable?
   :repository-transient?
   :repository-writable?
   :repository-class-definition
   :repository-property-definition
   :repository-namespace-bindings
   :repository-value
   :repository-value-binary
   :repository-value-double
   :repository-value-float
   :repository-value-i16
   :repository-value-i32
   :repository-value-i64
   :repository-value-i08
   :repository-value-integer
   :repository-value-symbol
   :repository-value-string
   :repository-value-uuid
   :repository-value-uri
   :require-vocabulary
   :resource
   :resource-class
   :resource-not-found-error
   :repository-mediator
   :repository-mediator-p
   :resource-object
   :resource-p
   :retain-values?
   :rollback
   :save
   :save-repository
   :save-repository-as
   :save-resource
   :schema-not-found-error
   :set
   :setf-property-value
   :slot-definition-predicate
   :slot-definition-datatype
   :spoc-case
   :stored
   :statement
   :statement-p
   :subject
   :subject-value
   :this
   :transient
   :triple
   :triple-p
   :triple-id
   :triple-object
   :triple-predicate
   :triple-subject
   :tuple-datatype
   :tuple-object
   :tuple-predicate
   :tuple-subject
   :tuple-value
   :type-of
   :triple
   :unbind-property-slots
   :unbound-source-error
   :uri
   :uri-match-p
   :uri-namestring
   :uri-namestring-identifier
   :valid?
   :vocabulary
   :vocabulary-uri
   :vocabulary-resource-uri
   :wilbur-mediator
   :with-transaction
   :write-properties
   :*urn.cl-package*))


(defpackage :de.setf.resource.implementation
  (:shadowing-import-from :common-lisp
                          :defclass
                          :delete
                          :equal
                          :find-class
                          :make-instance
                          :namestring
                          :require
                          :string
                          :set
                          :type-of)
  (:shadowing-import-from :de.setf.utility
                          :ensure-package)
  (:use :common-lisp
        :de.setf.utility
        :de.setf.rdf
        :de.setf.resource)
  
  #+ccl
  (:import-from :ccl
                :stream-write-byte :stream-read-byte :stream-position
                :stream-force-output)
  #+mcl
  (:import-from :ccl
                :stream-tyo :stream-tyi
                :stream-read-sequence :stream-write-sequence)
  #+sbcl
  (:import-from :sb-gray 
                :stream-write-byte :stream-read-byte
                :stream-read-sequence :stream-write-sequence
                :stream-force-output :stream-finish-output))


(defpackage :urn.cl
  (:use )
  (:nicknames "US-ANSI-INCITS-226-1994-R2004"))


(defpackage "_" (:use )
  (:documentation "A package for macro-generated symbols, to keep them isolated, but available."))

;;; (inspect xqdm::*namespace-dictionary*)
;;;
;;; in the absence of the xml parser, the rdf-associated packages are loaded in connection
;;; with vocabularies in vocabulary.lisp. in preparation for which predefine
;;; those packages which appear in cross references

(loop for (p . exports) in
      '(("http://www.w3.org/1999/02/22-rdf-syntax-ns#")
        ("http://www.w3.org/2000/01/rdf-schema#" "Resource" "Container"))
      unless (find-package p)
      do (progn (setf p (make-package  p :use ()))
                (dolist (e exports) (export (intern e p) p))))

#+de.setf.xml
(flet ((prefix-namespace (ns)
         (dolist (nickname (xqdm::namespace.nicknames ns)) (print (cons nickname ns))
           (unless (ignore-errors (xqdm:prefix-value nickname))
             (setf (xqdm:prefix-value nickname) ns)))))
  (mapc #'prefix-namespace
        (list (xqdm:find-namespace "http://www.w3.org/2001/XMLSchema-datatypes" :if-does-not-exist :load)
              (xqdm:find-namespace "http://xmlns.com/foaf/0.1/" :if-does-not-exist :load)
              (xqdm:find-namespace "http://www.w3.org/1999/02/22-rdf-syntax-ns#" :if-does-not-exist :load)
              (xqdm:find-namespace "http://www.w3.org/2000/01/rdf-schema#" :if-does-not-exist :load)
              (xqdm:find-namespace "http://purl.org/vocab/relationship" :if-does-not-exist :load)
              (xqdm:find-namespace "http://www.w3.org/2002/07/owl#" :if-does-not-exist :load)
              (xqdm:find-namespace "http://purl.org/vocab/vann/" :if-does-not-exist :load))))

