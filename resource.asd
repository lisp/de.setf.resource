;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: cl-user; -*-

;;; This file is the system definition for the `de.setf.resource` Common Lisp library.
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

(asdf:defsystem :de.setf.resource
  :version "0.1"
  :license "AGPL"
  :author "james anderson <james.anderson@setf.de>"
  :depends-on (;; the parser is not used as the the xml+rdf path is deprecated and
               ;; in favor of mediating all rdf operations through a triple store
               ;; or direct processing to ntriple files.
               ;; :de.setf.xml
               :net.common-lisp.closer-mop
               :net.dardoria.uuid       ; order matters to get asdf to find them
               :com.github.ironclad
               ;; ! the reader conficts with that of agraph - need to reinstate that
               :net.sourceforge.wilbur
               :com.b9.puri.puri-ppcre
               :de.setf.utility.mime
               :org.apache.thrift)
  :serial t
  :description "A CLOS-aware RDF interface feature equivalent to Spira.rb"
  :components ((:file "packages")
               (:file "reader")
               (:file "parameters")
               (:file "utilities")
               (:file "vocabulary")
               (:file "n3")
               (:file "api")
               (:file "statement")
               (:file "repository-mediator")
               (:file "resource-class")
               (:file "resource-object")
               (:file "resource-transaction")
               (:file "resource-lifecycle")
               #-lispworks              ; would need porting
               (:module :wilbur
                :components ((:file "wilbur-extensions")
                             (:file "wilbur-mediator"))))
  :long-description
  "`de.setf.resource` implements a transparent bidirectional projection between CLOS models and linked data
 repositories. The implementation relies on the CLOS-MOP support for augmented slot access mechanisms to
 equate class/slot structure with a resource-node[0] oriented view of RDF graphs. It includes
 interfaces to wilbur[1,2], cassandra[8], and allegro-graph[3] repositories which permits both resident and
 remote repositories.

 The core implementation involves three classes, to model classes the resource-class metaclass and the
 resource-object abstract class, and the abstract repository interface class source-mediator.
 The model classes extend the standard-class and standard-object implementations to identify concrete
 archetypal classes with resource classes in repositories, instances with individual resource by URI, and
 slots with properties and computed predicates. Extensions to the instantiation and slot access provide
 access to literal values, navigation between resource instances, and transaction support analogous to the
 JDO persistence life-cycle[4].

 The architecture and interface is analogous to other RDF data bindings, such as RDFBeans[5] and Spira[6],
 and endeavours to find a balance between structural constraints and ad-hoc comprehension well suited to
 CLOS software development.

 See README.md for more information.

 ---
 [0] : http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.100.6738&rep=rep1&type=pdf
 [1] : wilbur-rdf.sourceforge.net/
 [2] : lisp.github.com/wilbur
 [3] : http://www.franz.com/agraph/
 [4] : http://en.wikipedia.org/wiki/Java_Data_Objects
 [5] : http://blog.cyberborean.org/2009/02/06/simple-rdf-data-binding
 [6] : http://github.com/datagraph/spira
 [8] : cassandra.apache.org/
 ")


#+(or)                                  ; generate documentation
(progn
  (asdf:load-system :de.setf.documentation)
  (setf.documentation:document ':de.setf.resource #p"LIBRARY:20100624;")
  )
