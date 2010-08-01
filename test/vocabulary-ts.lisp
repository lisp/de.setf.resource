;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.resource.implementation; -*-


(in-package :de.setf.resource.implementation)

(:documentation
  "This file defines tests for RDF vocabulary interatction with stores."
 
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
  "'de.setf.resource' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.resource' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.resource' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/)."))

;;; (test:execute-test :resource.vocabulary-ts.**)


(test:test resource.vocabulary-ts.add-vocabulary.1
  (let ((source (resource-mediator 'resource-mediator
                                   :store nil
                                   :identifier-function 'camel-dash-canonicalizer))
        (vocabulary (make-instance 'vocabulary :uri "x")))
    (add-vocabulary source vocabulary)
    (eq (find-vocabulary source "x") vocabulary)))


(test:test resource.vocabulary-ts.add-vocabulary.2
  (let ((source (resource-mediator 'resource-mediator :store nil))
        (vocabulary (make-instance 'vocabulary :uri "x"
                                   :identifier-map '((:uri-1 . "x/uri_1") (:uri-2 . "x/uri_2")))))
    (and (add-vocabulary source vocabulary)
         ;(maphash #'(lambda (k v) (print (list k v))) (source-model2repository-value-map source))
         (eq :uri-1 (rdf:model-value source (intern "uri_1" (find-package "x/")))))))


(test:test resource.vocabulary-ts.uri-symbol.2
  (let ((source (resource-mediator 'resource-mediator
                                   :store nil
                                   :identifier-function 'camel-dash-canonicalizer))
        (vocabulary (make-instance 'vocabulary :uri "x"
                                   :identifier-map '((:uri-1 . "uri/1")  (:uri-2 . "uri/2")))))
    ;; (maphash #'(lambda (k v) (print (list k v))) (source-model2repository-value-map source))
    (add-vocabulary source vocabulary)
    (and (eq (rdf:model-value source (rdf:repository-value source :uri-1)) :uri-1)
         (eq (rdf:model-value source (rdf:repository-value source :uri-2)) :uri-2)
         (eq (rdf:model-value source (rdf:repository-value source :uri-3)) :uri-3)
         (equal (rdf:repository-value source :uri-1) (intern "1" (find-package "uri/")))
         (equal (rdf:repository-value source :uri-2) (intern "2" (find-package "uri/")))
         (equal (rdf:repository-value source :uri-3) :|uri3|)
         (delete-package "uri/"))))


(test:test resource.vocabulary-ts.load-vocabulary.1
  "This loads the foaf vocabulary from the file system, plus two others upon which it depends."
  (let* ((v (load-vocabulary (vocabulary-pathname "http://xmlns.com/foaf/0.1/")
                             "http://xmlns.com/foaf/0.1/")))
    (and (equal "foaf" (vocabulary-name v))
         (eq (first (find (find-symbol "Agent" "http://xmlns.com/foaf/0.1/")
                          (vocabulary-definitions v)
                          :key #'second))
             'rdfs:defclass))))
    

(test:test resource.vocabulary-ts.load-vocabulary.2
  "Load the foaf vocabulary through a triple-store, check that the names and definitions
 are complete."
  (let* ((*load-verbose* (eq test::*test-unit-mode* :verbose))
         (m (resource-mediator 'wilbur-mediator :identifier-function 'camel-dash-canonicalizer))
         (v (load-vocabulary m "http://xmlns.com/foaf/0.1/"))
         (class-names '("AGENT" "ONLINE-CHAT-ACCOUNT" "ONLINE-ECOMMERCE-ACCOUNT" "ONLINE-GAMING-ACCOUNT"
                        "ONLINE-ACCOUNT" "IMAGE" "PROJECT" "ORGANIZATION" "DOCUMENT" "PERSON"
                        "LABEL-PROPERTY")))
    (and (equal "foaf" (vocabulary-name v))
         (equal "http://xmlns.com/foaf/0.1/" (vocabulary-uri v))
         (equal (length class-names) (length (vocabulary-definitions v)))
         (null (set-difference class-names (mapcar #'second (vocabulary-definitions v))
                               :test #'string-equal)))))


(test:test resource.vocabulary-ts.load-vocabulary.3
  "Load the foaf vocabulary through a triple-store. Use a concrete term uri to designate it and
 check that the base uri is recognized as the ontology designator."
  (let* ((m (resource-mediator 'wilbur-mediator :identifier-function 'camel-dash-canonicalizer))
         (*load-verbose* (eq test::*test-unit-mode* :verbose))
         (v (load-vocabulary m "http://xmlns.com/foaf/0.1/Agent")))
    (and (equal "foaf" (vocabulary-name v))
         (equal "http://xmlns.com/foaf/0.1/" (vocabulary-uri v)))))


(test:test resource.vocabulary-ts.wilbur-mediator.1
  "This tests that the literal mapping is adopted from the default vocabularies despite the
 specified identifier canonicalizer."
  (let ((m (resource-mediator 'wilbur-mediator :identifier-function 'camel-dash-canonicalizer)))
    (and (eq '|http://www.w3.org/1999/02/22-rdf-syntax-ns#|:|subject|
             (rdf:model-value m (wilbur:node "http://www.w3.org/1999/02/22-rdf-syntax-ns#subject")))
         (eq (rdf:repository-value m '|http://www.w3.org/1999/02/22-rdf-syntax-ns#|:|subject|)
             (wilbur:node "http://www.w3.org/1999/02/22-rdf-syntax-ns#subject")))))


(test:test resource.vocabulary-ts.find-vocabulary.1
  (let ((m (resource-mediator 'resource-mediator :store nil)))
    (and (eq *rdf-vocabulary*
             (find-vocabulary m "http://www.w3.org/1999/02/22-rdf-syntax-ns#"))
         (eq *rdfs-vocabulary*
             (find-vocabulary m "http://www.w3.org/2000/01/rdf-schema#")))
         (eq *owl-vocabulary*
             (find-vocabulary m "http://www.w3.org/2002/07/owl#"))))
