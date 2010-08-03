;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.resource.implementation; -*-


(in-package :de.setf.resource.implementation)

(:documentation
  "This file defines tests for the meta-classes for the `de.setf.resource` Common Lisp / RDF library."
 
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
  "'de.setf.resource' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.resource' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.resource' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/)."))

;;; (test:execute-test :resource.resource-class.**)

(test:test resource.resource-class.finalize-inheritance
  (progn (c2mop:finalize-inheritance (find-class 'resource-class))
         (c2mop:finalize-inheritance (find-class 'abstract-resource-class))
         
         (c2mop:finalize-inheritance (find-class 'rdf-relation-definition))
         (c2mop:finalize-inheritance (find-class 'rdf-direct-relation-definition))
         (c2mop:finalize-inheritance (find-class 'archetypal-property-definition))
         (c2mop:finalize-inheritance (find-class 'prototypal-property-definition))
         (c2mop:finalize-inheritance (find-class 'rdf-effective-property-definition))
         (c2mop:finalize-inheritance (find-class 'rdf-statement-slot-definition))
         t))


(test:test resource.resource-class.defaults
  (and (eql (class-compute-uri-function 'person) #'COMPUTE-OBJECT-UUID)
       (eql (class-property-missing-function 'person) #'rdf:property-missing)))


(test:test resource.resource-class.make-instance.1
  (let ((class (c2mop:ensure-class 'test-resource-class
                                   :metaclass  'resource-class
                                   :direct-superclasses (list (find-class 'resource-object))
                                   :direct-slots '((:name slot1 ))
                                   :datatype :test-type)))
    (and (typep class 'resource-class)
          (eq (class-name class) 'test-resource-class)
          (eq (find-class 'test-resource-class) class)
          (eq (find-class :test-type) class)
          (eq (rdf:find-class 'resource-class 'test-resource-class) class)
          (eq (rdf:find-class 'resource-class :test-type) class))))



(test:test resource.resource-class.make-instance.2
  "Define a sub-class of PERSON, with additional archetypal properties for weight and height.
 Do not assert a new datatype, but check that a subtype relation applies.
 nb. the indirect type tests have been observed to fail in mcl if it's type chace gets out of sync"
  (let ((*load-verbose* (eq test::*test-unit-mode* :verbose))
        (class (c2mop:ensure-class 'test-person
                                   :metaclass  'resource-class
                                   :vocabulary "http://xmlns.com/foaf/0.1/"
                                   :direct-superclasses '(person)
                                   :direct-slots '((:name weight :initargs (:weight) :readers (person-name)  :writers ((setf person-name))
                                                          :type string :predicate {foaf}firstName :datatype {xsd}string)
                                                   (:name age :initargs (:age) :readers (person-age) :writers ((setf person-age))
                                                          :type integer :predicate {foaf}age :datatype {xsd}integer)
                                                   (:name parents :initargs (:parents) :readers (person-parents) :writers ((setf person-parents))
                                                          :type (cons test-person) :datatype {rdf}Seq :predicate {rel}childof)))))
    (c2mop:finalize-inheritance (find-class 'test-person))
    (list (typep (class-repository class) 'repository-mediator)
         (eq (class-datatype class) '{foaf}Person)
         (subtypep 'test-person 'person)
         (typep (make-instance 'test-person :uri nil) 'person)
         (not (subtypep 'test-person '{foaf}Person))
         #+digitool
         (typep (make-instance 'test-person :uri nil) '{foaf}Person)
         (typep (make-instance 'test-person :uri nil) (find-class '{foaf}Person))
         (typep (class-vocabulary class) 'vocabulary)
         (equal (vocabulary-uri (class-vocabulary class)) "http://xmlns.com/foaf/0.1/"))))


(test:test resource.resource-class.find-class
  "The db should be empty, which leads to loading the schema from the file-system cache"
  (let* ((class (find-class 'person))
         (*load-verbose* (eq test::*test-unit-mode* :verbose)))
    (slot-makunbound class 'vocabulary)
    (slot-makunbound class 'repository)
    (and (rdf:find-class class '{rdfs}Class)
         #+(or)                         ; this should not fail - the person fixture class datatype is {foaf}Person
                                        ; which means that the standard-class method finds it
         (typep (test:ignored-error (rdf:find-class (class-repository class) '{foaf}Person)) 'rdf:class-not-found-error)
         (rdf:find-class (class-repository class) '{foaf}Person)
         (typep (rdf:find-class class '{foaf}Person)
                'resource-class)
         (class-vocabulary class)
         (typep (rdf:find-class (class-repository class) '{foaf}Person)
                'resource-class))))


(test:test resource.resource-class.class-repository
  "Verify that a specialization inherits source and vocabulary from its superclass"
  (let ((person-class (find-class 'person))
        (adult-class (find-class 'adult)))
    (and (typep person-class 'resource-class)
         (typep (class-repository person-class) 'repository-mediator)
         (equal (class-datatype person-class) '{foaf}Person)
         (equal (vocabulary-uri (class-vocabulary person-class)) "http://xmlns.com/foaf/0.1/")
         (subtypep adult-class person-class)
         (typep adult-class 'resource-class)
         (typep (class-repository adult-class) 'repository-mediator)
         (equal (class-datatype adult-class) '{foaf}Person)
         (equal (vocabulary-uri (class-vocabulary adult-class)) "http://xmlns.com/foaf/0.1/"))))


(test:test resource.resource-class.find-archetypal-property-definition.1
  (let ((class (find-class 'person)))
    (and (typep (find-archetypal-property-definition-by-name 'person 'name) 'rdf:archetypal-property-definition)
         (typep (find-archetypal-property-definition-by-name class 'name) 'rdf:archetypal-property-definition)
         (typep (find-archetypal-property-definition-by-name class 'age) 'rdf:archetypal-property-definition)
         (typep (find-archetypal-property-definition-by-name class 'parents) 'rdf:archetypal-property-definition)
         t)))


(test:test resource.resource-class.find-archetypal-property-definition.2
  (let ((class (find-class 'person)))
    (flet ((test-sd (sd)
             (and (typep sd 'rdf:archetypal-property-definition)
                  (typep (slot-definition-statement-slot sd) 'rdf-statement-slot-definition)
                  (eq sd (slot-definition-property-slot (slot-definition-statement-slot sd))))))
      (and (test-sd (find-archetypal-property-definition-by-name class 'name))
           (test-sd (find-archetypal-property-definition-by-name class 'age))
           (test-sd (find-archetypal-property-definition-by-name class 'parents))))))


(test:test resource.resource-class.find-prototypal-property-definition
  "verify prototypal slots and augmentation with a property."
  (let ((p (make-instance 'person :name "name" :age 1 :parents nil)))
    (and (rdf:insert-statement p (rdf:triple (uri p) 'height 100))
         (typep (find-prototypal-property-definition p 'height) 'rdf:prototypal-property-definition))))


(test:test resource.resource-class.property-value.1
  "verify intirinsic slots and augmentation with a property."
  (let ((p (make-instance 'person :name "name" :age 1 :parents nil)))
    (and (equal (person-name p) "name")
         (equal (person-age p) 1)
         (equal (person-parents p) nil)
         ;; the standard behavior is to return nil for a missing property
         (null (person-height p))
         (equal (setf (person-height p) 100) 100)
         (equal (person-height p) 100)
         (equal (property-value p 'height) 100)
         (eq (object-state p) rdf:transient))))


(test:test resource.resource-class.property-value.2
  "verify intrinsic slots and augmentation with a property."
  (let ((p (make-instance 'person :name "name" :age 1 :parents nil)))
    (and (rdf:insert-statement p (rdf:triple (uri p) 'height 100))
         (equal (property-value p '{foaf}firstName) "name")
         (equal (property-value p '{foaf}age) 1)
         (equal (property-value p '{rel}childof) nil)
         (equal (property-value p 'height) 100))))


(test:test resource.resource-class.insert-statement.0
  "verify intrinsic slots and augmentation with a property."
  (let ((p (make-instance 'person :name "name" :age 1 :parents nil)))
    (and (rdf:insert-statement p (rdf:triple (uri p) 'height 100))
         (equal (property-value p 'height) 100))))


(test:test resource.resource-class.insert-statement.1
  "verify intrinsic slots and augmentation with a property."
  (let ((p (make-instance 'person :name "name" :age 1 :parents nil)))
    (and (rdf:insert-statement p (rdf:triple (uri p) 'height 100))
         (unbind-property-slots p)
         (null (find-prototypal-property-definition p 'height)))))


(test:test resource.resource-class.insert-statement.2
  "verify distinction between setting a property and adding a statement"
  (let ((p (make-instance 'person :name "name" :age 1 :parents nil)))
    (and (rdf:insert-statement p (rdf:triple (uri p) 'height 100))
         (rdf:insert-statement p (rdf:triple (uri p) 'height 200))
         (equal (person-height p) '(200 100))
         (equal (setf (person-height p) 300) 300)
         (equal (property-value p 'height) 300))))


(test:test resource.resource-class.map-property-slots
  "verify intrinsic slots and augmentation with a property."
  (let ((p (make-instance 'person :name "name" :age 1 :parents nil))
        (expected '(name age parents height)))
    (and (rdf:insert-statement p (rdf:triple (uri p) 'height 100))
         (let ((list nil))
           (map-property-slots #'(lambda (sd) (push (c2mop:slot-definition-name sd) list)) p)
           (null (set-exclusive-or list expected :test #'equal))))))


(test:test resource.resource-class.map-property-values.1
  "verify intrinsic slots and augmentation with a property."
  (let* ((p (make-instance 'person :name "name" :age 1 :parents nil))
        (expected (list "name" 1 100)))
    (and (rdf:insert-statement p (rdf:triple (uri p) 'height 100))
         (let ((list ()))
           (map-property-values #'(lambda (v) (push v list)) p)
           (null (set-exclusive-or list expected :test #'equal))))))


(test:test resource.resource-class.map-property-values.2
  "verify intrinsic slots and augmentation with a property."
  (let* ((p (make-instance 'person :name "name" :age 1 :parents nil))
        (expected (list "name" 1 100)))
    (and (setf (property-value p 'height) 100)
         (let ((list ()))
           (map-property-values #'(lambda (v) (push v list)) p)
           (null (set-exclusive-or list expected :test #'equal))))))

;;; (map nil 'print (wilbur:db-triples wilbur:*db*))
;;; (map nil 'print (wilbur:db-query wilbur:*db* nil nil !owl:Ontology))
;;; (map nil 'print (wilbur:db-query wilbur:*db* nil !rdf:type nil))
;;; (rdf:query (class-repository (find-class 'prototype)) :predicate '{rdf}type :object '{owl}Ontology)
;;; (unregister-value (class-repository (find-class 'prototype)) '{rdf}type nil)
;;; (unregister-value (class-repository (find-class 'prototype)) '{owl}Ontology nil)
;;; (rdf:repository-value (class-repository (find-class 'resource-class)) '{owl}Ontology)
;;; (trace register-value)
;;; (trace (rdf:repository-value :before :break))

#|
;; an exercise to track the sync failures in the mcl type cache
;; it was observed that {foaf}Persons cached type was a class of  former (setf (find-class ...) ) binding
;; rather than the current one. the solution was
;;   (ccl::remove-from-type-cache  '|http://xmlns.com/foaf/0.1/|:|Person|)

(let* ((p (make-instance 'test-person :uri nil))
                (tp #'(lambda () (unwind-protect
                                   (progn (trace ccl::%%typep typep ccl::%typep CCL::CLASS-CELL-TYPEP ccl::class-typep)
                                          (typep p '{foaf}Person))
                                   (untrace ccl::%%typep typep ccl::%typep CCL::CLASS-CELL-TYPEP ccl::class-typep)))))
           ;; (print (ccl:advise ccl::typep (progn (print ccl:arglist) (when (eq (first ccl:arglist) p) (inspect ccl:arglist)) (:do-it)) :when :around))
           (inspect tp)
           (when (funcall tp) (break)))
|#
