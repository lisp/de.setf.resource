;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.resource.implementation; -*-

(in-package :de.setf.resource.implementation)

(:documentation
  "This file defines tests for the interface for the clos-rdf layer."
  
  (copyright
   "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
   "'de.setf.resource' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.resource' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.resource' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/)."))


;;; (test:execute-test :resource.interface.**)



(test:test resource.interface.repository-clear.wilbur.1
  (let ((m (repository-mediator 'wilbur-mediator))
        (s (wilbur:triple (wilbur:node "http://www.w3.org/1999/02/22-rdf-syntax-ns#subject")
                          (wilbur:node "http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate")
                          (wilbur:node "http://www.w3.org/1999/02/22-rdf-syntax-ns#object"))))
    (rdf:insert-statement m s)
    (rdf:repository-clear m)
    (null (rdf:query m :subject nil :predicate nil :object nil))))


(test:test resource.interface.delete.wilbur
  (let* ((object (resource :uri '{CL-USER}01 :state rdf:modified-persistent))
         (m (object-repository object))
         (stmt (rdf:triple '{CL-USER}01 '{COMMON-LISP}TYPE '{COMMON-LISP}STANDARD-OBJECT)))
    (rdf:insert-statement m stmt)
    (and (rdf:has-statement? m stmt)
         (progn (rdf:delete object)
                (not (rdf:has-statement? m stmt))))))


(test:test resource.interface.delete-statement.wilbur
  (let* ((m (repository-mediator 'wilbur-mediator))
         (stmt (rdf:triple '{rdf}subject '{rdf}predicate '{rdf}object)))
    (rdf:insert-statement m stmt)
    (and (rdf:has-statement? m stmt)
         (progn (rdf:delete-statement m stmt)
                (not (rdf:has-statement? m stmt))))))

(test:test resource.interface.delete-statement.resource-object
  (let* ((object (make-instance 'person :uri '{CL-USER}01 :state rdf:modified-persistent :name "name"))
         (stmt (rdf:triple '{CL-USER}01 '{foaf}firstName "name")))
    (and (rdf:has-statement? object stmt)
         (progn (rdf:delete-statement object stmt)
                (not (rdf:has-statement? object stmt))))))


(test:test resource.interface.equal
  (and (rdf:equal '{rdf}subject '{rdf}subject)
       (not (eq (rdf:triple '{rdf}subject '{rdf}predicate '{rdf}object)
                (rdf:triple '{rdf}subject '{rdf}predicate '{rdf}object)))
       (rdf:equal (rdf:triple '{rdf}subject '{rdf}predicate '{rdf}object)
                  (rdf:triple '{rdf}subject '{rdf}predicate '{rdf}object))
       (rdf:equal (rdf:quad '{rdf}subject '{rdf}predicate '{rdf}object 'graph)
                  (rdf:quad '{rdf}subject '{rdf}predicate '{rdf}object 'graph))))
  

(test:test resource.interface.equal.wilbur.1
  (rdf:equal (wilbur:triple (wilbur:node "http://www.w3.org/1999/02/22-rdf-syntax-ns#subject")
                          (wilbur:node "http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate")
                          (wilbur:node "http://www.w3.org/1999/02/22-rdf-syntax-ns#object"))
             (wilbur:triple (wilbur:node "http://www.w3.org/1999/02/22-rdf-syntax-ns#subject")
                          (wilbur:node "http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate")
                          (wilbur:node "http://www.w3.org/1999/02/22-rdf-syntax-ns#object"))))


(test:test resource.interface.equal.wilbur.2
  (and (rdf:equal (wilbur:node "http://www.w3.org/1999/02/22-rdf-syntax-ns#subject") '{rdf}subject)
       (rdf:equal '{rdf}subject (wilbur:node "http://www.w3.org/1999/02/22-rdf-syntax-ns#subject"))
       (rdf:equal (wilbur:node "http://www.w3.org/1999/02/22-rdf-syntax-ns#subject")
                  (wilbur:node "http://www.w3.org/1999/02/22-rdf-syntax-ns#subject"))))


(test:test resource.interface.has-statement.resource-object
  "Verify just single slot presence."
  (let* ((p (make-instance 'person :name "name" :age 1 :parents nil)))
    (and (rdf:has-statement? p (rdf:triple (uri p) '{foaf}firstName "name"))
         (slot-makunbound p 'name)
         (not (rdf:has-statement? p (rdf:triple (uri p) '{foaf}firstName "name"))))))


(test:test rdf:make-persistent
  :nyi)


(test:test resource.interface.property-missing.1
  (handler-case (progn (property-missing (find-class 'standard-object)
                                         (make-instance 'standard-object)
                                         'property
                                         'property-value)
                       nil)
    (rdf:property-missing-error (c)
                        (and (typep (condition-object c) 'standard-object)
                             (eq (condition-predicate c) 'property)
                             (eq (condition-operation c) 'property-value)))))

(test:test resource.interface.property-missing.2
  (let ((p (make-instance 'person :name "name" :age 1 :parents nil)))
    (typep (nth-value 1 (ignore-errors (property-missing (class-of p) p '{foaf}firstName
                                                         'property-value)))
           'rdf:property-missing-error)))


(test:test resource.interface.property-read-only.2
  (handler-case (progn (property-read-only (find-class 'standard-object)
                                           (make-instance 'standard-object)
                                           'property
                                           'property-value
                                           t)
                       nil)
    (rdf:property-read-only-error (c)
                        (and (typep (condition-object c) 'standard-object)
                             (eq (condition-predicate c) 'property)
                             (eq (condition-operation c) 'property-value)
                             (eq (condition-value c) t)))))

(test:test resource.interface.property-read-only.3
  (let ((p (make-instance 'person :name "name" :age 1 :parents nil)))
    (typep (nth-value 1 (ignore-errors (property-read-only (class-of p) p '{foaf}firstName
                                                           'setf-property-value "asdf")))
           'rdf:property-read-only-error)))


(test:test resource.interface.project-graph
  "Test primitive projection statement->repository. Include statements typcial of an object's
 properties. Verify their presence in the repository."
  (let ((m (repository-mediator 'wilbur-mediator))
        (statements (list (rdf:triple '{rdf}subject '{rdf}predicate '{rdf}object)
                          (rdf:triple '{CL}LIST '{rdf}type '{CL}CLASS)
                          (rdf:triple '{CL-USER}01 '{CL-USER}SLOT-1 "a string")
                          (rdf:triple '{CL-USER}01 '{rdf}type '{RDF}RESOURCE-OBJECT))))
    (rdf:repository-clear m)
    (rdf:project-graph statements m)
    (dolist (stmt statements t)
      (unless (rdf:has-statement? m stmt)
        (return (values nil stmt (wilbur:db-triples (mediator-repository m))))))))


(test:test resource.interface.map-property-slots
  (let ((object (resource :uri '{CL-USER}01
                           :properties (list (prototypal-property-definition :name 'a :predicate :a :value 1)
                                             (prototypal-property-definition :name 'b :predicate :b :value 2))))
        (result ()))
    (rdf:map-property-slots #'(lambda (pd) (push (c2mop:slot-definition-name pd) result)) object)
    (null (set-exclusive-or result '(a b)))))


(test:test resource.interface.map-property-predicates
  (let ((object (resource :uri '{CL-USER}01
                           :properties (list (prototypal-property-definition :name 'a :predicate :a :value 1)
                                             (prototypal-property-definition :name 'b :predicate :b :value 2))))
        (result ()))
    (rdf:map-property-predicates #'(lambda (p) (push p result)) object)
    (null (set-exclusive-or result '(:a :b)))))


(test:test resource.interface.map-property-values
  "Verify iteration over each value and skipping unbound properties"
  (let ((object (resource :uri '{CL-USER}01
                           :properties (list (prototypal-property-definition :name 'a :predicate :a :value 1)
                                             (prototypal-property-definition :name 'b :predicate :b :value '(2 3))
                                             (prototypal-property-definition :name 'c :predicate :c))))
        (result ()))
    (rdf:map-property-values #'(lambda (v) (push v result)) object)
    (null (set-exclusive-or result '(1 2 3) :test #'eql))))


(test:test resource.interface.map-statements
  (let ((object (resource :uri '{CL-USER}01
                           :properties (list (prototypal-property-definition :name 'a :predicate :a :value 1)
                                             (prototypal-property-definition :name 'b :predicate :b :value 2))))
        (result ()))
    (rdf:map-statements #'(lambda (s) (push (copy-triple s) result)) object)
    (null (set-exclusive-or result (list (rdf:triple '{CL-USER}01 :a 1) (rdf:triple '{CL-USER}01 :b 2)) :test #'rdf:equal))))


(test:test reource.interface.model-value
  (let ((ht (make-hash-table ))
        (repository-value '(x)))
    (setf (gethash repository-value ht) :x)
    (and (eq (rdf:model-value ht :a) :a)
         (eql (rdf:model-value nil 1) 1)
         (equal (rdf:model-value nil "a") "a")
         (eq (rdf:model-value ht repository-value) :x)
         (eq (rdf:model-value ht nil) nil)
         (eq (rdf:model-value nil repository-value) nil))))

(test:test resource.interface.query.0
  (let ((object (make-instance 'person :uri '{CL-USER}01 :name "name"
                               :properties (list (prototypal-property-definition :name 'p :predicate :p :value 1)))))
    (flet ((ok (set1 set2) (null (set-exclusive-or set1 set2 :test #'rdf:equal))))
      (list (ok (rdf:query object :subject '{CL-USER}01)
                (list (rdf:triple '{CL-USER}01 '{foaf}firstName "name")
                      (rdf:triple '{CL-USER}01 :p 1)))
            (ok (rdf:query object :predicate '{foaf}firstName)
                (list (rdf:triple '{CL-USER}01 '{foaf}firstName "name")))
            (ok (rdf:query object :predicate :p)
                (list (rdf:triple '{CL-USER}01 :p 1)))
            (ok (rdf:query object :object "name")
                (list (rdf:triple '{CL-USER}01 '{foaf}firstName "name")))
            (ok (rdf:query object :object 1)
                (list (rdf:triple '{CL-USER}01 :p 1)))
            (ok (rdf:query object :subject '{CL-USER}01 :predicate '{foaf}firstName)
                (list (rdf:triple '{CL-USER}01 '{foaf}firstName "name")))
            (ok (rdf:query object :subject '{CL-USER}01 :predicate :p)
                (list (rdf:triple '{CL-USER}01 :p 1)))
            (ok (rdf:query object :subject '{CL-USER}01 :object "name")
                (list (rdf:triple '{CL-USER}01 '{foaf}firstName "name")))
            (ok (rdf:query object :subject '{CL-USER}01 :object 1)
                (list (rdf:triple '{CL-USER}01 :p 1)))))))


(test:test resource.interface.query.1
  (let* ((object (resource :uri '{CL-USER}01))
         (m (object-repository object))
         (statements (list (rdf:triple '{CL-USER}01 '{CL-USER}SLOT-1 "a string")
                           (rdf:triple '{CL-USER}01 '{rdf}type '{RDF}RESOURCE-OBJECT))))
    (rdf:repository-clear m)
    (rdf:project-graph statements m)
    (let ((query (rdf:query m :subject object)))
      (and (null (set-exclusive-or (mapcar #'rdf:predicate statements)
                                   (mapcar #'(lambda (stmt) (rdf:predicate-value m stmt)) query)
                                   :test #'rdf:equal))
           (null (set-exclusive-or (mapcar #'rdf:object statements)
                                   (mapcar #'(lambda (stmt) (rdf:object-value m stmt)) query)
                                   :test #'rdf:equal))))))


(test:test reource.interface.repository-value
  (let ((ht (make-hash-table )))
    (setf (gethash :a ht) :b)
    (eq (rdf:repository-value ht :a) :b)
    (eq (rdf:repository-value ht nil) nil)
    (eq (rdf:repository-value nil :a) nil)))


(test:test resource.interface.unbind-property-slots
  "Verify that unbinding removes asserted properties, but retains the 'this' binding"
  (let ((object (resource :uri '{CL-USER}01
                           :properties (list (prototypal-property-definition :name 'a :predicate :a :value 1)
                                             (prototypal-property-definition :name 'b :predicate :b :value 2))))
        (result ()))
    (rdf:unbind-property-slots object)
    (rdf:map-property-values #'(lambda (v) (push v result)) object)
    (and (null result)
         (eq (rdf:property-value object 'rdf:this) object))))


(test:test resource.interface.uri
  (let ((object (resource :uri '{CL-USER}01)))
    (and (eq (rdf:uri object) '{CL-USER}01)
         (rdf:equal object '{CL-USER}01))))
        

