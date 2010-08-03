;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.resource.implementation; -*-


(in-package :de.setf.resource.implementation)

(:documentation
  "This file defines tests for the wilbur interface for the `de.setf.resource` Common Lisp library."
 
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
  "'de.setf.resource' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.resource' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.resource' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/)."))


;;; (test:execute-test :resource.wilbur-mediator.**)

(test:test resource.wilbur-mediator.class
  (let ((mediator (make-instance 'wilbur-mediator)))
    (and (typep mediator 'rdf:repository-mediator)
         (eq (mediator-repository mediator) (wilbur-db)))))


(test:test resource.wilbur-mediator.delete-object
  "verify object enquiry of a wilbur repository for intrinsic slots and augmentation with a property."
  (let* ((p (make-instance 'person :name "name" :age 1 :parents nil))
         (uri (rdf:uri p))
         (m (object-repository p)))
    (setf (property-value p 'height) 100)
    (dolist (s (rdf:query m :subject uri)) (rdf:delete-statement m s))
    (rdf:project-graph p m)
    (and (rdf:has-object? m "name")
         (rdf:has-object? m 1)
         (rdf:has-object? m 100)
         (progn (rdf:delete-object m "name")
                (rdf:delete-object m 1)
                (rdf:delete-object m 100)
                (not (rdf:has-object? m "name"))
                (not (rdf:has-object? m 1))
                (not (rdf:has-object? m 100))))))


(test:test resource.wilbur-mediator.delete-predicate
  "verify subject enquiry of a wilbur repository for intrinsic slots and augmentation with a property."
  (let* ((p (make-instance 'person :name "name" :age 1 :parents nil))
         (uri (rdf:uri p))
         (m (object-repository p)))
    (setf (property-value p 'height) 100)
    (dolist (s (rdf:query m :subject uri)) (rdf:delete-statement m s))
    (rdf:project-graph p m)
    (and (rdf:has-predicate? m '{foaf}firstName)
         (rdf:has-predicate? m '{foaf}age)
         (rdf:has-predicate? m 'height)
         (progn (rdf:delete-predicate m '{foaf}firstName)
                (rdf:delete-predicate m '{foaf}age)
                (rdf:delete-predicate m 'height)
                (not (rdf:has-predicate? m '{foaf}firstName))
                (not (rdf:has-predicate? m '{foaf}age))
                (not (rdf:has-predicate? m 'height))))))

(test:test resource.wilbur-mediator.delete-subject
  "verify subject enquiry of a wilbur repository for intrinsic slots and augmentation with a property."
  (let* ((p (make-instance 'person :name "name" :age 1 :parents nil))
         (uri (rdf:uri p))
         (m (object-repository p)))
    (setf (property-value p 'height) 100)
    (dolist (s (rdf:query m :subject uri)) (rdf:delete-statement m s))
    (rdf:project-graph p m)
    (and (rdf:has-subject? m p)
         (progn (rdf:delete-subject m uri)
                (not (rdf:has-subject? m uri))))))


(test:test resource.wilbur-mediator.delete-statement
  (let ((m (repository-mediator 'wilbur-mediator))
        (s (wilbur:triple (wilbur:node "http://www.w3.org/1999/02/22-rdf-syntax-ns#subject")
                          (wilbur:node "http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate")
                          (wilbur:node "http://www.w3.org/1999/02/22-rdf-syntax-ns#object"))))
    ;; ensure the triple is not there
    (dolist (s (wilbur:db-query (mediator-repository m)
                                (wilbur:triple-subject s)
                                (wilbur:triple-predicate s)
                                (wilbur:triple-object s)))
      (rdf:delete-statement m s))
    ;; then add/delete it and test existence
    (rdf:insert-statement m s)
    (and (rdf:has-statement? m s)
         (progn (rdf:delete-statement m s)
                (null (rdf:has-statement? m s))))))


(test:test resource.wilbur-mediator.find-instance
  "Verify instance registration in the class' mediator. Remove and verify removal."
  (let* ((instance (rdf:ensure-instance 'resource '{http://example.com}self))
         (mediator (object-repository instance)))
    (and (typep instance 'resource-object)
         (eq instance (rdf:find-instance 'resource '{http://example.com}self))
         (eq instance (rdf:find-instance mediator '{http://example.com}self))
         (rdf:equal instance '{http://example.com}self)
         (null (rdf:find-instance mediator '{http://example.com}self_2))
         (null (setf (rdf:find-instance instance '{http://example.com}self) nil))
         (null (rdf:find-instance mediator '{http://example.com}self)))))


(test:test resource.wilbur-mediator.has-statement
  (let ((m (repository-mediator 'wilbur-mediator))
        (s (wilbur:triple (wilbur:node "http://www.w3.org/1999/02/22-rdf-syntax-ns#subject")
                          (wilbur:node "http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate")
                          (wilbur:node "http://www.w3.org/1999/02/22-rdf-syntax-ns#object"))))
    ;; ensure the triple is not there
    (dolist (s (wilbur:db-query (mediator-repository m)
                                (wilbur:triple-subject s)
                                (wilbur:triple-predicate s)
                                (wilbur:triple-object s)))
      (rdf:delete-statement m s))
    ;; then add/delete it and test existence
    (rdf:insert-statement m s)
    (and (rdf:has-statement? m s)
         (progn (rdf:delete-statement m s)
                (null (rdf:has-statement? m s))))))


(test:test resource.wilbur-mediator.has-subject
  "verify subject enquiry of a wilbur repository for intrinsic slots and augmentation with a property."
  (let* ((p (make-instance 'person :name "name" :age 1 :parents nil))
         (s (object-repository p)))
    (setf (property-value p 'height) 100)
    (rdf:project-graph p s)
    (and (rdf:has-subject? s p)
         (rdf:has-subject? s (rdf:uri p))
         (rdf:has-subject? s (wilbur:node (uri-namestring (rdf:uri p)))))))


(test:test resource.wilbur-mediator.has-object
  "verify object enquiry of a wilbur repository for intrinsic slots and augmentation with a property."
  (let* ((p (make-instance 'person :name "name" :age 1 :parents nil))
         (s (object-repository p)))
    (setf (property-value p 'height) 100)
    (rdf:project-graph p s)
    (and (rdf:has-object? s "name")
         (rdf:has-object? s 1)
         (rdf:has-object? s 100))))


(test:test resource.wilbur-mediator.has-predicate
  "verify predicate enquiry of a wilbur repository for intrinsic slots and augmentation with a property."
  (let* ((p (make-instance 'person :name "name" :age 1 :parents nil))
         (s (object-repository p)))
    (setf (property-value p 'height) 100)
    (rdf:project-graph p s)
    (and (rdf:has-predicate? s '{foaf}firstName)
         (rdf:has-predicate? s '{foaf}age)
         (rdf:has-predicate? s 'height))))


(test:test resource.wilbur-mediator.literal.1
  (let ((source (make-instance 'wilbur-mediator)))
    (every #'(lambda (v)  (equal v (rdf:model-value source (rdf:repository-value source v))))
           '(1 "1" 1.0s0 1.0d0))))


(test:test resource.wilbur-mediator.literal.2
  (let ((source (make-instance 'wilbur-mediator)))
    (equal (mapcar #'(lambda (v)  (wilbur:literal-datatype (rdf:repository-value source v)))
                   '(1 "1" 1.0s0 1.0d0))
           '(!xsd:integer  !xsd:string  !xsd:float  !xsd:double))))

(test:test resource.wilbur-mediator.statement.1
  "When placing a native triple, either the context must be provided, or the search must use a wild context."
  (let* ((m (repository-mediator 'wilbur-mediator))
        (new (wilbur:triple (wilbur:node "http://www.w3.org/1999/02/22-rdf-syntax-ns#subject")
                            (wilbur:node "http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate")
                            (wilbur:node "http://www.w3.org/1999/02/22-rdf-syntax-ns#object")
                            (repository-value m (mediator-default-context m)))))
    (wilbur:add-triple new)
    (let ((found (first (rdf:query m :subject '{rdf}subject :predicate '{rdf}predicate :object '{rdf}object))))
      (and found
           (eq (rdf:subject new) (rdf:subject found))
           (eq (rdf:predicate new) (rdf:predicate found))
           (eq (rdf:object new) (rdf:object found))))))

(test:test resource.wilbur-mediator.statement.2
  "When placing a native triple, either the context must be provided, or the search must use a wild context."
  (let* ((m (repository-mediator 'wilbur-mediator))
         (new (wilbur:triple (wilbur:node "http://www.w3.org/1999/02/22-rdf-syntax-ns#subject")
                             (wilbur:node "http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate")
                             (wilbur:node "http://www.w3.org/1999/02/22-rdf-syntax-ns#object"))))
    (wilbur:add-triple new)
    (let ((found (first (rdf:query m :subject '{rdf}subject :predicate '{rdf}predicate :object '{rdf}object
                                   :context nil))))
      (and found
           (eq (rdf:subject new) (rdf:subject found))
           (eq (rdf:predicate new) (rdf:predicate found))
           (eq (rdf:object new) (rdf:object found))))))


(test:test resource.wilbur-mediator.statement.namestring
  (rdf:namestring (wilbur:triple (wilbur:node "http://www.w3.org/1999/02/22-rdf-syntax-ns#subject")
                                 (wilbur:node "http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate")
                                 (wilbur:node "http://www.w3.org/1999/02/22-rdf-syntax-ns#object"))))



(test:test resource.wilbur-mediator.triple
  (let ((s (wilbur:triple 1 "2" !xsd:string)))
    (and (eq (wilbur:triple-subject s) (rdf:subject s))
         (eq (wilbur:triple-predicate s) (rdf:predicate s))
         (eq (wilbur:triple-object s) (rdf:object s))
         (eq (first (wilbur:triple-sources s)) (rdf:context s)))))

