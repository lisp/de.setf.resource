;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.resource.implementation; -*-


(in-package :de.setf.resource.implementation)

(:documentation
  "This file defines tests for the persistence life-cycle for property objects."
 
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
  "'de.setf.resource' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.resource' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.resource' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/).")

 (:description "The tests are arranged to exercise the interface life-cycle operators directly, to cover the
 state-specific operators indirectly through the interface, and to cover the internal methods  through
 detailed tests."))


;;; (test:execute-test :resource.resource-lifecycle.**)


(test:test resource.resource-lifecycle.commit
  :nyi)

(test:test resource.resource-lifecycle.evict
  :nyi)

(test:test resource.resource-lifecycle.delete
  :nyi)

(test:test resource.resource-lifecycle.make-persistent
  :nyi)

(test:test resource.resource-lifecycle.make-transient
  :nyi)

(test:test resource.resource-lifecycle.modify
  :nyi)

(test:test resource.resource-lifecycle.read-properties
  :nyi)

(test:test resource.resource-lifecycle.refresh
  :nyi)

(test:test resource.resource-lifecycle.rollback
  :nyi)

(test:test resource.resource-lifecycle.write-properties
  :nyi)

(test:test resource.resource-lifecycle.make-hollow
  :nyi)

(test:test resource.resource-lifecycle.project-graph.resource-object.0
  (let* ((p (make-instance 'person :name "name" :age 1 :parents nil)))
    (rdf:project-graph (list (rdf:triple (uri p) 'height 200)
                             (rdf:triple (uri p) 'weight 70))
                       p)
    (and (eql (person-height p) 200)
         (eql (person-weight p) 70))))

(test:test resource.resource-lifecycle.project-graph.resource-object.1
  (let* ((p (make-instance 'person :name "name" :age 1 :parents nil))
         (values ())
         (expected '("name" 1 200 70)))
    (rdf:project-graph (list (rdf:triple (uri p) 'height 200)
                             (rdf:triple (uri p) 'weight 70))
                       p)
    (rdf:project-graph p #'(lambda (s) (push (triple-object s) values)))
    (null (set-exclusive-or values expected :test 'equal))))

(test:test resource.resource-lifecycle.project-graph.resource-object.2
  (let* ((uri (uuid:make-v1-uuid))
         (class (rdf:project-graph (list (rdf:triple uri '{foaf}firstName "also a name")
                                         (rdf:triple uri '{foaf}age 2)
                                         (rdf:triple uri 'height 200)
                                         (rdf:triple uri 'weight 70))
                                   (find-class 'person)))
         (object (rdf:find-instance class uri))
         (expected '("also a name" 2 200 70))
         (values ()))
    (rdf:project-graph object #'(lambda (s) (push (triple-object s) values)))
    (null (set-exclusive-or values expected :test 'equal))))

(test:test resource.resource-lifecycle.project-graph.resource-object.2
  (let* ((p (make-instance 'person :name "name" :age 1 :parents nil))
         (source (object-repository p)))
    (rdf:project-graph (list (rdf:triple (uri p) 'height 200)
                             (rdf:triple (uri p) 'weight 70))
                       p)
    (rdf:project-graph p source)
    (every #'(lambda (stmt) (rdf:has-statement? source stmt))
           (list (rdf:triple p '{foaf}firstName "name")
                 (rdf:triple p '{foaf}age 1)
                 (rdf:triple p 'height 200)
                 (rdf:triple p 'weight 70)))))


(test:test resource.resource-lifecycle.project-graph.resource-class
  "Project a three-instance graph from the repository to the model; test atomic slots and
 the simple parent relation. The model does not specify inverse relations.
 Since {foaf}Person is the datatype of the declared PERSON class, for which the PARENTS slot is
 declared of type (CONS PERSON), no statement yiels the result NIL."
   (progn (rdf:find-class 'person '{foaf}Person)
          (rdf:find-class 'person '{foaf}Agent)
          (let ((s (class-repository '{foaf}Person)))
            (load-family s)
            (let ((people (rdf:project-graph s (find-class '{foaf}Person))))
              (list (= (length people) 3)
                    (null (set-exclusive-or (mapcar #'person-name people) '("baby" "dad" "mom") :test #'equal))
                    (null (set-exclusive-or (mapcar #'person-age people) '(1 21 22) :test #'equal))
                    (null (set-exclusive-or (mapcar #'person-height people) '(50 200 180) :test #'equal))
                    (null (set-exclusive-or (mapcar #'person-weight people) '(10 70 60) :test #'equal))
                    (let ((baby (find "baby" people :key #'person-name :test #'equal))
                          (parents (remove  "baby" people :key #'person-name :test #'equal)))
                      (and (= (length parents) 2)
                           ;; (print parents)
                           ;; (print (person-parents baby))
                            (null (set-exclusive-or parents (person-parents baby)))
                            (notany #'person-parents parents))))))))



;;; (map nil 'print (wilbur:db-triples wilbur:*db*))

;;; (let ((m (class-repository 'person))) (model-value m (rdf-value m 'weight)))
;;; (unregister-value (class-repository 'person) 'weight !"DE.SETF.RESOURCE.IMPLEMENTATION/WEIGHT")
;;; (unregister-value (class-repository 'person) 'height !"DE.SETF.RESOURCE.IMPLEMENTATION/HEIGHT")

;; (eq (wilbur::db-make-literal wilbur::*db* "10") (wilbur::db-make-literal wilbur::*db* "10"))
;; (eq (wilbur::node "http://x") (wilbur::node "http://x"))

;; (map nil 'print (wilbur:db-triples wilbur:*db*))
;; (wilbur:db-clear wilbur:*db*)
;; (wilbur:db-query wilbur:*db* !"urn:uuid:91282580-72a2-11df-908e-0c19b5f7e401" !foaf:name nil)
;; (wilbur:db-query wilbur:*db* !"urn:uuid:91282580-72a2-11df-908e-0c19b5f7e401" !foaf:name (wilbur:triple-object (first *)))
