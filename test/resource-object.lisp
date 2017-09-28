;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.resource.implementation; -*-


(in-package :de.setf.resource.implementation)

(:documentation
  "This file tests the de.setf.resource resource-object class"
 
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
  "'de.setf.resource' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.resource' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.resource' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/)."))

;;; (test:execute-test :resource.resource-object.**)

(test:test resource.resource-object.0
  (eq (find-class '{rdfs}Resource) (find-class 'resource)))

(test:test resource.resource-object.1
  (let ((person-class (find-class 'person))
        (adult-class (find-class 'adult)))
    (and (equal (de.setf.rdf:class-property-slots person-class) (de.setf.rdf:class-property-slots adult-class))
          (every #'(lambda (sd) 
                     (let* ((ssd (slot-definition-statement-slot sd))
                            (ssd-psd (slot-definition-property-slot ssd)))
                       (and ssd ssd-psd (eq (c2mop:slot-definition-name sd) (c2mop:slot-definition-name ssd-psd)))))
                 (de.setf.rdf:class-property-slots person-class))
          (every #'(lambda (sd) (typep sd 'rdf:archetypal-property-definition))
                 (de.setf.rdf:class-property-slots person-class))
          (null (set-exclusive-or '(name age parents)
                                  (mapcar #'slot-definition-name
                                          (de.setf.rdf:class-property-slots person-class))))
          (null (set-exclusive-or '({foaf}firstName {foaf}age {rel}childof)
                                  (mapcar #'slot-definition-predicate
                                          (de.setf.rdf:class-property-slots person-class)))))))


(test:test resource.resource-object.readers
  (let ((person (make-instance 'person :name "a person" :age 17)))
    ;; working with resources, one does not want to drag uri around in their full dress.
    ;; it should be sufficient to designate them with an internal handle.
    ;; for this one uses a symbol.
    
    (list (object-state person) (person-name person) (person-age person)))
  (list de.setf.rdf:TRANSIENT "a person" 17))


(test:test resource.resource-object.n3.1
  (let ((person (make-instance 'person :name "a person" :age 17 :parents nil)))
    ;; working with resources, one does not want to drag uri around in their full dress.
    ;; it should be sufficient to designate them with an internal handle.
    ;; for this one uses a symbol.
    (with-output-to-string (stream) (de.setf.rdf:map-statements #'(lambda (stmt) (print stmt stream)) person))))


(test:test resource.resource-object.has-statement
  "verify statement enquiry for intrinsic slots and augmentation with a property."
  (let* ((p (make-instance 'person :name "name" :age 1 :parents nil)))
    (and (setf (property-value p 'height) 100)
         (de.setf.rdf:has-statement? p (de.setf.rdf:triple (uri p) '{foaf}firstName "name"))
         (null (de.setf.rdf:has-statement? p (de.setf.rdf:triple (uri p) '{foaf}firstName "other name")))
         (de.setf.rdf:has-statement? p (de.setf.rdf:triple (uri p) '{foaf}age 1))
         (null (de.setf.rdf:has-statement? p (de.setf.rdf:triple (uri p) '{foaf}parents nil)))
         (null (de.setf.rdf:has-statement? p (de.setf.rdf:triple (uri p) 'parents nil)))
         (de.setf.rdf:has-statement? p (de.setf.rdf:triple (uri p) '{rel}childof nil))
         (de.setf.rdf:has-statement? p (de.setf.rdf:triple (uri p) 'height 100))
         (null (de.setf.rdf:has-statement? p (de.setf.rdf:triple (uri p) 'height 200)))
         (null (de.setf.rdf:has-statement? p (de.setf.rdf:triple (uri p) 'weight 100))))))


(test:test resource.resource-object.delete-statement
  :nyi)
(test:test resource.resource-object.delete-subject
  :nyi)
(test:test resource.resource-object.delete-predicate
  :nyi)
(test:test resource.resource-object.delete-object
  :nyi)


(test:test resource.resource-object.has-subject
  "verify statement enquiry for intrinsic slots and augmentation with a property."
  (let* ((p (make-instance 'person :name "name" :age 1 :parents nil)))
    (and (setf (property-value p 'height) 100)
         (de.setf.rdf:has-subject? p p)
         (de.setf.rdf:has-subject? p (de.setf.rdf:uri p))
         (de.setf.rdf:has-subject? p (wilbur:node (uri-namestring (de.setf.rdf:uri p)))))))


(test:test resource.resource-object.has-object
  "verify statement enquiry for intrinsic slots and augmentation with a property."
  (let* ((p (make-instance 'person :name "name" :age 1 :parents nil)))
    (and (setf (property-value p 'height) 100)
         (de.setf.rdf:has-object? p "name")
         (de.setf.rdf:has-object? p 1)
         (de.setf.rdf:has-object? p 100))))

(test:test resource.resource-object.has-predicate
  "verify statement enquiry for intrinsic slots and augmentation with a property."
  (let* ((p (make-instance 'person :name "name" :age 1 :parents nil)))
    (and (setf (property-value p 'height) 100)
         (de.setf.rdf:has-predicate? p '{foaf}firstName)
         (de.setf.rdf:has-predicate? p '{foaf}age)
         (de.setf.rdf:has-predicate? p 'height))))

