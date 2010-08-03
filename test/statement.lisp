;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.resource.implementation; -*-

(in-package :de.setf.resource.implementation)

(:documentation
  "This file defines tests for primitive n3 encoding in the `de.setf.resource` Common Lisp library."
 
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
  "'de.setf.resource' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.resource' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.resource' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/)."))


;;; (test:execute-test :resource.statement.**)


(test:test resource.statement.instantiation
  (and (triple-p (make-triple :subject 'subject :predicate 'predicate :object 'object :id 'id))
       (triple-p (rdf:triple 'subject 'predicate 'object))
       (triple-p (make-quad :subject 'subject :predicate 'predicate :object 'object :context nil))
       (quad-p (make-quad :subject 'subject :predicate 'predicate :object 'object :context nil))
       (quad-p (rdf:quad 'subject 'predicate 'object 'context))))

(test:test resource.statement.valid
  (and (valid? (make-triple :subject 'subject :predicate 'predicate :object 'object))
       (not (valid? (make-triple :subject 'subject :predicate 'predicate :object nil)))))

(test:test resource.statement.namestring
  (rdf:namestring (make-triple :subject "uri" :predicate "uri" :object "data")))

(test:test resource.statement.accessors
  (let ((s (rdf:quad 'subject 'predicate 'object 'context 'id)))
    (and (eq (quad-subject s) (rdf:subject s))
         (eq (quad-predicate s) (rdf:predicate s))
         (eq (quad-object s) (rdf:object s))
         (eq (quad-context s) (rdf:context s))
         (eq (quad-id s) (rdf:id s)))))

(test:test resource.statement.assertion.type
  (every #'(lambda (s) (typecase s (statement t) (t nil)))
         (list (make-triple :subject 'subject :predicate 'predicate :object 'object)
               (make-quad :subject 'subject :predicate 'predicate :object 'object :context nil))))

