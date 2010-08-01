;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.resource.implementation; -*-


(in-package :de.setf.resource.implementation)

(:documentation
  "This file defines tests for RDF vocabularies."
 
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
  "'de.setf.resource' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.resource' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.resource' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/)."))

;;; (test:execute-test :resource.vocabulary.**)

(test:test resource.vocabulary.fixtures
  (and (typep *foaf-vocabulary* 'vocabulary)
       (typep *rel-vocabulary* 'vocabulary)))

(test:test resource.vocabulary.find-class
  (and (typep (rdf:find-class *foaf-vocabulary* '{foaf}Person) 'cons)
       (null (rdf:find-class *foaf-vocabulary* '{foaf}Person_x :error-p nil))
       (typep (ignored-error (rdf:find-class *foaf-vocabulary* '{foaf}Person_x))
              'rdf:class-not-found-error)
       t))
