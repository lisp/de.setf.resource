;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.resource.implementation; -*-


(in-package :de.setf.resource.implementation)

(:documentation
  "This file defines tests for utilities in the `de.setf.resource` Common Lisp library."
 
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
  "'de.setf.resource' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.resource' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.resource' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/)."))

;;; (test:execute-test :resource.utilities.**)


(test:test resource.pathname-type-mime-type
  "Performs an explicit of known mime types."
  (mapcar #'location-mime-type (mapcar #'first  *pathname-type-mime-type*))
  (list mime:application/n3 mime:application/n3 mime:application/n3
        mime:application/rdf+xml  mime:application/rdf+xml))


(test:test resource.utilities.set
  (and (typep '(1 2) '(cons number))
       (typep '(1 a) '(cons number))
       (not (typep "a" '(cons number)))))

(test:test resource.utilities.resource-subtypep
  (and (not (resource-subtypep 'string))
       (resource-subtypep 'resource-object)
       (resource-subtypep '(cons resource-object))
       (resource-subtypep '(or resource-object (cons resource-object)))))

(test:test resource.utilities.base-type
  (mapcar #'base-type
          '(nil string (cons string) (or resource-object (cons resource-object))
            (or fixnum (cons float))))
  '(nil string string resource-object fixnum))


(test:test resource.utilities.format-url-encoded
  (list (format nil ">~/format-url-encoded/<" "http://host.domain/dir/")
        (format nil ">~/format-url-encoded/<" "host.domain"))
  '(">http%3A%2F%2Fhost.domain%2Fdir%2F<"
    ">host.domain<"))

(test:test resource.utilities.uri-vocabulary-components
  (uri-vocabulary-components "http://www.w3.org/0000/02/22-rdf-syntax-ns#type")
  (find-package "http://www.w3.org/0000/02/22-rdf-syntax-ns#") "type")


(test:test resource.utilities.uri-match-p
  (and (uri-match-p "http://host.domain/dir/" "http://host.domain/")
       (not (uri-match-p "http://host.domain/" "http://host.domain/dir/"))))
