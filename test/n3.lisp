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

;;; (test:execute-test :resource.n3.**)

(test:test resource.n3.format
  (list (format nil "~/n3:format/" (list '{http://example.com}resource '{http://example.com}property "value"))
        (format nil "~/n3:format/" (list '{http://example.com}resource '{http://example.com}property 1))
        (format nil "~/n3:format/" (list '{http://example.com}resource '{http://example.com}property 1.0d0))
        (format nil "~/n3:format/" (list '{http://example.com}resource '{http://example.com}property 1.0s0))
        (format nil "~/n3:format/" (list '{http://example.com}resource '{rdf}type '{rdf}type))
        )
  `(" <http://example.com/resource> <http://example.com/property> \"value\" ."
    " <http://example.com/resource> <http://example.com/property> \"1\"^^<http://www.w3.org/2001/XMLSchema#integer> ."
    ,(format nil " <http://example.com/resource> <http://example.com/property> \"~s\"^^<http://www.w3.org/2001/XMLSchema#double> ." 1.0d0)
    ,(format nil " <http://example.com/resource> <http://example.com/property> \"~s\"^^<http://www.w3.org/2001/XMLSchema#float> ." 1.0s0)
    " <http://example.com/resource> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> ."))


(test:test resource.n3.read.0
  (let ((*readtable* n3:*readtable*))
    (mapcar #'read-from-string
            '(" \"string\" "
              " \"\" "
              " \"\"\"string\"\"\" "
              " \"123\"^^<http://www.w3.org/2001/XMLSchema#integer>"
              " \"\"\"123\"\"\"^^<http://www.w3.org/2001/XMLSchema#integer>"
              " \"\\n\\t\\'\\\"\\u0009\" ")))
  `("string" "" "string" 123 123 ,(concatenate 'string #(#\newline #\tab #\' #\" #\tab))))


(test:test resource.n3.read.2
  (let ((statements (list (triple '{http://example.com}resource '{http://example.com}property "value")
                          (triple '{http://example.com}resource '{http://example.com}property 1)
                          (triple '{http://example.com}resource '{http://example.com}property 1.0d0)
                          (triple '{http://example.com}resource '{http://example.com}property 1.0s0)
                          (triple '{http://example.com}resource '{rdf}type '{rdf}type))))
    (let ((result ()))
      (with-input-from-string (stream (with-output-to-string (stream)
                                        (format stream "~{~/n3:format/~%~}" statements)))
        (loop (let ((spo (n3:read stream nil nil)))
                (if spo (push (apply #'triple spo) result) (return)))))
      (equalp statements (nreverse result)))))
