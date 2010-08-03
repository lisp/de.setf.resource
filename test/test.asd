;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: cl-user; -*-

;;; This file is the test system definition for the `de.setf.resource` Common Lisp library.
;;; `de.setf.resource` is a Common Lisp scene model and rendering package.
;;;
;;; Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved
;;; `de.setf.resource` is free software: you can redistribute it and/or modify it under the terms of version 3
;;; of the the GNU Affero General Public License as published by the Free Software Foundation.
;;;
;;; `de.setf.resource` is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
;;; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;; See the the Affero General Public License for more details.
;;;
;;;  A copy of the GNU Affero General Public License should be included with `de.setf.resource`, as `agpl.txt`.
;;; If not, see the GNU [site](http://www.gnu.org/licenses/).


(in-package :cl-user)

(asdf:defsystem :de.setf.resource.test
  :version "0.1"
  :depends-on (:de.setf.resource
               :de.setf.utility.test)

  :serial t
  :description "Tests for rdf model components"
  :components ((:file "fixtures")
               (:file "utilities")
               (:file "statement")
               (:file "api")
               (:file "n3")
               (:file "vocabulary")
               ;; (:file "resource-metaclass")
               (:file "resource-class")
               (:file "resource-object")
               (:file "resource-lifecycle")
               (:file "wilbur-mediator")
               (:file "vocabulary-ts")
               ))



;;; (setf (test:find-test :resource.**) nil)
;;; (asdf:load-system :de.setf.resource :force t)
;;; (asdf:load-system :de.setf.resource.test)
;;; (length (test:find-tests :resource.**))
;;; (test:monitor '(:de.setf.resource :de.setf.resource.implementation))
;;; (test:unmonitor '(:de.setf.resource :de.setf.resource.implementation))
;;; (ccl::clear-method-combination-caches)
;;; (test:execute-test :resource.**)
;;; (test:report-monitor :rdf #p"LIBRARY:de;setf;resource;coverage;index.html" mime:*/html)


#|
;;; removing temporary classes

(defun purge-class-methods (package name)
  (flet ((purge-from-function (designator)
           (let ((function (and (fboundp designator) (fdefinition designator))))
             (when (typep function 'generic-function)
               (dolist (method (c2mop:generic-function-methods function))
                 (when (find-if #'(lambda (spec) (and (typep spec 'class)
                                                      (string-equal (class-name spec) name)))
                                (c2mop:method-specializers method))
                   (format *trace-output* "~&;; ~s" method)
                   (remove-method function method)))))))
    (with-package-iterator (next package :external :internal)
      (loop (multiple-value-bind (next-p symbol) (next)
              (unless next-p (return))
              (purge-from-function symbol)
              (purge-from-function `(setf ,symbol)))))))

(purge-class-methods :de.setf.resource.implementation 'test-person)
(purge-class-methods :de.setf.resource.implementation 'person2)
(purge-class-methods :de.setf.resource 'test-person)



|#