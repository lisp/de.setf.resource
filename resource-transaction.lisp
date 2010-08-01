;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.resource.implementation; -*-


(in-package :de.setf.resource.implementation)

(:documentation
  "This file implements resource transactions for the `de.setf.resource` CLOS linked data library."
 
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
  "'de.setf.resource' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.resource' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.resource' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/)."))


;;; life-cycle suport


(defmethod abort-transaction (source)
  (transaction-step source (repository-state source) transaction-abort))

(defmethod commit-transaction (source)
  (transaction-step source (repository-state source) transaction-commit))

(defmethod open-transaction (source)
  (transaction-step source (repository-state source) transaction-open))

(defgeneric transaction-evict (source object)
  (:documentation "Remove the object from the transactional cache.")

  (:method ((source resource-mediator) object)
    (remhash object (repository-transaction-cache source))))

(defgeneric transaction-register (source object)
  (:documentation "Register the object and state in the transaction cache.")

  (:method ((source resource-mediator) object)
    (setf (gethash object (repository-transaction-cache source)) (object-state object))))



(defgeneric transaction-step (source start-state end-state)
  (:method ((source resource-mediator) (start t) (end t))
    (invalid-state-error :object source :start-state start :end-state end))

  (:method ((source resource-mediator) (start non-transactional) (end transaction-commit))
    ;; ignore it
    end)

  (:method ((source resource-mediator) (start transaction-open) (end transaction-commit))
    "Iterate twice over the transaction cache. In the first pass write all modified object properties. Once
 that has succeeded, update the object states to either hollow or non-transactional. Finally, clear the cache."
    (setf-repository-state end source)
    ;; push any changes (or deletion) to the repository
    (maphash #'write-properties-in-state (repository-transaction-cache source))
    ;; then mark comitted
    (maphash #'commit-in-state (repository-transaction-cache source))
    (clrhash (repository-transaction-cache source))
    (setf-repository-state non-transactional source))

  (:method ((source resource-mediator) (start non-transactional) (end transaction-open))
    (setf-repository-state end source))

  (:method ((source resource-mediator) (start non-transactional) (end transaction-abort))
    ;; ignore it
    end)

  (:method ((source resource-mediator) (start transaction-open) (end transaction-abort))
    (setf-repository-state end source)
    (maphash #'rollback-in-state (repository-transaction-cache source))
    (clrhash (repository-transaction-cache source))
    (setf-repository-state non-transactional source)))

(defmacro rdf:with-transaction ((source) &body body)
  (let ((op (cons-symbol nil :transaction))
        (repository-var (cons-symbol nil :source)))
    `(flet ((,op () ,@body))
       (let ((,repository-var ,source))
         (typecase (repository-state ,repository-var)
           (transactional (,op))
           (t (unwind-protect (progn (open-transaction ,repository-var)
                                     (multiple-value-prog1 (,op)
                                       (commit-transaction ,repository-var)))
                (typecase (repository-state ,repository-var)
                  (transactional (abort-transaction source))))))))))
