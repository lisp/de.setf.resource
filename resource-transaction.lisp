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


(defmethod abort-transaction (mediator)
  (transaction-step mediator (mediator-state mediator) transaction-abort))

(defmethod commit-transaction (mediator)
  (transaction-step mediator (mediator-state mediator) transaction-commit))

(defmethod open-transaction (mediator)
  (transaction-step mediator (mediator-state mediator) transaction-open))

(defgeneric transaction-evict (mediator object)
  (:documentation "Remove the object from the transactional cache.")

  (:method ((mediator repository-mediator) object)
    (remhash object (mediator-transaction-cache mediator))))

(defgeneric transaction-register (mediator object)
  (:documentation "Register the object and state in the transaction cache.")

  (:method ((mediator repository-mediator) object)
    (setf (gethash object (mediator-transaction-cache mediator)) (object-state object))))



(defgeneric transaction-step (mediator start-state end-state)
  (:method ((mediator repository-mediator) (start t) (end t))
    (invalid-state-error :object mediator :start-state start :end-state end))

  (:method ((mediator repository-mediator) (start non-transactional) (end transaction-commit))
    ;; ignore it
    end)

  (:method ((mediator repository-mediator) (start transaction-open) (end transaction-commit))
    "Iterate twice over the transaction cache. In the first pass write all modified object properties. Once
 that has succeeded, update the object states to either hollow or non-transactional. Finally, clear the cache."
    (setf-mediator-state end mediator)
    ;; push any changes (or deletion) to the repository
    (maphash #'write-properties-in-state (mediator-transaction-cache mediator))
    ;; then mark comitted
    (maphash #'commit-in-state (mediator-transaction-cache mediator))
    (clrhash (mediator-transaction-cache mediator))
    (setf-mediator-state non-transactional mediator))

  (:method ((mediator repository-mediator) (start non-transactional) (end transaction-open))
    (setf-mediator-state end mediator))

  (:method ((mediator repository-mediator) (start non-transactional) (end transaction-abort))
    ;; ignore it
    end)

  (:method ((mediator repository-mediator) (start transaction-open) (end transaction-abort))
    (setf-mediator-state end mediator)
    (maphash #'rollback-in-state (mediator-transaction-cache mediator))
    (clrhash (mediator-transaction-cache mediator))
    (setf-mediator-state non-transactional mediator)))

(defmacro rdf:with-transaction ((mediator) &body body)
  (let ((op (cons-symbol nil :transaction))
        (repository-var (cons-symbol nil :mediator)))
    `(flet ((,op () ,@body))
       (let ((,repository-var ,mediator))
         (typecase (mediator-state ,repository-var)
           (transactional (,op))
           (t (unwind-protect (progn (open-transaction ,repository-var)
                                     (multiple-value-prog1 (,op)
                                       (commit-transaction ,repository-var)))
                (typecase (mediator-state ,repository-var)
                  (transactional (abort-transaction mediator))))))))))
