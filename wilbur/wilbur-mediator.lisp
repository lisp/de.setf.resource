;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.resource.implementation; -*-


(in-package :de.setf.resource.implementation)

(:documentation
  "This file mediates access to wilbur RDF stores for the 'de.setf.resource' CLOS linked data library."
 
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
  "'de.setf.resource' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.resource' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.resource' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/).")

 (description "Implement identifier, data, and instance mediation for a wilbur rdf store.

 - use the subject node instance as the identity cache
 - wrap the triple acessors
 - support find for subject based on the node-keyed cache
 - support select for resource-object and triple instances
 - delete based on wilbur:triple and rdf:triple instances"))

(defvar *class.wilbur-db* 'wilbur::edb)

(defvar *model-to-store-datatype-map* (make-hash-table))

(defvar *wilbur-mediator* nil
  "A global binding for a reporitory mediator to support automatic instantiation via the wilbur-mediator function.")

(defun wilbur-db ()
  (or wilbur:*db*
      (setq wilbur:*db* (make-instance *class.wilbur-db*))))

(defclass wilbur-mediator (resource-mediator)
  ((store
    :initarg :db :initform (wilbur-db))))

(defun wilbur-mediator (&rest args)
  "Return the current dynamic binding for the wilbur mediator.
 If not exists instantiate a new one and bind it."
  (if args
    (setq *wilbur-mediator* (apply #'make-instance 'wilbur-mediator args))
    (or *wilbur-mediator*
        (setq *wilbur-mediator* (make-instance 'wilbur-mediator)))))


(defmethod rdf:insert-statement ((source wilbur-mediator) (statement wilbur:triple))
  (wilbur:db-add-triple (repository-store source) statement))


(defmethod rdf:insert-statement ((source wilbur-mediator) (statement rdf:triple))
  (wilbur:db-add-triple (repository-store source)
                        (wilbur:triple (rdf:repository-value source (triple-subject statement))
                                       (rdf:repository-value source (triple-predicate statement))
                                       (rdf:repository-value source (triple-object statement)))))

(defmethod rdf:insert-statement ((source wilbur-mediator) (statement rdf:quad))
  (wilbur:db-add-triple (repository-store source)
                        (wilbur:triple (rdf:repository-value source (triple-subject statement))
                                       (rdf:repository-value source (triple-predicate statement))
                                       (rdf:repository-value source (triple-object statement))
                                       (rdf:repository-value source (quad-context statement)))))

(defmethod rdf:statement-p ((object wilbur:triple))
  t)

(defmethod rdf:clear-repository ((source wilbur-mediator))
  (wilbur:db-clear (repository-store source)))


(defmethod rdf:delete-statement ((source wilbur-mediator) (triple wilbur:triple))
  (wilbur:db-del-triple (repository-store source) triple))


(defmethod rdf:delete-object ((source wilbur-mediator) (object t))
  (let ((db (repository-store source)))
    (dolist (matching-triple (wilbur:db-query db nil nil (rdf:repository-value source object)))
      (wilbur:db-del-triple db matching-triple))))


(defmethod rdf:delete-predicate ((source wilbur-mediator) (predicate t))
  (let ((db (repository-store source)))
    (dolist (matching-triple (wilbur:db-query db nil (rdf:repository-value source predicate) nil))
      (wilbur:db-del-triple db matching-triple))))


(defmethod rdf:delete-statement ((source wilbur-mediator) (statement rdf:triple))
  (let ((db (repository-store source)))
    (dolist (matching-triple (wilbur:db-query db
                                              (rdf:repository-value source (triple-subject statement))
                                              (rdf:repository-value source (triple-predicate statement))
                                              (rdf:repository-value source (triple-object statement))))
      (wilbur:db-del-triple db matching-triple))))


(defmethod rdf:delete-subject ((source wilbur-mediator) (subject t))
  (let ((db (repository-store source)))
    (dolist (matching-triple (wilbur:db-query db (rdf:repository-value source subject) nil nil))
      (wilbur:db-del-triple db matching-triple))))


(defmethod rdf:equal ((n1 wilbur:node) (n2 wilbur:node))
  (or (eq n1 n2)
      (equal (wilbur:node-uri n1) (wilbur:node-uri n2))))


(defmethod rdf:equal ((n1 symbol) (n2 wilbur:node))
  "Implement RDF equality for the symbol x node combination."
  (let ((base (package-name (symbol-package n1)))
        (fragment (symbol-name n1))
        (uri (wilbur:node-uri n2)))
    (and (= (+ (length base) (length fragment)
               (if (uri-has-separator-p base) 0 1))
            (length uri))
         (string-equal base uri :end2 (length base))
         (string-equal fragment uri :start2 (- (length uri) (length fragment))))))

(defmethod rdf:equal ((n1 wilbur:node) (n2 symbol))
  "Implement RDF equality for the symbol x node combination."
  (rdf:equal n2 n1))


(defmethod rdf:equal ((n1 uuid:uuid) (n2 wilbur:node))
  "Implement RDF equality for the UUID x node combination."
  (string-equal (uri-namestring n1) (wilbur:node-uri n2)))

(defmethod rdf:equal ((n1 wilbur:node) (n2 uuid:uuid))
  "Implement RDF equality for the UUID x node combination."
  (rdf:equal n2 n1))


(defmethod rdf:equal ((s1 wilbur:triple) (s2 wilbur:triple))
  "Triple equivalence devolves to testing the constituents."
  (or (eq s1 s2)
      (and (rdf:equal (wilbur:triple-subject s1) (wilbur:triple-subject s2))
           (rdf:equal (wilbur:triple-predicate s1) (wilbur:triple-predicate s2))
           (rdf:equal (wilbur:triple-object s1) (wilbur:triple-object s2)))))


(defmethod rdf:ensure-instance ((metaclass standard-class) (identifier wilbur:node))
  (let ((name (model-value (wilbur-mediator) identifier)))
    (or (find-class name nil)
        (c2mop:ensure-class name :metaclass metaclass
                            :direct-superclasses '({rdfs}Resource)))))


(defmethod rdf:find-instance ((source wilbur-mediator) (designator symbol))
  (rdf:find-instance source (rdf:repository-value source designator)))

(defmethod rdf:find-instance ((source wilbur-mediator) (designator uuid:uuid))
  (rdf:find-instance source (rdf:repository-value source designator)))

(defmethod rdf:find-instance ((source wilbur-mediator) (designator wilbur:node))
  (gethash designator (repository-instance-cache source)))


(defmethod (setf rdf:find-instance) (instance (source wilbur-mediator) (designator symbol))
  (setf (rdf:find-instance source (rdf:repository-value source designator)) instance))

(defmethod (setf rdf:find-instance) (instance (source wilbur-mediator) (designator uuid:uuid))
  (setf (rdf:find-instance source (rdf:repository-value source designator)) instance))

(defmethod (setf rdf:find-instance) ((instance resource-object) (source wilbur-mediator) (designator wilbur:node))
  (setf (gethash designator (repository-instance-cache source)) instance))

(defmethod (setf rdf:find-instance) ((null null) (source wilbur-mediator) (designator wilbur:node))
  (remhash designator (repository-instance-cache source))
  nil)



(defmethod rdf:has-statement? ((source wilbur-mediator) (statement wilbur:triple))
  (wilbur::db-find-triple (repository-store source) statement))


(defmethod rdf:has-statement? ((source wilbur-mediator) (statement rdf:triple))
  (wilbur::db-has-statement? (repository-store source)
                             (rdf:repository-value source (triple-subject statement))
                             (rdf:repository-value source (triple-predicate statement))
                             (rdf:repository-value source (triple-object statement))))


(defmethod rdf:has-subject? ((source wilbur-mediator) (subject t))
  (wilbur::db-has-statement? (repository-store source) (rdf:repository-value source subject) nil nil))


(defmethod rdf:has-predicate? ((source wilbur-mediator) (predicate t))
  (wilbur::db-has-statement? (repository-store source) nil (rdf:repository-value source predicate) nil))


(defmethod rdf:has-object? ((source wilbur-mediator) (object t))
  (wilbur::db-has-statement? (repository-store source) nil nil (rdf:repository-value source object)))


(defmethod rdf:has-statement? ((object resource-object) (statement wilbur:triple))
  (and (rdf:equal (rdf:uri object) (wilbur:triple-subject statement))
       (multiple-value-bind (value exists)
                            (bound-property-value object
                                                  (rdf:model-value object
                                                                   (wilbur:triple-predicate statement)))
         (and exists
              (rdf:equal (wilbur:triple-object statement) value)))))


(defmethod rdf:identifier-p ((object wilbur:node))
  "Guard against literals, as they can include node as a superclass."
  (not (typep object 'wilbur:literal)))


(defmethod rdf::literal-p ((object wilbur:literal))
  "Guard against literals, as they can include node as a superclass."
  t)


(defmethod rdf:repository-persistent? ((repository wilbur-mediator))
  "The wilbur store in an in-memory cache. In order to persist its state, use save-repository."
  nil)

(defmethod rdf:repository-readable? ((repository wilbur-mediator))
  "The wilbur store supports read/write operations."
  t)

(defmethod rdf:repository-transient? ((repository wilbur-mediator))
  "The wilbur store in an in-memory cache. In order to persist its state, use save-repository."
  t)

(defmethod rdf:repository-writable? ((repository wilbur-mediator))
  "The wilbur store supports read/write operations."
  t)     


(defmethod rdf:subject ((statement wilbur:triple))
  (wilbur:triple-subject statement))

(defmethod rdf:subject-value ((source resource-mediator) (statement wilbur:triple))
  (model-value source (wilbur:triple-subject statement)))

(defmethod rdf:predicate ((statement wilbur:triple))
  (wilbur:triple-predicate statement))

(defmethod rdf:predicate-value ((source resource-mediator) (statement wilbur:triple))
  (model-value source (wilbur:triple-predicate statement)))

(defmethod rdf:object ((statement wilbur:triple))
  (wilbur:triple-object statement))

(defmethod rdf:object-value ((source resource-mediator) (statement wilbur:triple))
  (model-value source (wilbur:triple-object statement)))

(defmethod rdf:graph ((statement wilbur:triple))
  (first (wilbur:triple-sources statement)))

(defmethod rdf:type-of ((source resource-mediator) (identifier symbol))
  (assert identifier () "Invalid identifier: ~s." identifier)
  (rdf:type-of source (rdf:repository-value source identifier)))

(defmethod rdf:type-of ((source resource-mediator) (identifier uuid:uuid))
  (rdf:type-of source (rdf:repository-value source identifier)))

(defmethod rdf:type-of ((source resource-mediator) (identifier wilbur:node))
  "Iff the node is cached, return its type, otherwise retrieve the type from the store."
  (or (let ((instance (gethash identifier (repository-instance-cache source))))
        (when instance (type-of instance)))
      (let ((type (first (rdf:query source :subject identifier :predicate '{rdf}type))))
        (when type (rdf:object-value source type)))
      '{rdfs}Resource))

;;; (rdf:type-of (make-instance 'wilbur-mediator) '{foaf}Person)
;;; (let ((m (make-instance 'wilbur-mediator))) (rdf:object-value m (first (rdf:query m :subject '{foaf}Person :predicate '{rdf}type))))
;;; (let ((m (make-instance 'wilbur-mediator))) (rdf:object-value m (first (rdf:query m :subject !foaf:Person :predicate '{rdf}type))))
;;; (gethash !foaf:Person 


(defmethod rdf:load-repository-as ((source wilbur-mediator) (stream stream) (form mime:application/n3))
  (rdf:load-repository-as (repository-store source) stream form))


(defun wilbur-blank-node (id-string)
  (wilbur:node (concatenate 'string *blank-node-prefix* id-string)))


(defmethod rdf:project-graph ((location pathname) (destination wilbur-mediator))
  "Project a graph document onto the repostiory by iterating over read statements
 and inserting each in turn. Establish necessary internting operators to yield the
 store's specific data types."
  (flet ((intern-resource (uri-namestring)
             (wilbur:node uri-namestring))
           (intern-literal (string datatype language)
             (wilbur:literal string :datatype datatype :language language))
           (intern-blank-node (id-string)
             (or (gethash id-string *blank-nodes*)
                 (setf (gethash id-string *blank-nodes*)
                       (wilbur-blank-node id-string)))))
      (let ((n3::*intern-resource* #'intern-resource)
            (n3::*intern-literal* #'intern-literal)
            (n3::*intern-blank-node* #'intern-blank-node)
            (n3::*construct-statement* #'wilbur:triple))
        (call-next-method))))


(defmethod rdf:load-repository-as ((source wilbur-mediator) (location pathname) (form mime:application/rdf+xml))
  (with-open-file (stream location :direction :input :element-type '(unsigned-byte 8))
    (rdf:load-repository-as source stream form)))


(defmethod rdf:load-repository-as ((source wilbur-mediator) (stream stream) (form mime:application/rdf+xml))
  (wilbur:parse-db-from-stream stream ""))


(defmethod rdf:repository-count ((source wilbur-mediator))
  (wilbur::db-count-triples (repository-store source)))


(defmethod rdf:repository-namespace-bindings ((source wilbur-mediator))
  (wilbur:dictionary-namespaces wilbur:*nodes*))


(defmethod rdf:project-graph ((quad rdf:quad) (destination wilbur-mediator))
  (let ((triple (wilbur:triple (rdf:repository-value destination (rdf:quad-subject quad))
                               (rdf:repository-value destination (rdf:quad-predicate quad))
                               (rdf:repository-value destination (rdf:quad-object quad))
                               (rdf:repository-value destination (rdf:quad-context quad)))))
    (wilbur:db-add-triple (repository-store destination) triple)
    triple))

(defmethod rdf:project-graph ((quad rdf:triple) (destination wilbur-mediator))
  (let ((triple (wilbur:triple (rdf:repository-value destination (rdf:quad-subject quad))
                               (rdf:repository-value destination (rdf:quad-predicate quad))
                               (rdf:repository-value destination (rdf:quad-object quad)))))
    (wilbur:db-add-triple (repository-store destination) triple)
    triple))

(defmethod rdf:project-graph ((statement wilbur:triple) (object resource-object))
  (when (rdf:equal (wilbur:triple-subject statement) (uri object))
    (rdf:insert-statement object statement)))

(defmethod rdf:project-graph ((source wilbur-mediator) (destination function))
  (map nil destination (wilbur:db-triples (repository-store source))))


(defmethod store-uri ((source wilbur-mediator) (uri string))
  (wilbur:node uri))

(defmethod store-uri ((source wilbur-mediator) (uri t))
  (rdf:repository-value source uri))


(defmethod rdf:repository-value ((source t) (value wilbur:node))
  value)

(defmethod rdf:repository-value ((source t) (value wilbur:literal))
  value)

(defmethod rdf:repository-value ((source wilbur-mediator) (value string))
  (wilbur:literal value :datatype !xsd:string))

(defmethod rdf:repository-value ((source wilbur-mediator) (value float))
  (wilbur:literal (princ-to-string value) :value value :datatype !xsd:float))

(defmethod rdf:repository-value ((source wilbur-mediator) (value double-float))
  (wilbur:literal (princ-to-string value) :value value :datatype !xsd:double))

(defmethod rdf:repository-value ((source wilbur-mediator) (value integer))
  (wilbur:literal (princ-to-string value) :value value :datatype !xsd:integer))

(defmethod rdf:repository-value ((source wilbur-mediator) (value symbol))
  (flet ((canonicalize (symbol) (canonicalize-identifier source symbol)))
    (declare (dynamic-extent #'canonicalize))
    (if (symbol-package value)
      (wilbur:node (symbol-uri-namestring value #'canonicalize))
      (wilbur-blank-node (symbol-name value)))))

(defmethod rdf:repository-value ((source wilbur-mediator) (identifier uuid:uuid))
  (wilbur:node (uri-namestring identifier)))

#+de.setf.xml
(defmethod rdf:repository-value ((source wilbur-mediator) (identifier xqdm:uname))
  (let ((uri-base (xqdm:namespace-name (xqdm:namespace identifier))))
    (wilbur:node (concatenate 'string uri-base
                              (unless (uri-has-separator-p uri-base) "/")
                              (xqdm:local-part identifier)))))


(defmethod rdf:model-value ((source wilbur-mediator) (value wilbur:literal))
  (wilbur:literal-value value))

(defmethod rdf:model-value ((source wilbur-mediator) (value wilbur:node))
  (flet ((canonicalize (fragment)
           (canonicalize-identifier source fragment)))
    (declare (dynamic-extent #'canonicalize))
    (let ((namestring (wilbur:node-uri value)))
      (if (string-equal "_:" namestring :end2 2)
        (make-symbol (subseq namestring 2))
        (uri-namestring-identifier namestring #'canonicalize)))))


(defmethod rdf:uri-match-p ((node wilbur:node) object)
  (uri-match-p (wilbur:node-uri node) object))

(defmethod rdf:uri-match-p (object (node wilbur:node))
  (uri-match-p object (wilbur:node-uri node)))

(defmethod uri-namestring ((object wilbur:node))
  (wilbur:node-uri object))

  

(defmethod n3:format ((object wilbur:triple) stream  &optional colon at var)
  (declare (ignore colon var))
  (unless at (write-char #\space stream))
  (n3:print-property (wilbur:triple-subject object) stream)
  (write-char #\space stream)
  (n3:print-property (wilbur:triple-predicate object) stream)
  (write-char #\space stream)
  (n3:print-property (wilbur:triple-object object) stream)
  (unless at (write-string " ." stream))
  object)

(defmethod n3:print-property ((object wilbur:node) stream)
  (let ((namestring (wilbur:node-uri object)))
    (if (and (> (length namestring) (length *blank-node-prefix*))
             (string-equal *blank-node-prefix* namestring :end2 (length *blank-node-prefix*)))
      (write-string namestring stream)
      (format stream "<~a>" namestring))))

(defmethod rdf:namestring ((statement wilbur:triple))
    (with-output-to-string (stream)
      (format stream "~@/n3:format/" statement)))
