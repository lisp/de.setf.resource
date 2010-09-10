;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.resource.implementation; -*-


(in-package :de.setf.resource.implementation)

(:documentation
  "This file mediates access to wilbur RDF repositories for the 'de.setf.resource' CLOS linked data library."
 
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
  "'de.setf.resource' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.resource' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.resource' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/).")

 (description "Implement identifier, data, and instance mediation for a wilbur rdf repository.

 - use the subject node instance as the identity cache
 - wrap the triple acessors
 - support find for subject based on the node-keyed cache
 - support select for resource-object and triple instances
 - delete based on wilbur:triple and rdf:triple instances"))

(defvar *class.wilbur-db* 'wilbur::edb)

(defvar *model-to-repository-datatype-map* (make-hash-table))

(defvar *wilbur-mediator* nil
  "A global binding for a reporitory mediator to support automatic instantiation via the wilbur-mediator function.")

(defun wilbur-db ()
  (or wilbur:*db*
      (setq wilbur:*db* (make-instance *class.wilbur-db*))))

(defclass wilbur-mediator (repository-mediator)
  ((repository
    :initarg :db :initform (wilbur-db))
   (persistent
    :initform nil :allocation :class
    :documentation "The wilbur repository in an in-memory cache. In order to persist its state,
     use save-repository.")
   (readable
    :initform t :allocation :class
    :documentation "The wilbur repository supports read/write operations.")
   (writable
    :initarg :writable :initform t :allocation :class
    :documentation "The wilbur repository supports read/write operations.")
   (maps-dynamic-extent
    :initform nil :allocation :class
    :documentation "wilbur maps over instances directly from the db.")))


(defun wilbur-mediator (&rest args)
  "Return the current dynamic binding for the wilbur mediator.
 If not exists instantiate a new one and bind it."
  (if args
    (setq *wilbur-mediator* (apply #'make-instance 'wilbur-mediator args))
    (or *wilbur-mediator*
        (setq *wilbur-mediator* (make-instance 'wilbur-mediator)))))



#+:wilbur-triples-as-classes
(defmethod copy-statement ((statement wilbur:triple))
  (make-instance 'wilbur:triple
    :subject (wilbur:triple-subject statement)
    :predicate (wilbur:triple-predicate statement)
    :object (wilbur:triple-object statement)
    ;; reuse the sources?
    :sources (wilbur:triple-sources statement)))

#-:wilbur-triples-as-classes
(defmethod copy-statement ((statement wilbur:triple))
  (wilbur:copy-triple statement))


;;;
;;;

(defmethod add-statement* ((mediator wilbur-mediator) subject predicate object context)
  "Constuct and add the statement to the repository db. Leaves duplication check to db-add-triple."
  (wilbur:db-add-triple (mediator-repository mediator)
                        (wilbur:triple (repository-value mediator subject)
                                       (repository-value mediator predicate)
                                       (repository-value mediator object)
                                       (repository-value mediator context))))


(defmethod rdf:context ((statement wilbur:triple))
  (first (wilbur:triple-sources statement)))


(defmethod rdf:delete-statement ((mediator wilbur-mediator) (triple wilbur:triple))
  "Given an actual wilbur:triple, remove it as-is."
  (wilbur:db-del-triple (mediator-repository mediator) triple))


(defmethod rdf:delete-statement ((destination repository-mediator) (statement wilbur:triple))
  "A method to deconstruct wilbur triples for deletion elsewhere."
  (let* ((wm (wilbur-mediator))
         (contexts (loop for  source in (wilbur:triple-sources statement)
                         collect (model-value wm source))))
    (dolist (context (or contexts (list (mediator-default-context destination))))
      (delete-statement* destination
                         (model-value wm (wilbur:triple-subject statement))
                         (model-value wm (wilbur:triple-predicate statement))
                         (model-value wm (wilbur:triple-object statement))
                         context))))


(defmethod rdf:delete-object ((mediator wilbur-mediator) (object t))
  (let ((db (mediator-repository mediator)))
    (dolist (matching-triple (wilbur:db-query db nil nil (rdf:repository-value mediator object)))
      (wilbur:db-del-triple db matching-triple))))


(defmethod rdf:delete-predicate ((mediator wilbur-mediator) (predicate t))
  (let ((db (mediator-repository mediator)))
    (dolist (matching-triple (wilbur:db-query db nil (rdf:repository-value mediator predicate) nil))
      (wilbur:db-del-triple db matching-triple))))


(defmethod rdf:delete-statement ((mediator wilbur-mediator) (statement rdf:triple))
  (let ((db (mediator-repository mediator)))
    (dolist (matching-triple (wilbur:db-query db
                                              (rdf:repository-value mediator (triple-subject statement))
                                              (rdf:repository-value mediator (triple-predicate statement))
                                              (rdf:repository-value mediator (triple-object statement))))
      (wilbur:db-del-triple db matching-triple))))


(defmethod rdf:delete-subject ((mediator wilbur-mediator) (subject t))
  (let ((db (mediator-repository mediator)))
    (dolist (matching-triple (wilbur:db-query db (rdf:repository-value mediator subject) nil nil))
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
               (if (uri-intrinsic-separator base) 0 1))
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


(defmethod rdf:find-instance ((mediator wilbur-mediator) (designator symbol))
  (rdf:find-instance mediator (rdf:repository-value mediator designator)))

(defmethod rdf:find-instance ((mediator wilbur-mediator) (designator uuid:uuid))
  (rdf:find-instance mediator (rdf:repository-value mediator designator)))

(defmethod rdf:find-instance ((mediator wilbur-mediator) (designator wilbur:node))
  (gethash designator (mediator-instance-cache mediator)))


(defmethod (setf rdf:find-instance) (instance (mediator wilbur-mediator) (designator symbol))
  (setf (rdf:find-instance mediator (rdf:repository-value mediator designator)) instance))

(defmethod (setf rdf:find-instance) (instance (mediator wilbur-mediator) (designator uuid:uuid))
  (setf (rdf:find-instance mediator (rdf:repository-value mediator designator)) instance))

(defmethod (setf rdf:find-instance) ((instance resource-object) (mediator wilbur-mediator) (designator wilbur:node))
  (setf (gethash designator (mediator-instance-cache mediator)) instance))

(defmethod (setf rdf:find-instance) ((null null) (mediator wilbur-mediator) (designator wilbur:node))
  (remhash designator (mediator-instance-cache mediator))
  nil)


(defmethod rdf:has-statement? ((object resource-object) (statement wilbur:triple))
  (and (rdf:equal (rdf:uri object) (wilbur:triple-subject statement))
       (multiple-value-bind (value exists)
                            (bound-property-value object
                                                  (rdf:model-value object
                                                                   (wilbur:triple-predicate statement)))
         (and exists
              (rdf:equal (wilbur:triple-object statement) value)))))

(defmethod rdf:has-statement? ((mediator wilbur-mediator) (statement wilbur:triple))
  "Given the repository's native statement, look directly"
  (wilbur::db-find-triple (mediator-repository mediator) statement))


(defmethod rdf:has-statement? ((mediator repository-mediator) (statement wilbur:triple))
  "Deconstruct wilbur triples for search elsewhere."
  (flet ((probe (statement)
           (declare (ignore statement))
           (return-from rdf:has-statement? t)))
    (declare (dynamic-extent #'probe))
    (let* ((wm (wilbur-mediator))
           (m-subject (model-value wm (wilbur:triple-subject statement)))
           (m-predicate (model-value wm (wilbur:triple-predicate statement)))
           (m-object (model-value wm (wilbur:triple-object statement)))
           (contexts (loop for  source in (wilbur:triple-sources statement)
                           collect (model-value wm source))))
      (if contexts
        (dolist (m-context contexts)
          (map-statements* #'probe mediator m-subject m-predicate m-object m-context))
        (map-statements* #'probe mediator m-subject m-predicate m-object nil)))))


(defmethod rdf:id ((triple wilbur:triple))
  nil)


(defmethod rdf:identifier-p ((object wilbur:node))
  "Guard against literals, as they can include node as a superclass."
  (not (typep object 'wilbur:literal)))


(defmethod rdf:insert-statement ((mediator wilbur-mediator) (statement wilbur:triple))
  "Given an actual wilbur:triple, add it directly as-is."
  (wilbur:db-add-triple (mediator-repository mediator) statement))

(defmethod rdf:insert-statement ((destination repository-mediator) (statement wilbur:triple))
  "A method to deconstruct wilbur triples for insertion elsewhere."
  (let* ((wm (wilbur-mediator))
         (contexts (loop for  source in (wilbur:triple-sources statement)
                         collect (model-value wm source))))
    (dolist (context (or contexts (list (mediator-default-context destination))))
      (add-statement* destination
                      (model-value wm (wilbur:triple-subject statement))
                      (model-value wm (wilbur:triple-predicate statement))
                      (model-value wm (wilbur:triple-object statement))
                      context))))


(defmethod rdf::literal-p ((object wilbur:literal))
  "Guard against literals, as they can include node as a superclass."
  t)


;;; (rdf:type-of (make-instance 'wilbur-mediator) '{foaf}Person)
;;; (let ((m (make-instance 'wilbur-mediator))) (rdf:object-value m (first (rdf:query m :subject '{foaf}Person :predicate '{rdf}type))))
;;; (let ((m (make-instance 'wilbur-mediator))) (rdf:object-value m (first (rdf:query m :subject !foaf:Person :predicate '{rdf}type))))
;;; (gethash !foaf:Person 


(defmethod rdf:load-repository-as ((mediator wilbur-mediator) (stream stream) (form mime:application/n3))
  (rdf:load-repository-as (mediator-repository mediator) stream form))


(defun wilbur-blank-node (id-string)
  (wilbur:node (concatenate 'string *blank-node-prefix* id-string)))


(defmethod rdf:load-repository-as ((mediator wilbur-mediator) (location pathname) (form mime:application/rdf+xml))
  (with-open-file (stream location :direction :input :element-type '(unsigned-byte 8))
    (rdf:load-repository-as mediator stream form)))


(defmethod rdf:load-repository-as ((mediator wilbur-mediator) (stream stream) (form mime:application/rdf+xml))
  (wilbur:parse-db-from-stream stream ""))


(defmethod rdf:statement-p ((object wilbur:triple))
  t)

(defmethod rdf:repository-clear ((mediator wilbur-mediator))
  (wilbur:db-clear (mediator-repository mediator)))


(defmethod map-statements* (continuation (mediator wilbur-mediator) subject predicate object context)
  "wilbur statement iteration depends on the db implementation - with or without indices.
 The values are mapped here, as the espective db is passed on."
  (map-statements* continuation (mediator-repository mediator)
                   (repository-value mediator subject)
                   (repository-value mediator predicate)
                   (repository-value mediator object)
                   (repository-value mediator context)))


(defmethod rdf:model-value ((mediator wilbur-mediator) (value wilbur:literal))
  (wilbur:literal-value value))

(defvar *uri-symbols* t)

(defmethod rdf:model-value ((mediator wilbur-mediator) (value wilbur:node))
  (flet ((canonicalize (fragment)
           (canonicalize-identifier mediator fragment)))
    (declare (dynamic-extent #'canonicalize))
    (let ((namestring (wilbur:node-uri value)))
      (cond ((null namestring)
             (make-symbol ""))
            ((string-equal "_:" namestring :end2 2)
             (make-symbol (subseq namestring 2)))
            (*uri-symbols*
             (uri-namestring-identifier namestring #'canonicalize))
            (t
             (puri:uri namestring))))))


(defmethod rdf:predicate ((statement wilbur:triple))
  (wilbur:triple-predicate statement))

(defmethod rdf:predicate-value ((mediator repository-mediator) (statement wilbur:triple))
  (model-value mediator (wilbur:triple-predicate statement)))

(defmethod rdf:object ((statement wilbur:triple))
  (wilbur:triple-object statement))

(defmethod rdf:object-value ((mediator repository-mediator) (statement wilbur:triple))
  (model-value mediator (wilbur:triple-object statement)))


(defmethod rdf:project-graph ((location pathname) (destination wilbur-mediator))
  "Project a graph document onto the repostiory by iterating over read statements
 and inserting each in turn. Establish necessary internting operators to yield the
 repository's specific data types."
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


(defmethod rdf:project-graph ((statement wilbur:triple) (destination repository-mediator))
  "A method to deconstruct wilbur triples for insertion elsewhere."
  (let* ((wm (wilbur-mediator))
         (contexts (loop for  source in (wilbur:triple-sources statement)
                         collect (model-value wm source))))
    (dolist (context (or contexts (list (mediator-default-context destination))))
      (add-statement* destination
                      (model-value wm (wilbur:triple-subject statement))
                      (model-value wm (wilbur:triple-predicate statement))
                      (model-value wm (wilbur:triple-object statement))
                      context))))


(defmethod rdf:project-graph ((triple wilbur:triple) (destination wilbur-mediator))
  "Given an actual wilbur:triple, add it as-is."
  (wilbur:db-add-triple (mediator-repository destination) triple)
  triple)


(defmethod rdf:project-graph ((statement wilbur:triple) (object resource-object))
  (when (rdf:equal (wilbur:triple-subject statement) (uri object))
    (rdf:insert-statement object statement)))

(defmethod rdf:project-graph ((mediator wilbur-mediator) (destination function))
  (map nil destination (wilbur:db-triples (mediator-repository mediator))))


(defmethod rdf:repository-count ((mediator wilbur-mediator))
  (wilbur::db-count-triples (mediator-repository mediator)))


(defmethod rdf:repository-namespace-bindings ((mediator wilbur-mediator))
  (wilbur:dictionary-namespaces wilbur:*nodes*))


(defmethod rdf:subject ((statement wilbur:triple))
  (wilbur:triple-subject statement))

(defmethod rdf:subject-value ((mediator repository-mediator) (statement wilbur:triple))
  (model-value mediator (wilbur:triple-subject statement)))

(defmethod rdf:type-of ((mediator repository-mediator) (identifier symbol))
  (assert identifier () "Invalid identifier: ~s." identifier)
  (rdf:type-of mediator (rdf:repository-value mediator identifier)))

(defmethod rdf:type-of ((mediator repository-mediator) (identifier uuid:uuid))
  (rdf:type-of mediator (rdf:repository-value mediator identifier)))

(defmethod rdf:type-of ((mediator repository-mediator) (identifier wilbur:node))
  "Iff the node is cached, return its type, otherwise retrieve the type from the repository."
  (or (let ((instance (gethash identifier (mediator-instance-cache mediator))))
        (when instance (type-of instance)))
      (let ((type (first (rdf:query mediator :subject identifier :predicate '{rdf}type))))
        (when type (rdf:object-value mediator type)))
      '{rdfs}Resource))


(defmethod repository-uri ((mediator wilbur-mediator) (uri string))
  (wilbur:node uri))

(defmethod repository-uri ((mediator wilbur-mediator) (uri t))
  (rdf:repository-value mediator uri))


(defmethod rdf:repository-value ((mediator t) (value wilbur:node))
  value)

(defmethod rdf:repository-value ((mediator t) (value wilbur:literal))
  value)

(defmethod rdf:repository-value ((mediator wilbur-mediator) (value string))
  (wilbur:literal value :datatype !xsd:string))

(defmethod rdf:repository-value ((mediator wilbur-mediator) (value float))
  (wilbur:literal (princ-to-string value) :value value :datatype !xsd:float))

(defmethod rdf:repository-value ((mediator wilbur-mediator) (value double-float))
  (wilbur:literal (princ-to-string value) :value value :datatype !xsd:double))

(defmethod rdf:repository-value ((mediator wilbur-mediator) (value integer))
  (wilbur:literal (princ-to-string value) :value value :datatype !xsd:integer))

(defmethod rdf:repository-value ((mediator wilbur-mediator) (value symbol))
  (flet ((canonicalize (symbol) (canonicalize-identifier mediator symbol)))
    (declare (dynamic-extent #'canonicalize))
    (if (symbol-package value)
      (wilbur:node (symbol-uri-namestring value #'canonicalize))
      (wilbur-blank-node (symbol-name value)))))

(defmethod rdf:repository-value ((mediator wilbur-mediator) (identifier uuid:uuid))
  (wilbur:node (uri-namestring identifier)))

#+de.setf.xml
(defmethod rdf:repository-value ((mediator wilbur-mediator) (identifier xqdm:uname))
  (let ((uri-base (xqdm:namespace-name (xqdm:namespace identifier))))
    (wilbur:node (concatenate 'string uri-base
                              (unless (uri-has-separator-p uri-base) "/")
                              (xqdm:local-part identifier)))))


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


;;;
;;; transaction support
;;;
;;; delegate to the database to distinguish implementation for locked v/s unlocked databases.

(defmethod nbfeb-load ((mediator wilbur-mediator) location-id)
  "Delegate to the db"
  (nbfeb-load (mediator-repository mediator) (repository-value mediator location-id)))


(defmethod nbfeb-sac ((mediator wilbur-mediator) location-id value)
  "Delegate to the db"
  (nbfeb-sac (mediator-repository mediator)
             (repository-value mediator location-id) (repository-value mediator value)))


(defmethod nbfeb-sas ((mediator wilbur-mediator) location-id value)
  "Delegate to the db"
  (nbfeb-sas (mediator-repository mediator)
             (repository-value mediator location-id) (repository-value mediator value)))


(defmethod nbfeb-tfas ((mediator wilbur-mediator) location-id value)
  "Delegate to the db"
  (nbfeb-tfas (mediator-repository mediator)
              (repository-value mediator location-id) (repository-value mediator value)))


(thrift:def-struct "nbfebLocation"
  "The variable cell combines a value with a full/empty bit."
  (("feb" nil :type thrift:bool :id 1)
   ("value" nil :type thrift:binary :id 2)))


(defun decode-nbfeb-state (data)
  (let ((feb nil) (value nil) (extras nil))
    (with-input-from-vector-stream (stream :vector data)
      (thrift.implementation::decode-struct stream 'nbfeb-location
                                            ((feb nil :id 1 :type thrift:bool)
                                             (value nil :id 2 :type thrift:binary))
                                            extras))
    (values (uuid:byte-array-to-uuid value) feb)))


(defun encode-nbfeb-state (value flag)
  (with-output-to-vector-stream (stream)
    (thrift:stream-write-struct stream (thrift:list (cons value value) (cons feb flag)) 'nbfeb-location)))