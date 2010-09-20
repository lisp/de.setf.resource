;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.resource.implementation; -*-


(in-package :de.setf.resource.implementation)

(:documentation
  "This file defines model classes and operators for the `de.setf.resource` Common Lisp linked data library."
 
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
  "'de.setf.resource' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.resource' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.resource' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/)."))


(defclass resource-object (standard-object)
  ((uri
    :initarg :uri
    :reader get-object-uri :writer setf-object-uri
    :type (or uuid:uuid symbol))
   (repository
    :initform nil :initarg :repository
    :reader get-object-repository
    :documentation "Binds the repository for the instance. The default is the class repository.")
   (state
    :initform rdf:transient :initarg :state
    :reader object-state :writer setf-object-state
    :documentation "Records the instance lifecycle state. Initially :transient. If it is created as a
     consequence of a persistent reference, that is changed to :hollow. a persistent-new reference propagates.
     Any :hollow access causes a transition to the respective modified state. Write access causes it to become
     modified. Read acess on a transient or new unbound slot fails. make-object-persistent,
     make-object-transient, and delete-object cause transitions between the realms for autonomous instances.
     Transaction completion synchronizes any persistent objects with the database. It examines each object
     created in the transaction, synchronizes all persistent ones and each object reachable from one. Any
     reachable transents are changed to persistent.
     see [JDO](http://www.jpox.org/docs/jdo/jdo_state_transition.html).")
   (graph
     :initform nil :initarg :graph :initarg :context
     :accessor object-graph :accessor object-context
     :documentation "Specifies the individual graph, within the object's repository, which comprises the object's
      description. The default value is nil, which means that associations will be retrieved from all graphs
      and the object is unversioned. If bound to a graph URI (as per an rdfs:isDefinedBy property), then each
      time the object is written, a new document version is generated and a new relation is written to that
      one in the context of the transaction.")
   (history
    :initform ()
    :reader object-history :writer setf-object-history
    :documentation "Records the persistent property changes during a transaction.")
   (properties
    :initform nil :type (or null hash-table)
    :reader get-object-properties :writer setf-object-properties
    :documentation "Bind the property-slot-definition metaobjects which bind non-slot properties for
     structural variations and prototypes."))
  (:metaclass abstract-resource-class)
  (:repository t)
  (:compute-uri-function compute-object-uuid)
  (:property-missing-function rdf:property-missing)

  (:documentation "The resource-object class describes the abstract features of 'resource' linked data entities.
 Each object comprises an identifier - either a symbol or a UUID, a repository -either directly, or by
 delegation through its class, a complement of archetypal properties and a complement of prototypal properties.
 Archetypal properties are those slots for which a datatype or a predicate is declared.
 Prototypal properties are those additional values which are associated with an instance through a property
 accessor or projected on to the instance from a repository.

 In addition to the instance's owned properties, each instance is associated with a repository datatype, and
 as a default, a repository by delegation to its class.
 
 When operated on in the context of a transaction, a resource-object instance retains an history which can be used
 to effect a roll-back or to purge obsolete assersions from its repository.

 As the class is an abstract-resource-class, it is intended to be specialized rather than instantiated.
 See the resource class for the most general concrete class."))


(defclass resource (resource-object)
  ()
  (:metaclass resource-class)
  (:datatype {rdfs}Resource)

  (:documentation "The resource class is the most general instantiable for of resource-object.
 As it s the most general form, it specifies the {rdfs}Resource datatype."))


(setf (find-class '{owl}Thing) (find-class 'resource-object))

(defclass {rdfs}Literal () ()
  (:documentation "Serves as a stop-gap for repository content which spcifies an explicit {rdfs}Literal class."))


(defmethod shared-initialize :after ((instance resource-object) (slots t) &key properties)
  "If a list of properties is provided when (re)initializing a resource-object, bind them to the
 respective predicates in the instance. Any un-named previous bindings will remain."

  (when properties
    (let ((p-table (object-properties instance)))
      (dolist (pd properties)
        (setf (gethash (slot-definition-predicate pd) p-table) pd)))))


(defmethod print-object ((object resource-object) stream)
  (let ((id (when (slot-boundp object 'uri) (get-object-uri object))))
    (if *print-readably*
      (format stream "~s" id)
      (print-unreadable-object (object stream :identity t :type t)
        (write id :stream stream)))))


(def-class-constructor resource)


(defmethod n3:print-property ((object resource-object) stream)
  "Encode a resource-object instance in n-triple as its URI."
  (n3:print-property (rdf:uri object) stream))


(defmethod rdf:uri ((object resource-object))
  "Return the object's URI and compute a default value if necessary by delegation to compute-object-uri."
  (if (slot-boundp object 'uri)
    (get-object-uri object)
    (setf-object-uri (compute-object-uri object) object)))


(defgeneric compute-object-uri (object)
  (:documentation "Given an object, compute and return a URI for it.")
 
  (:method ((object resource-object))
    "The base method for resource-object instances delegates to the class."
    (compute-object-uri-with-class (class-of object) object)))


(defmethod rdf:ensure-instance ((object resource-object) identifier)
  (rdf:ensure-instance (class-of object) identifier))


(defmethod rdf:model-value ((mediator repository-mediator) (object resource-object))
  "In the context of a repository, the model domain value of a resource-object instance
 is the instance itself."
  object)


(defmethod rdf:model-value ((object resource-object) identifier)
  "In the context of a resource-object, resolve a model value by delegating to the respective class."
  (rdf:model-value (class-of object) identifier))


(defmethod rdf:repository-value :around ((mediator repository-mediator) (object resource-object))
  "In the context of a repository, a resource-object is identified with its URI.
 This is present as an :around method to delegate immediately to the URI in order that it appear in the
 repositories cache in relation to the concrete repository-value."
  (rdf:repository-value mediator (rdf:uri object)))


(defmethod compute-object-uuid ((object resource-object))
  "The general resource identifier is a UUID. This function serves as the base value for the resource object
 class' compute-uri-function attribute. It generates a new UUID for each invocation."
  (uuid:make-v1-uuid))


(defmethod rdf:equal ((object resource-object) (uri t))
  "Given a resource-object and something else, the result is that for the instance's URI.
 This equates event two distinct resource-object instances which have equivalent URI"
  (rdf:equal (rdf:uri object) uri))


(defmethod rdf:equal ((uri t) (object resource-object))
  "Given a resource-object and something else, the result is that for the instance's URI.
 This equates event two distinct resource-object instances which have equivalent URI"
  (rdf:equal (rdf:uri object) uri))


(defmethod object-repository ((object resource-object))
  "Given a RESOURCE-OBJECT, return its immediate repository or delegate to its class if none
 was specified."
  (or (get-object-repository object)
      (class-repository (class-of object))))


(defmethod rdf:prototypal-property-definition ((object resource-object) &rest initargs)
    "The primary method for resource-object instances delegates to the respective class."
    (declare (dynamic-extent initargs))
    (apply #'prototypal-property-definition (class-of object) initargs))

(defgeneric rdf:property-value-using-class (class resource-object name)
  (:documentation "Given a CLASS a RESOURCE-OBJECT instance, and a predicate NAME, return
 the value bound in the instance context. If the predicate names an archetypal slot, this
 is equivalent to slot value, but in the predicate rather than the slot name namespace.
 Otherwise a bound prototypal property is sought and its value is returned. If no
 property exists, the base protocol invokes class' class-property-missing-function on the class,
 the instance, and the predicate name. The standard bindings invoke property-missing, which
 signals a property-missing-error.")

  (:method ((class resource-class) (object resource-object) predicate)
    "First, if the object is persistent, but uninitialized, load it's properties.
     The locate an property definition - whether arcetypal or prototypal and return the value.
     Absent a definition signal a property-missing-error."
    (typecase (object-state object)
      (rdf:hollow (read-properties object)))
    (let ((definition  (find-archetypal-property-definition-by-predicate class predicate)))
      (cond (definition
             (let* ((reader (slot-definition-reader definition)))
               (funcall reader object)))
            (t
             (rdf:prototypal-property-value object predicate))))))


(defgeneric rdf:property-value (object predicate)
  (:documentation "Return the value associated with the object by the predicate. This devolves to
 a slot reference and signals a resource-missing error if the predicate is not related to the
 object.")
  (:method ((object resource-object) predicate)
    (property-value-using-class (class-of object) object predicate)))


(defgeneric (setf rdf:property-value-using-class) (new-value class object predicate)
  (:method ((new-value t) (class resource-class) object predicate)
    (let ((definition (find-archetypal-property-definition-by-predicate class predicate)))
      (cond (definition
             (let* ((writer (slot-definition-writer definition)))
               (funcall writer new-value object)))
            (t
             (setf (prototypal-property-value object predicate) new-value))))))

(defgeneric (setf rdf:property-value) (value object predicate)
  (:documentation "Set the value associated with the object by the predicate. This devolves to
 a slot reference and signals a resource-missing error if the predicate is not related to the
 object.")
  (:method (new-value (object resource-object) predicate)
    (setf (property-value-using-class (class-of object) object predicate) new-value)))


(defgeneric bound-property-value-using-class (class object predicate)
  (:method ((class resource-class) object predicate)
    (typecase (object-state object)
      (rdf:hollow (read-properties object)))
    (let ((definition  (find-archetypal-property-definition-by-predicate class predicate)))
      (cond (definition
              (if (slot-boundp object (c2mop:slot-definition-name definition))
                (values (funcall (slot-definition-reader definition) object) t)
                (values nil t)))
            (t
             (bound-prototypal-property-value object predicate))))))

(defgeneric bound-property-value (object predicate)
  (:documentation "Return the value associated with the object by the predicate. This devolves to
 a slot reference and signals a resource-missing error if the predicate is not related to the
 object.")
  (:method ((object resource-object) predicate)
    (bound-property-value-using-class (class-of object) object predicate)))


(defmethod unbind-property-slots progn ((object resource-object))
  "The base method for the unbinding operator removes property slot definitions except for those
 which bind internal properties."
  (let ((properties (get-object-properties object)))
    (when properties
      (maphash #'(lambda (key pd)
                   (unless (typep pd 'rdf-internal-property-definition)
                     (remhash key properties)))
               properties))))


;; nb. these are augmented with class-specific methods which handle the direct slots for
;; the respective class

(defmethod rdf:map-property-slots progn (function (object resource-object))
  "The base method for a resource object applies the operator to each property slot definition."
  (let ((properties (get-object-properties object)))
    (when properties
      (loop for pd being the hash-values of properties
            unless (typep pd 'rdf-internal-property-definition)
            do (funcall function pd)))))


(defmethod rdf:map-property-values progn (function (object resource-object))
  "The base method for a resource object applies the operator to the values of each property slot definition."
  (let ((properties (get-object-properties object)))
    (when properties
      (loop for pd being each hash-value of properties
            unless (typep pd 'rdf-internal-property-definition)
            when (slot-boundp pd 'value)
            do (rdf:map-collection function (slot-definition-value pd))))))


(defmethod rdf:map-property-predicates progn (function (object resource-object))
  "The base method for a resource object applies the operator to the predicate of each property slot definition."
  (let ((properties (get-object-properties object)))
    (when properties
      (loop for pd being each hash-value of properties
            unless (typep pd 'rdf-internal-property-definition)
            do (rdf:map-collection function (slot-definition-predicate pd))))))



(:documentation  object-properties prototypal-property-value
  "The resource-object class represents prototypal, ad-hoc properties as prototypal-property-definition
 instances. These are collected in the instance's properties slot. This is an on-demand hash-table managed
 by the object-properties reader. If no binding is found, by default, reads fail and writes augment the
 bindings.")


(defgeneric object-properties (resource-object)
  (:documentation "Return the resource-object instance's prototypal property registry.
 If none was yet refereneced, create a new registry with the 'this' bound to the instance.")

  (:method ((object resource-object))
    (or (get-object-properties object)
        (let ((properties (make-hash-table )))
          (setf (gethash 'rdf:this properties)
                (rdf-internal-property-definition :name 'rdf:this :value object))
          (setf-object-properties properties object)))))


(defgeneric rdf:prototypal-property-value (resource-object name &optional type)
  (:documentation "Given a RESOURCE-OBJECT instance and a predicate NAME, return the prototypal property value.
 If none is bound, signal a continuable property-missing error to permit the application to furnish or create a
 property definition. Accept an optional TYPE to incorporate when instantiating a new property.")

  (:method ((object resource-object) name &optional (type nil))
    ;; if hollow, perform the read _before_ looking for a property definition
    (typecase (object-state object)
      (rdf:hollow (read-properties object)))
    ;; then return the property or signal an error if none is found
    (let ((definition (find-prototypal-property-definition object name)))
      (etypecase definition
        (null
         (restart-case (funcall (class-property-missing-function (class-of object))
                                (class-of object) object name 'prototypal-property-value)
           (ignore ()
                   :report (lambda (stream)
                             (format stream "Ignore the operation, return no values."))
                   (values))
           (make-definition (&key value (name name) (type type))
                            :report (lambda (stream)
                                      (format stream "Create a property definition and continue."))
                            (setf (find-prototypal-property-definition object name)
                                  (rdf:prototypal-property-definition object :name name :value value :type type))
                            (prototypal-property-value object name))
           (use-definition (definition)
                           :report (lambda (stream)
                                     (format stream "Supply a property definition and continue."))
                           (assert (typep definition 'rdf:prototypal-property-definition) ()
                                   "Invalid property definition: ~s." definition)
                           (setf (find-prototypal-property-definition object name) definition)
                           (rdf:prototypal-property-value object name))
           (use-value (value)
                      :report (lambda (stream)
                                (format stream "Specify a value to return from the operation"))
                      value)))
        (rdf:prototypal-property-definition
         (slot-definition-value definition))))))


(defgeneric (setf prototypal-property-value) (new-value object name &optional type)
  (:documentation "Given a RESOURCE-OBJECT instance, a predicate NAME, and a NEW-VALUE, set the named prototypal
 property value. If none exists invoke property-missing, with setf-property-value specified. By default
 this immediately invokes the continuation to create the property definition. In addition provide continuations
 to ignore the operation or to ise a specific property definition.")

  (:method (new-value (object resource-object) name &optional (type t))
    ;; ensure the slot or property definition exists and then
    ;; check to read properties for a hollow instance
    (let ((definition (find-prototypal-property-definition object name)))
      (etypecase definition
        (null
         (restart-case (funcall (class-property-missing-function (class-of object))
                                (class-of object) object name 'rdf:setf-property-value)
           (ignore ()
                   :report (lambda (stream)
                             (format stream "Ignore the operation, return no values."))
                   new-value)
           (make-definition (&key (name name) (type type))
                            :report (lambda (stream)
                                      (format stream "Create a property definition and continue."))
                            (setf (find-prototypal-property-definition object name)
                                  (rdf:prototypal-property-definition object :name name :type type))
                            (setf (prototypal-property-value object name) new-value))
           (use-definition (definition)
                           :report (lambda (stream)
                                     (format stream "Supply a property definition and continue."))
                           (assert (typep definition 'rdf:prototypal-property-definition) ()
                                   "Invalid property definition: ~s." definition)
                           (setf (find-prototypal-property-definition object name) definition)
                           (setf (prototypal-property-value object name) new-value))))
        (rdf:prototypal-property-definition 
         (typecase (object-state object)
           (rdf:hollow (read-properties object)))
         (setf (slot-definition-value definition) new-value)))))

  (:method (new-value (object resource-object) (definition rdf:prototypal-property-definition)
                      &optional (type (c2mop:slot-definition-type definition)))
    "Given a specific property definition for an object and a NEW-VALUE, setf the property value."
    (unless (slot-definition-writable definition)
      (property-read-only (class-of object) object definition 'rdf:setf-property-value new-value))
    (when type
      (assert (typep new-value type) () "Invalid slot value: ~a (~a): ~a."
              (c2mop:slot-definition-name definition) type new-value))
    (setf (slot-definition-value definition) new-value)))


(defun setf-prototypal-property-value (new-value object name)
  (setf (prototypal-property-value object name) new-value))


(defgeneric bound-prototypal-property-value (object name)
  (:documentation "Given a resource object and a predicate name, return two values:
 The property value or nil if it is not present or not bound, and it is present but nil if not.")
 
  (:method ((object resource-object) name)
    ;; if hollow, perform the read _before_ looking for a property definition
    (typecase (object-state object)
      (rdf:hollow (read-properties object)))
    ;; then return the property or signal an error if none is found
    (let ((definition (find-prototypal-property-definition object name)))
      (etypecase definition
        (null
         (values nil nil))
        (rdf:prototypal-property-definition
         (if (slot-boundp definition 'value)
           (values (slot-definition-value definition) t)
           (values nil t)))))))


#+(or) ;; managed by specializing property-missing for the operator
(defgeneric setf-prototypal-property-value! (new-value object name)
  (:method (new-value (object resource-object) name)
    ;; ensure the slot or property definition exists and then
    ;; check to read properties for a hollow instance
    (let ((definition (find-prototypal-property-definition object name)))
      (unless definition
        (setf (find-prototypal-property-definition object name)
              (setf definition
                    (rdf:prototypal-property-definition object :name name))))
      (typecase (object-state object)
        (rdf:hollow (read-properties object)))
      (setf (slot-definition-value definition) new-value))))


(defgeneric property-boundp (object property-definition)
  (:documentation "Given a RESOURCE-OBJECT instance and a property definition, return true iff
 the property is bound.")

  (:method ((object resource-object) (definition rdf:prototypal-property-definition))
    (slot-boundp definition 'value))
  (:method ((object resource-object) (definition rdf:archetypal-property-definition))
    (slot-boundp object (c2mop:slot-definition-name definition))))


(defgeneric bound-property-slots (instance)
  (:method ((object resource-object))
    (let ((slots ()))
      (flet ((collector (sd)
               (when (property-boundp object sd) (push sd slots))))
        (rdf:map-property-slots #'collector object))
      slots)))


(defgeneric rdf:retain-values? (resource)
  (:documentation "Return true iff the resource should retain property values subsequent to having
 been evicted from a transaction. The default value is NIL.")

  (:method ((object resource-object))
    "The base method universally returns NIL."
    nil))


(defgeneric find-prototypal-property-definition (environment name)
 (:documentation "Search the environment and return the first slot definition which matches the name.
 The environment is represented as a tree of environment hashtables.
 If no definition is present, return nil.")

 (:method ((environment null) (name t))
   nil)
 (:method ((environment cons) (name t))
   (or (find-prototypal-property-definition (first environment) name)
       (find-prototypal-property-definition (rest environment) name)))

 (:method ((environment hash-table) (name t))
   (gethash name environment))

 (:method ((object resource-object) (name t))
   ;; delegates either to the hash-table or the null method, depending on whether
   ;; any properties have been set
   (find-prototypal-property-definition (get-object-properties object) name)))


(defgeneric (setf find-prototypal-property-definition) (definition environment name)
 (:documentation "Search the environment and return the first slot definition which matches the name.
 The environment is represented as a tree of environment hashtables.
 If no definition is present, return nil.")

 (:method (definition (environment cons) (name t))
   (setf (find-prototypal-property-definition (first environment) name) definition))

 (:method (definition (environment hash-table) (name t))
   (setf (gethash name environment) definition))

 (:method (definition (object resource-object) (name t))
   (setf (find-prototypal-property-definition (object-properties object) name) definition)))


(defmethod find-archetypal-property-definition-by-name ((object resource-object) (name t))
  "Delegate to the class to search for the slot definition."

  (find-archetypal-property-definition-by-name (class-of object) name))


(defmethod find-archetypal-property-definition-by-predicate ((object resource-object) (predicate t))
  "Delegate to the class to search for the slot definition."

  (find-archetypal-property-definition-by-predicate (class-of object) predicate))


;;;
;;; rdf enumeration interface

(defmethod rdf:has-statement? ((object resource-object) (statement rdf:triple))
  (and (rdf:equal (rdf:uri object) (triple-subject statement))
       (multiple-value-bind (value exists)
                            (bound-property-value object (triple-predicate statement))
         (and exists
              (rdf:equal (triple-object statement) value)))))


(defmethod rdf:has-context? ((object resource-object) context)
  (rdf:equal (object-graph object) context))


(defmethod rdf:has-object? ((resource-object resource-object) object)
  (labels ((test-value (value)
             (when (typecase value
                     (cons (member object value :test #'rdf:equal))
                     (t (rdf:equal object value)))
               (return-from rdf:has-object? t)))
           (test-slot (sd)
             (etypecase sd
               (rdf:prototypal-property-definition
                (when (slot-boundp sd 'value)
                  (test-value (slot-definition-value sd))))
               (rdf:archetypal-property-definition
                (when (slot-boundp resource-object (c2mop:slot-definition-name sd))
                  (test-value (funcall (slot-definition-reader sd) resource-object)))))))
    (declare (dynamic-extent #'test-slot))
    (map-property-slots #'test-slot resource-object)))


(defmethod rdf:has-predicate? ((object resource-object) predicate)
  "Test the presences of the predicate among the resource slots - not all slots, and
 against any bound properties. exclude unbound slots."

  (or (let ((sd (find-archetypal-property-definition-by-predicate object predicate)))
        (and sd (slot-boundp object (c2mop:slot-definition-name sd))))
      (let ((sd (find-prototypal-property-definition object predicate)))
        (and sd (slot-boundp sd 'value)))))


(defmethod rdf:has-subject? ((object resource-object) (subject resource-object))
  (eq object subject))


(defmethod rdf:has-subject? ((object resource-object) (subject t))
  (rdf:equal (rdf:uri object) subject))


;;;
;;; life-cycle support

(defmethod (setf rdf:find-instance) (instance (object resource-object) identifier)
  (setf (rdf:find-instance (object-repository object) identifier) instance))


(defmethod repository-register ((mediator repository-mediator) (object resource-object))
  (setf (gethash object (mediator-transaction-cache mediator))
        (object-state object)))


(defmethod rdf:query ((resource-object resource-object) &key subject predicate object context continuation offset limit)
  (when (and (or (null subject) (rdf:equal resource-object subject))
             (or (null context) (equal context (object-context resource-object))))
    (unless subject (setf subject (rdf:uri resource-object)))
    
    (dsu:collect-list (collect)
      (flet ((dynamic-collect (statement)
               (when (or (null offset) (minusp (decf offset)))
                 (if (or (null limit) (not (minusp (decf limit))))
                   (collect (copy-triple statement))
                   (return))))
             (constrained-continue (statement)
               (when (or (null offset) (minusp (decf offset)))
                 (if (or (null limit) (not (minusp (decf limit))))
                   (funcall continuation statement)))))
        (declare (dynamic-extent #'dynamic-collect #'constrained-continue))
        (let ((continuation (if continuation
                              (if (or offset limit) #'constrained-continue continuation)
                              #'dynamic-collect)))
          (if predicate
            (if object
              (let ((triple (rdf:triple subject predicate nil)))
                (rdf:map-collection #'(lambda (value)
                                        (when (rdf:equal value object)
                                          (setf (triple-object triple) value)
                                          (funcall continuation triple)))
                                    (bound-property-value resource-object predicate)))
              (let ((triple (rdf:triple subject predicate nil)))
                (rdf:map-collection #'(lambda (value)
                                        (setf (triple-object triple) value)
                                        (funcall continuation triple))
                                    (bound-property-value resource-object predicate))))
            (if object
              (rdf:map-property-slots #'(lambda (sd)
                                          (when (property-boundp resource-object sd)
                                            (project-slot-using-statement resource-object sd (rdf:triple subject nil nil)
                                                                          #'(lambda (stmt)
                                                                              (when (rdf:equal object (triple-object stmt))
                                                                                (funcall continuation stmt))))))
                                      resource-object)
              (rdf:project-graph resource-object #'(lambda (stmt) (funcall continuation (copy-triple stmt)))))))))))


(defmethod rdf:project-graph ((object resource-object) (function function))
  (let ((statement (make-quad :subject (rdf:uri object) :context (object-context object))))
    (flet ((project-slot (sd)
             (project-slot-using-statement object sd statement function)))
      (rdf:map-property-slots #'project-slot object))
    function))


