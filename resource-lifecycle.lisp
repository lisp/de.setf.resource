;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.resource.implementation; -*-


(in-package :de.setf.resource.implementation)

(:documentation
  "This file defines the operators which implement the persistence life-cycle for property objects
 for the `de.setf.resource` CLOS linked data library."
 
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
  "'de.setf.resource' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.resource' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.resource' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/).")

 (description "Implements the JDO object life-cycle for resource objects.[1]
 The general behavior involves three states: transient, persistent variations, in particular hollow.
 In the transient state mo content is tracked and nothing happens to maintain a relation with the source.
 In the persistent states changes history is tracked and object content reflects persistent state plus the
 tracked delta. Hollow, as a special case, contains no state, as the instance should be identical with the
 repository data.

 ---
 [1] : http://db.apache.org/jdo/state_transition.html"))


;;; the interface implements the state-relevant operators and direct state modifiers independent of the current
;;; state and delegates to state specific methods, which are implemented for the legal initial states.

(defmethod rdf:commit ((object resource-object))
  "Invoked at the successful conclusion of a transaction to transfer the object state to its
 respective persistent repository. Calls write-properties-in-state, which acts according to whether
 the instane is new, has been modified, or deleted, etc."
  (write-properties-in-state object (object-state object))
  (commit-in-state object (object-state object)))


(defmethod rdf:delete ((object resource-object))
  "When called inside of a transaction, marks the object to be deleted upon successful conslusion, at which
 point all statements which refer to the object as subject or object are deleted.
 Outside of a transaction the repository deletion happens immediately."
  (delete-in-state object (object-state object)))


(defmethod rdf:evict ((object resource-object))
  "The base method operates on the object combined with its state."
  (evict-in-state object (object-state object)))


(defmethod rdf:make-persistent ((object resource-object))
  "The base method operates on the object combined with its state."
  (make-persistent-in-state object (object-state object)))


(defmethod rdf:make-transient ((object resource-object))
  "The base method operates on the object combined with its state."
  (make-transient-in-state object (object-state object)))


(defmethod rdf:modify ((object resource-object) slot)
  "The base method operates on the object combined with its state."
  (modify-in-state object (object-state object) slot))


(defmethod rdf:read-properties ((object resource-object))
  "The base method operates on the object combined with its state."
  (read-properties-in-state object (object-state object)))


(defmethod rdf:refresh ((object resource-object))
  "The base method operates on the object combined with its state."
  (refresh-in-state object (object-state object)))


(defmethod rdf:rollback ((object resource-object))
  "The base method operates on the object combined with its state."
  (rollback-in-state object (object-state object)))


(defmethod rdf:write-properties ((object resource-object))
  "The base method operates on the object combined with its state."
  (write-properties-in-state object (object-state object)))


;;; state specific versions

(defgeneric commit-in-state (object state)
  (:documentation "The last step of the commit operation removes all cached state and marks it hollow.")

  (:method (object (state rdf:persistent))
    (make-hollow object))

  (:method (object (state rdf:modified-persistent))
    "A modified object clears its version lock to the new version."
    (let ((graph (object-graph object)))
      (when graph
        (repository-commit-object-version (object-repository object) (uri object) graph)))
    (call-next-method))

  (:method (object (state rdf:new-persistent))
    "A new object creates its version lock with the new version."
    (let ((graph (object-graph object)))
      (when graph
         (repository-commit-object-version (object-repository object) (uri object) graph)))))
  

(defgeneric delete-in-state (object state)
  (:documentation "Given a permitted initial state, mark the instance as deleted and 'write' its properties,
    to delete them from the repository.")

  (:method (object (state rdf:hollow))
    (setf-object-state rdf:deleted-persistent object)
    (write-properties-in-state object rdf:deleted-persistent))

  (:method (object (state rdf:modified-persistent))
    (setf-object-state rdf:deleted-persistent object)
    (write-properties-in-state object rdf:deleted-persistent))

  (:method (object (state rdf:new-persistent))
    "If the instance is still 'new' there are no persisten properties to delete. Just change the state."
    (setf-object-state rdf:deleted-persistent object)))


(defgeneric evict-in-state (object state)
  (:documentation "Remove the object form those governed by a transaction. This constrains the object
 to be in an unmodified state, and otherwise sognals en invalid-state-error.")

  (:method (object state)
    (invalid-state-error :object object :start-state state :end-state rdf:transient))

  (:method ((object t) (state rdf:hollow))
    "If alread hollow, do nothing."
    state)

  (:method (object (state rdf:clean-persistent))
    "If the object is unmodified, remove it from the collection of tracked objects, by default unbind
 its properties, reset its state."
    (transaction-evict (object-repository object) object)
    (unless (rdf:retain-values? (object-repository object))
      (rdf:unbind-property-slots object))
    (setf-object-state rdf:hollow object)))


(defgeneric lock-version-in-state (object state)
  (:documentation "The first step of the commit operation is to grab the object's version lock.
 If the object is to be deleted, or is unmodified, do nothing. The concrete step to 'revise' the object's
 version in the repository is to write a new value to the object's lock cell at the commit conclusion.
 Return true if the lock succeeded.")

  (:method ((object t) (state rdf:deleted-persistent))
    ;; nothing to do
    t)

  (:method (object (state rdf:modified-persistent))
    "A modified object clears its version lock to the new version."
    (let ((graph (object-graph object)))
      (when graph
        (repository-lock-object-version (object-repository object) (uri object) graph))))

  (:method ((object t) (state rdf:new-persistent))
    "A new object does not yet have a version lock."
    t))


(defgeneric make-persistent-in-state (object state)
  (:documentation   "Mark the object to be tracked and saved in the event of modifications.")

  (:method (object state)
    (invalid-state-error :object object :start-state state :end-state rdf:transient))

  (:method ((object t) (state rdf:persistent))
    "If already persistent, do nothing."
    state)

  (:method (object (state rdf:transient))
    "Change the state of transient objects to persistent."
    (setf-object-state rdf:new-persistent object)))


(defgeneric make-transient-in-state (object state)
  (:documentation "Change the state to transient. Permitted for hollow and clean objects only.
 If the object is in a state which reflects modifications, signal an invalid-state error.")

  (:method (object state)
    (invalid-state-error :object object :start-state state :end-state rdf:transient))

  (:method (object (state rdf:hollow))
    "From hollow just sets the state. No properties are present."
    (setf-object-state rdf:transient object))

  (:method (object (state rdf:clean-persistent))
    "From clean, set the state and retain any content."
    (setf-object-state rdf:transient object)))


(defgeneric modify-in-state (object state name)
  (:documentation  "For the first change of a slot, record the original value of the slot in the object
 history. Mark the state as modified.")

  (:method (object (state rdf:persistent) name)
    (let ((value (if (slot-boundp object name)
                   (slot-value object 'name)
                   +unbound-marker+)))
      (unless (assoc name (object-history object))
        (setf-object-history (acons name value (object-history object)) object))
      (setf-object-state rdf:modified-persistent object)))

  (:method ((object t) (state rdf:transient) (name t))
    "Transient objects record no state"
    rdf:transient))


(defgeneric read-properties-in-state (object state)
  (:documentation "Apply the object itself as the selection constraints against its source and project the
 result statements onto its properties. Permitted from hollow status only.")

  (:method (object state)
    (invalid-state-error :object object :start-state state :end-state rdf:transient))

  (:method (object (state rdf:hollow))
    (rdf:project-graph (object-repository object) object)
    (setf-object-state rdf:clean-persistent object)))


(defgeneric refresh-in-state (object state)
  (:documentation "First, discard any content, and any history. Then read the properties and transition to
 clean persistent. Permitted from persistent modified status only.")

  (:method (object state)
    (invalid-state-error :object object :start-state state :end-state rdf:transient))

  (:method (object (state rdf:modified-persistent))
    (unbind-property-slots object)
    (setf-object-history nil object)
    (rdf:project-graph (object-repository object) object)
    (setf-object-state rdf:clean-persistent object)))


(defgeneric rollback-in-state (object state)
  (:documentation "Withing the context of a transactions, restore the state of a modified object to that from
 the point of first modification withn the transaction. There is no actual constraint on state, but if
 not in a transaction, no changes will have been cached. Update the result state as appropriate for the
 given state.")

  (:method (object (state t))
    "Iterate over the object's history and restore properties to their initial values. Those which were
 unbound are restored to that state."
    (loop for (name . value) in (object-history object)
          if (eq value +unbound-marker+)
          do (slot-makunbound object name)
          else do (setf (slot-value object name) value))
    (setf-object-history nil object))

  (:method (object (state rdf:modified-persistent))
    (call-next-method)
    (setf-object-state rdf:hollow object))

  (:method (object (state rdf:new-persistent))
   (call-next-method)
   (setf-object-state rdf:transient object)))


;;; utilities

(defgeneric make-hollow (resource-object)
  (:method ((object resource-object))
    "Leave the instance in a state where it cannot become inconsistent with the persistent source."
    (unbind-property-slots object)
    (setf-object-state rdf:hollow object)))



(defgeneric write-properties-in-state (object state)
  (:documentation "Write modified properties to the repository. This pertains to persistent object which
 have been modified.")

  (:method (object (state rdf:new-persistent))
    "For a new object, emit all statements to the source."
    (rdf:project-graph object (object-repository object)))

  (:method (object (state rdf:modified-persistent))
    "For an existing persistent object, for each modified property slot, delete the original statements, emit
 statements to reflect the new state, and record the concrete statements which arereflected back."
    (let ((statement (make-quad :subject object :context (object-graph object))))
      (flet ((project-modified (destination)
               (labels ((project-slot-value (value)
                          (typecase value
                            (list (dolist (value value) (project-slot-value value)))
                            (t
                             (setf (quad-object statement) value)
                             (rdf:project-graph statement destination))))
                        (delete-each (statement)
                          (typecase statement
                            ((eql +unbound-marker+) )
                            (list (dolist (statement statement) (delete-each statement)))
                            (t (rdf:delete-statement destination statement)))))
                 (loop for (slot . nil) in (object-history object)
                       do (let ((statement-sd (slot-definition-statement-slot slot)))
                            ;; for each slot which was associated with a statement, delete the respective statement
                            (when (slot-boundp object (c2mop:slot-definition-name statement-sd))
                              (delete-each (funcall (slot-definition-reader statement-sd) object)))
                            ;; write literal slots as the direct statement object and
                            ;; write property slots as the reference.
                            (setf (quad-predicate statement) (slot-definition-predicate slot))
                            (funcall (funcall (slot-definition-writer statement-sd) object)
                                     (project-slot-value (slot-value (c2mop:slot-definition-name slot) object))
                                     object))))))
        (rdf:project-graph #'project-modified (object-repository object)))))

  (:method (object (state rdf:deleted-persistent))
    "Given a persistent object which is to be deleted, delete all statements which refer to it in any
 role. For this, retrieve the subject-role anew as well, in order to avoid version skew."
    (flet ((delete-each (statement)
             (rdf:delete-statement (object-repository object) statement)))
      (rdf:query (object-repository object) :subject object :continuation #'delete-each)
      (rdf:query (object-repository object) :predicate object :continuation #'delete-each)
      (rdf:query (object-repository object) :object object :continuation #'delete-each)
      (setf-object-state rdf:transient object))))


(:documentation rdf:project-graph
  "Projection referes to the process of transfering a graph from one context to another.
 This can use a collection of statements from an external repository to set the state of an instance,
 synchronize the repository with an object's modifications, or interate over an object's properties
 to write them to a stream. The source and target can be any of several forms:
 - a specific instance, in which case only matching subjects apply
 - a class, in which the matching instance is sought
 - a metaclass, in which case the matching instance of the matching class is sought.")


(defmethod rdf:project-graph ((source t) (object resource-object))
  "Project the source enumeration onto the object.
 SOURCE : rdf:enumeration : a selection enumeration to apply to the object
 OBJECT : resource-object
 VALUE : resource-object : the result object
 CONDITIONS : property-missing-error : if a statement specifies a property which the object class does not own,
  a continuable error is signaled.

 Given a resource-object target, retrieve its properties as a statement sequence and apply each in turn.
 Prepare for the eventuality, that the CLOS model is incomplete by collecting relations which specify absent
 properties ans signal a conclusive error, with continuations to abort the process or reclassify the
 instance.  If continued, allow to interrogate the repository for further type information or reclassify
 based on a structural analysis.
 NB. this process may also occur on a higerlevel, when mapping entire graphs, to augment the CLOS model
 proactively."
 
  (flet ((apply-statement (statement)
           (rdf:project-graph statement object)))
    (declare (dynamic-extent #'apply-statement))
    (rdf:project-graph source #'apply-statement)
    object))


(defmethod rdf:project-graph ((statement rdf:triple) (object resource-object))
  (when (rdf:equal (triple-subject statement) object)
    (rdf:insert-statement object statement))
  object)


(defmethod rdf:project-graph ((source list) (class resource-class))
  "Combine a list and a class by locating the designated the instance of the class and adding the
 Statement to it."
  (let ((object nil)
        (object-uri nil))
    (dolist (statement source)
      (let* ((subject-uri (rdf:subject statement)))
        (unless (and object-uri (rdf:equal object-uri subject-uri))       ; reuse the latest object
          (setf object (rdf:ensure-instance class subject-uri)
                object-uri (rdf:uri object)))
        (rdf:insert-statement object statement)))
    class))


(defmethod rdf:project-graph ((source repository-mediator) (class resource-class))
  "Given a source, enumerate the statements which match the resource-class, project the source statements
 onto instances of that class.
 Choose the concrete instance by matching the statement subject uri to the instance URI."

  (let ((subjects ())
        (datatypes ()))
    (labels ((project-class (class)
               (flet ((project-subject-graph (statement)
                        (let* ((subject-uri (rdf:subject statement))
                               (subject (rdf:ensure-instance class subject-uri)))
                          (pushnew subject subjects)
                          (rdf:project-graph source subject))))
                 (declare (dynamic-extent #'project-subject-graph))
                 (let ((datatype (class-datatype class)))
                   (unless (find datatype datatypes)
                     (push datatype datatypes)
                     (rdf:query source :predicate '{rdf}type :object datatype
                                :continuation #'project-subject-graph)))
                 (map nil #'project-class (c2mop:class-direct-subclasses class)))))
      (project-class class))
    (values subjects datatypes)))


(defmethod rdf:project-graph ((source repository-mediator) (object resource-object))
  (dolist (statement (rdf:query source :subject (rdf:uri object)))
    (rdf:project-graph statement object))
  object)


(defmethod rdf:project-graph ((source repository-mediator) (destination (eql t)))
  "When the destination class is unspecific, project all type resources."
  (let ((subjects ())
        (datatypes '({owl}Class {rdfs}Class)))          ; don't touch the metaclasses
    (labels ((project-datatype (type-statement)
               (let ((datatype (rdf:object-value source type-statement)))
                 (unless (find datatype datatypes)
                   (push datatype datatypes)
                   (case datatype
                     ({rdf}XMLLiteral   ; don't know why they are allowed, but they appear
                                        ; collect the literals
                      (flet ((collect-literals (statement)
                               (push (rdf:subject-value source statement) subjects)))
                        (declare (dynamic-extent #'collect-literals))
                        (rdf:query source :predicate '{rdf}type :object datatype
                                   :continuation #'collect-literals)))
                     (t
                      (let ((class (rdf:find-class source datatype)))
                        (flet ((project-subject-graph (statement)
                                 (let* ((subject-uri (rdf:subject statement))
                                        (subject (rdf:ensure-instance class subject-uri)))
                                   (pushnew subject subjects)
                                   (rdf:project-graph source subject))))
                          (declare (dynamic-extent #'project-subject-graph))
                          (rdf:query source :predicate '{rdf}type :object datatype
                                     :continuation #'project-subject-graph)))))))))
      (declare (dynamic-extent #'project-datatype))
      (rdf:query source :predicate '{rdf}type :continuation #'project-datatype))
    (values subjects datatypes)))



(defmethod rdf:project-graph ((object resource-object) (mediator repository-mediator))
  (let ((statement (make-quad :subject object :context (object-graph object))))
    (labels ((project-slot (sd)
               (project-slot-using-statement object sd statement #'project-statement))
             (project-statement (stmt)
               (rdf:insert-statement mediator stmt)))
      (declare (dynamic-extent #'project-slot #'project-statement))
      (rdf:map-property-slots #'project-slot object))
    mediator))



;;; it is not practical to attempt this unless the statements are coherent. a repository, for example, has
;;; so many spurious assertions that a coherent model would spend an inordinate portion of the time rejecting
;;; meta and other spurious statements.
#+(or)
(defmethod rdf:project-graph ((source t) (metaclass resource-metaclass))
  "Given a source enumeration and a resource-metaclass, project the source statements onto instances of classes of that class.
 Choose the concrete class by matching the statement subject uri to the concrete class' base URI."

  (let ((class nil)
        (objects nil)
        (object nil)
        (type nil))
    (flet ((apply-statement (statement)
             (when statement
               (let* ((uri (rdf:subject statement)))
                 (cond ((and object (rdf:equal (rdf:uri object) uri)))          ; reuse the latest object
                       ((setf object (find uri objects :test #'rdf:equal :key #'rdf:uri)))    ; or one of those already targeted
                       ((setf object (rdf:find-instance class uri)))
                       ((setf type (rdf:type-of (class-repository metaclass) uri))
                        ; if the type is known, construct a new instance of the respective class
                        (unless (and class (rdf:equal (class-name class) type))
                          (setf class (rdf:find-class metaclass type))
                          (first (push (or (rdf:find-instance class uri)
                                           (make-instance class :uri uri))
                                       objects)))))
                 (rdf:project-graph statement object)))))
      (declare (dynamic-extent #'apply-statement))
      (rdf:project-graph source #'apply-statement))
    objects))

(defgeneric project-slot-using-statement (object slot statement function)
  (:documentation "use the slot type to distinguish the combinations
    - property v/s slot
    - literal v/s resource
 extract the property object value(s) and apply the function to the statement for each one.")
  
  (:method ((object resource-object) (sd rdf:archetypal-property-definition) (statement rdf:triple) function)
    (let* ((predicate (slot-definition-predicate sd))
           (name (c2mop:slot-definition-name sd))
           (reader (slot-definition-reader sd)))
      (when (slot-boundp object name)
        (setf (triple-predicate statement) predicate)
        (flet ((do-value (value)
                 (setf (triple-object statement) value)
                 (funcall function statement)))
          (declare (dynamic-extent #'do-value))
          (rdf:map-collection #'do-value (funcall reader object)))))
    statement)
  
  (:method ((object resource-object) (sd rdf:prototypal-property-definition) (statement rdf:triple) function)
    (when (slot-boundp sd 'value)
      (let ((predicate (slot-definition-predicate sd)))
        (setf (triple-predicate statement) predicate)
        (flet ((do-value (value)
                 (setf (triple-object statement) value)
                 (funcall function statement)))
          (declare (dynamic-extent #'do-value))
          (rdf:map-collection #'do-value (slot-definition-value sd)))))
    statement))


(defmethod rdf:insert-statement ((object resource-object) statement)
  "GIven a triple, locate the respective value and statement slots, and set or augment the values.
 If neither a property nor a slot is defined, call property-missing."
  
  (let* ((predicate (rdf:model-value object (rdf:predicate statement)))
         (definition (or (find-archetypal-property-definition-by-predicate object predicate)
                         (find-prototypal-property-definition object predicate))))
    (etypecase definition
      (rdf:archetypal-property-definition
       (rdf:insert-statement-using-slot definition object statement))
      (rdf:prototypal-property-definition
       (rdf:insert-statement-using-slot definition object statement))
      (null
       (restart-case (funcall (class-property-missing-function (class-of object))
                              (class-of object) object predicate 'rdf:insert-statement statement)
         (ignore ()
                 :report (lambda (stream)
                           (format stream "Ignore the operation, return no values."))
                 (values))
         (make-definition ()
                          :report (lambda (stream)
                                    (format stream "Create a property definition and continue."))
                          (setf (find-prototypal-property-definition object predicate)
                                (setf definition
                                      (rdf:prototypal-property-definition object :name predicate)))
                          (rdf:insert-statement-using-slot definition object statement))
         (use-definition (definition)
                         :report (lambda (stream)
                                   (format stream "Supply a property definition and continue."))
                         (assert (typep definition 'rdf:prototypal-property-definition) ()
                                 "Invalid property definition: ~s." definition)
                         (rdf:insert-statement-using-slot definition object statement)) 
         (use-statement (statement)
                        :report (lambda (stream)
                                  (format stream "Retry the operation with the given statement."))
                        (rdf:insert-statement object statement)))))))


(defgeneric rdf:insert-statement-using-slot (slot object statement)
  (:documentation "use the slot type to distinguish the combinations
    - property v/s slot
    - literal v/s resource
 If the type is a cons type, set initially to a list, then push subsequent values. Otherwise set and re-set.
 For a property, the value and statement are stored in the property definition.
 For a slot, the slot definition provides the name for slot-based storage.
 The operation leaves the statement with the respective literal atomic/group statement shadow to the
 statement or augmented with same, the slot/property bound to/augmented with the value if literal and
 unbound if a resource.")
  
  (:method ((sd rdf:archetypal-property-definition) (object resource-object) statement)
    (let* ((name (c2mop:slot-definition-name sd))
           (ssd (slot-definition-statement-slot sd))
           (ss-name (c2mop:slot-definition-name ssd))
           (repository-value (rdf:object statement))
            (value (if (rdf:identifier-p repository-value)
                     (rdf:ensure-instance object repository-value)    ; designators in rdf domain
                     (rdf:model-value object repository-value))))
      (if (slot-definition-list-type-p sd)
        (if (slot-boundp object name)
          (setf (slot-value object ss-name) (cons statement (slot-value object ss-name))
                (slot-value object name) (cons value (slot-value object name)))
          (setf (slot-value object ss-name) (list statement)
                (slot-value object name) (list value)))
        (setf (slot-value object ss-name) statement
              (slot-value object name) value)))
    statement)
  
  (:method ((sd rdf:prototypal-property-definition) (object resource-object) statement)
    "Given a prototypal definition, compute the model value from the statement object introspectively,
     and bind the value as a list or an atom depending on the (optionally) declared property type,
     whether the property is bound, and whether an existing value is a list."
    (let* ((repository-value (rdf:object statement))
           (value (if (rdf:identifier-p repository-value)
                    (rdf:ensure-instance object repository-value)    ; designators in rdf domain
                    (rdf:model-value object repository-value))))
      (multiple-value-bind (list-p certain)
                           (list-type-p (c2mop:slot-definition-type sd))
        (if certain
          (if list-p
            (setf (slot-definition-statement sd) (list statement)
                  (slot-definition-value sd) (list value))
            (setf (slot-definition-statement sd) statement
                  (slot-definition-value sd) value))
          (if (slot-boundp sd 'value)
            ;; for a bound, untyped property
            (let ((old-value (slot-definition-value sd)))
              ;; modify an existing value by augmenting a list, replacing an identical value,
              ;; and changing a previous atomic value into a list
              (cond ((listp old-value)          ; distinguish nil from unbound
                     (unless (find value old-value :test #'rdf:equal)
                       (push statement (slot-definition-statement sd))
                       (setf (slot-definition-value sd) (cons value old-value))))
                    ((rdf:equal value old-value))
                    (t
                     (setf (slot-definition-statement sd)
                           (list statement (slot-definition-statement sd)))
                     (setf (slot-definition-value sd)
                           (list value old-value)))))
            ;; the initial value of an untyped slot is bound as an atom.
            (setf (slot-definition-statement sd) statement
                  (slot-definition-value sd) value)))))
    statement))

#+(or)
(defmethod rdf:delete-object ((object resource-object) (object t))
  ;; this would need to compare to an atomic value and unbind the property, but
  ;; compare and remove the values from a sequence
  object)


(defmethod rdf:delete-predicate ((object resource-object) (predicate t))
  (or (let ((sd (find-archetypal-property-definition-by-predicate object predicate)))
        (and sd (unbind-property-using-definition object sd) predicate))
      (let ((sd (find-prototypal-property-definition object predicate)))
        (and sd (unbind-property-using-definition object sd) predicate))))


(defmethod rdf:repository-indelible? ((object resource-object))
  (rdf:repository-indelible? (class-of object)))


(defmethod rdf:delete-statement ((object resource-object) statement)
  "GIven a triple, locate the respective value and statement slots, and unbind them.
 If neither a property nor a slot is defined, call property-missing."

  (let* ((name (rdf:model-value object (rdf:predicate statement)))
         (definition (or (find-archetypal-property-definition-by-predicate object name)
                         (find-prototypal-property-definition object name))))
    (etypecase definition
      (rdf-relation-definition
       (unbind-property-using-definition object definition))
      (null
       (restart-case (funcall (class-property-missing-function (class-of object))
                              (class-of object) object name 'rdf:delete-statement statement)
         (ignore ()
                 :report (lambda (stream)
                           (format stream "Ignore the operation, return no values."))
                 (values)))))))


(defgeneric unbind-property-using-definition (object slot)
  (:documentation "use the slot definition to unbind the roperty.")
  
  (:method ((object resource-object) (sd rdf:archetypal-property-definition))
    (let* ((name (c2mop:slot-definition-name sd))
           (ssd (slot-definition-statement-slot sd))
           (ss-name (c2mop:slot-definition-name ssd)))
      (slot-makunbound object name)
      (slot-makunbound object ss-name))
    object)
  
  (:method ((object resource-object) (sd rdf:prototypal-property-definition))
    (let ((properties (get-object-properties object)))
      (when properties (remhash (slot-definition-predicate sd) properties)))
    (slot-makunbound sd 'value)
    (slot-makunbound sd 'statement)
    object))


(defmethod rdf:delete-subject ((object resource-object) (subject t))
  (when (rdf:equal subject object)
    (rdf:delete-subject (object-repository object) subject)))



(defmethod rdf:property-missing ((class resource-class) (object resource-object)
                             (property t) (operation t) &optional value)
  (rdf:property-missing-error :object object :predicate property :operation operation :value value))


(defmethod rdf:property-missing ((class resource-class) (object resource-object)
                                 (property t) (operation (eql 'rdf:setf-property-value)) &optional value)
  "The base method for setting a missing property is to create a prototypal property."
  (declare (ignore value))
  (invoke-restart 'make-definition))


(defmethod rdf:property-missing ((class resource-class) (object resource-object)
                                 (property t) (operation (eql 'rdf:insert-statement)) &optional value)
  (declare (ignore value))
  (invoke-restart 'make-definition))


(defmethod rdf:property-missing ((class resource-class) (object resource-object)
                                 (property t) (operation (eql 'rdf:prototypal-property-value)) &optional value)
  "The base method for a prototypal property read return nil for a missing property."
  (declare (ignore value))
  (invoke-restart 'use-value nil))


(defmethod property-read-only ((class resource-class) (object resource-object)
                               (slot rdf:prototypal-property-definition) operation new-value)
  (property-read-only class object (c2mop:slot-definition-name slot) operation new-value))


(defmethod property-read-only ((class resource-class) (object resource-object)
                               predicate operation new-value)
  "The base operator for resource-class and  resource-object arguments signals a continuable
 property-read-only-error. It establishes the contuations:
 - ignore : skip the operation and return no values
 - use value : return a given value
 - use property : perform the operation on a different property."

  (restart-case (property-read-only-error :object object :predicate predicate
                                          :operation operation :value new-value)
    (ignore ()
            :report (lambda (stream)
                      (format stream "Ignore the operation, return no values."))
            (values))
    (use-value (value)
               :report (lambda (stream)
                         (format stream "Specify a value to return from the operation"))
               value)
    (use-property (property)
                   :report (lambda (stream)
                             (format stream "Retry the operation with the given property."))
                   (funcall operation new-value object property))))