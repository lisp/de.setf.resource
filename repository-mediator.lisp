;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.resource.implementation; -*-


(in-package :de.setf.resource.implementation)

(:documentation
  "This file defines the linked data repository interface for the `de.setf.resource` CLOS linked data library."
 
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
  "'de.setf.resource' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.resource' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.resource' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/).")

 (description
  "The abstract repository-mediator class embodies the interface to linked data models and repositories.
 It comprises metadata required to manage access to a concrete in-memory model or an external persistent
 repository, maintains object identity, transforms representations, and bridges from an uniform operation
 interface to the each concrete implementation. The interface comprises an operators set adapted from several
 established RDF libraries, [RDF2Go](http://semanticweb.org/wiki/RDF2Go), [RDF.rb]
 including default method implementations for any abstract operators.

 The operators fall into three classes:

 - concrete api operators, for which 'api.lisp' defines the interface and this files defines some
   default implementations. Each concrete class may implement its own method.
   For the inquiry functions, the abstract class defines class-allocated slots.

   - load-repository
   - repository-clear
   - repository-count
   - repository-empty?
   - repository-indelible?
   - repository-persistent?
   - repository-readable?
   - repository-transient?
   - repository-writable?
   - save-repository

 - concrete operators, for which this file defines a default implementation

   - find-class
   - ensure-vocabulary
   - find-vocabulary
   - load-vocabulary
   - model-value
   - repository-class-definition
   - repository-value
   - repository-property-definition

 - abstract operators, for which this file defines a default implementation in terms of
   primitive functions. A concrete mediator class either defines those primitives or implements
   the entire operator

   - has-context?
   - has-predicate?
   - has-object?
   - has-statement?
   - has-subject?
   - insert-statement
   - map-contexts
   - map-objects
   - map-predicates
   - map-statements
   - map-subjects
   - project-graph
   - query
   - delete-statement

 - concrete primitive operators, for which this file defines the interface and each concrete class
   implements its own method.

   - add-statement*
   - delete-statement*
   - map-statement*
 "))


(defclass repository-mediator ()
  ((repository
    :initform (error "repository required") :initarg :repository
    :reader mediator-repository
    :documentation "The repository store instance. Each concrete class entails its own specification form
     and must provide a default value if no initialization argument is given.")
   (id
    :initform (uuid:make-v1-uuid) :initarg :id
    :reader mediator-id
    :documentation "Used to identify the mediator in transactions.")
   (transaction-cache
    :initform (make-hash-table)
    :reader mediator-transaction-cache
    :documentation "Caches the instances created and/or modified during a transaction.")
   (instance-cache
    :initform (make-hash-table :test 'equal)
    :reader mediator-instance-cache
    :documentation "URI-keyed cache for the instances present in the repository and read into the
     application. Registers both transactiona and non-transactional instances to support interned
     instantiation. It needs to be weak, but that is not possible portably. An alternative would be weak
     entries, in which case it could as well be a weak avl/b+tree.")
   (state
    :initform rdf:non-transactional
    :reader mediator-state :writer setf-mediator-state
    :documentation "Indicates the mediated transaction state.")
   (vocabularies
    :initform nil :type list
    :reader mediator-vocabularies :writer setf-mediator-vocabularies
    :documentation "A list of URI namestrings which designate the vocabularies known to the source.
     The value is initialized from (subject . (query nil {rdf}:type {owl}:Ontology)) when the source is
     connected and is updated whenever a vocabulary is loaded.")
   (identifier-function
    :initform 'identifier-identity :initarg :identifier-function
    :reader mediator-identifier-function
    :documentation "The canonicalization function for term names mapped between the storage and the model
     representations according to the convention that model terms are symbols while storage terms
     are strings.")
   (repository2model-value-map
    :initform (make-hash-table :test 'equal)
    :reader mediator-repository2model-value-map
    :documentation "Maps resource URI to program symbols. The URI representation depends on the repository.")
   (model2repository-value-map
    :initform (make-hash-table :test 'equal)
    :reader mediator-model2repository-value-map
    :documentation "Maps model identifiers to repository value. The representation depends on the repository.")
   (default-context
     :initform (concatenate 'string "urn:sha1:" (compute-spoc-sha1-hex-id ))
     :reader mediator-default-context
     :documentation "The URN used to identify the graph to used as the repository's default context.")
   (indelible
    :initarg :indelible :initform nil :allocation :class
    :reader rdf:repository-indelible? :writer setf-repository-indelible
    :documentation "If true, any delete-statement call signals an error. Initially nil.")
   (persistent
    :initarg :persistent :initform nil :allocation :class
    :reader rdf:repository-persistent? :writer setf-repository-persistent
    :documentation "Indicate if the mediated repository is persistent. Initially nil. (see transient)")
   (readable
    :initarg :readable :initform t :allocation :class
    :reader rdf:repository-readable? :writer setf-repository-readable
    :documentation "Indicate if the mediated repository is readable. Initially t")
   (writable
    :initarg :writable :initform t :allocation :class
    :reader rdf:repository-writable? :writer setf-repository-writable
    :documentation "Indicate if the mediated repository supports insert and (if it is not indelible)
     delete operations. Initially t.")
   (maps-dynamic-extent
    :initform t :allocation :class
    :reader mediator-maps-dynamic-extent?
    :documentation "Indicates the mapping operators pass statements with dynamic extent."))

  (:default-initargs
    :vocabularies (list *rdf-vocabulary* *rdfs-vocabulary* *owl-vocabulary*
                        *xsd-vocabulary*))
  (:documentation "A repository-mediator encapsulates a concrete triple store to provide a standard repository
 interface to that repository's operators and state and mediates between resource instances and the repository.
 Concrete specializations are defined as
 - wilbur-mediator : for wilbur[1]
 - cassandra-mediator_<version> : for cassandra[3] versions
 - agraph-mediator : for allegrograph[2]

 ---
 [1]: http://wilbur-rdf.sourceforge.net/
 [2]: http://www.franz.com/allegrograph
 [3]: cassandra.apache.org/
 "))
  
(define-condition duplicate-statement (simple-error)
  ((repository :initarg :repository :reader error-repository)
   (statement :initarg :statement :reader error-statement))
  (:report (lambda (c stream)
             (format stream "Statement exists in repository: ~a, ~s."
                     (error-repository c) (error-statement c)))))


(defmethod print-object ((mediator repository-mediator) (stream t))
  (print-unreadable-object (mediator stream :identity t :type t)
    (format stream "mediating: ~a x ~a"
             (mediator-repository mediator)
             (mediator-state mediator))))


(def-class-constructor repository-mediator (object &rest args)
  (:method ((null null) &key)
    nil)
  (:method ((default (eql t)) &key)
    (repository-mediator *repository-mediator.default*)))


(defmethod initialize-instance :after ((instance repository-mediator)
                                       &key vocabularies)

  "Complete initialization by incorporating vocabularies"
  (dolist (vocabulary vocabularies)
    (ensure-vocabulary instance vocabulary))
  #+ccl
  (ccl:terminate-when-unreachable instance))

#+ccl
(defmethod ccl:terminate ((object repository-mediator))
  (repository-close object))


(defgeneric mediator-clear-instance-cache (mediator)
  (:method ((mediator repository-mediator))
    (clrhash (mediator-instance-cache mediator))))


;;;
;;; concrete api operators


(defmethod rdf:load-repository ((mediator repository-mediator) (location pathname))
  (rdf:project-graph location mediator)
  (values (rdf:repository-count mediator)
          location))


(defmethod rdf:repository-close ((mediator repository-mediator))
  ;; the base method does nothing
  nil)


(defmethod rdf:repository-transient? ((mediator repository-mediator))
  (not (rdf:repository-persistent? mediator)))


(defmethod rdf:save-repository ((mediator repository-mediator) location)
  (rdf:save-repository (mediator-repository mediator) location))


;;;
;;; default implementations for concrete operators

(defmethod rdf:find-class ((mediator repository-mediator) (name symbol) &key (error-p t))
  (or (rdf:find-class (class-of mediator) name :error-p nil)
      (let ((vocabulary (ensure-vocabulary mediator name)))
        (when vocabulary
          (or (let ((definition (rdf:find-class vocabulary name :error-p nil)))
                (when definition
                  (prog1 (eval definition)
                    (dolist (superclass (third definition))
                      (rdf:find-class mediator superclass)))))
              (let ((definition (rdf:repository-class-definition mediator (repository-uri mediator name)))
                    (succeeded nil))
                (when definition
                  ;; handle circular references, but don't leave erroneous definitions registered
                  (setf (rdf:find-class vocabulary name) definition)
                  (unwind-protect (prog1 (eval definition)
                                    (dolist (superclass (third definition))
                                      (rdf:find-class mediator superclass))
                                    (setf succeeded t))
                    (unless succeeded
                      (setf (rdf:find-class vocabulary name) nil)
                      (setf (find-class name) nil))))))))
      (when error-p
        (rdf:class-not-found (find-class 'resource-class) name))))


(defmethod rdf:find-class ((mediator repository-mediator) (identifier t) &rest args)
  (declare (dynamic-extent args))
  (apply #'rdf:find-class mediator (rdf:model-value mediator identifier) args))


(defmethod rdf:find-instance ((mediator repository-mediator) (subject t))
  "Return the instance which is registered with the MEDIATOR for the SUBJECT value's interned equivalent."
  (gethash (rdf:repository-value mediator subject) (mediator-instance-cache mediator)))



(:documentation  rdf:ensure-vocabulary rdf:find-vocabulary rdf:load-vocabulary
  "Support RDF schema by translating them into CLOS. Integrate them into the respective repository
 mediator to govern term mapping and to provide class definitions. Mediate the definition process through
 the repository's repository in order to mitigate variations, inconsistencies, and general insufficiency
 in RDF schema documents. This leaves just the variations between RDFS and OWL schema models.

 This approach delegates all responsibility for schema discovery to the storage infrastructure, which is
 expedient, but admittedly does little to advance issues raised by valkenburg[1].

 This mechanism serves two purposes
 - During development, one can extract the definitions, augment the terms and/or types, and save them as
   Lisp source code for static vocabulary declarations - packages, types, and classes respective one or
   more RDF vocabularies.
 - At run-time, it generates ephemeral vocabulary definitions on demand, as required to reconcile data to
   existing data and procesing models. 
 
  The primary interface operations are
 - rdf:type-of : (identifier) given a resource identifier, return the type as cached or as asserted in the
   repository.
 - rdf:find-class : (mediator identifier) given a class URI, locate or import the class definition.
 - rdf:load-vocabulary (mediator &key uri) : retrieve the vocabulary specification, parse it to
   extract the namespace and the schema and save them in the namespace registry. succeeds only
   with self-contained schema - those which are just properties of other schema yield just the
   terms, but no classes. (eg. http://purl.org/net/vocab/2003/11/photo.rdf)

 ---
 [1] : http://www.ilrt.bris.ac.uk/discovery/rdf-dev/purls/papers/QL98-distributed/
 ")


(defmethod rdf:ensure-vocabulary ((mediator repository-mediator) (uri string) &rest args)
  (or (find-vocabulary mediator uri)
      ;; otherwise continue to load it and incorporate its terms
      (ensure-vocabulary mediator (apply #'load-vocabulary mediator uri args))))


(defmethod rdf:ensure-vocabulary ((mediator repository-mediator) (vocabulary vocabulary) &key)
  (unless (find vocabulary (mediator-vocabularies mediator))
    (rdf:load-vocabulary mediator vocabulary))
  vocabulary)


(defmethod rdf:ensure-vocabulary ((mediator repository-mediator) (uri symbol) &rest args)
  (apply #'rdf:ensure-vocabulary mediator (package-name (symbol-package uri)) args))


(defmethod rdf:find-vocabulary ((mediator repository-mediator) (uri string))
  (dolist (vocabulary (mediator-vocabularies mediator))
    (when (uri-match-p uri (vocabulary-uri vocabulary))
      ;; if the namespace is already present - on the basis of bindings, then return.
      (return vocabulary))))


(defmethod (setf rdf:find-vocabulary) ((value null) (mediator repository-mediator) (uri string))
  (flet ((vocabulary-match-p (vocabulary)
           (let ((v-uri (vocabulary-uri vocabulary)))
             (or (uri-match-p uri v-uri) (uri-match-p v-uri uri)))))
    (declare (dynamic-extent #'vocabulary-match-p))
    (setf-mediator-vocabularies (remove-if #'vocabulary-match-p (mediator-vocabularies mediator))
                                  mediator))
  nil)

(defmethod (setf rdf:find-vocabulary) ((vocabulary vocabulary) (mediator repository-mediator) (uri string))
  (flet ((vocabulary-match-p (vocabulary)
           (let ((v-uri (vocabulary-uri vocabulary)))
             (or (uri-match-p uri v-uri) (uri-match-p v-uri uri)))))
    (declare (dynamic-extent #'vocabulary-match-p))
    (setf-mediator-vocabularies (cons vocabulary
                                        (remove-if #'vocabulary-match-p (mediator-vocabularies mediator)))
                                  mediator))
  vocabulary)


(defmethod rdf:load-vocabulary ((mediator repository-mediator) (vocabulary vocabulary)
                                &key (resource-uri (vocabulary-uri vocabulary)))
  "Incorporate a vocabulary definition into a repository.
 MEDIATOR : repository-mediator
 VOCABULARY : vocabulary

 Add the vocabulary to the repository and include its terms in the repository's identifier map.
 Replaces an existing instance, but an existing instance, but does not attempt to expunge its terms."

  ;; update the local registry
  (setf (rdf:find-vocabulary mediator resource-uri) vocabulary)
    
  ;; augment the identifier cache
  (loop for (symbol . uri-namestring) in (vocabulary-identifier-map vocabulary)
        do (register-value mediator symbol (repository-uri mediator uri-namestring)))
  
  ;; return the vocabulary
  vocabulary)


(defmethod rdf:load-vocabulary ((mediator repository-mediator) (uri string) &key (resource-uri nil ru-s))
  "Load the schema into the repository repository based a vocabulary uri. Note the actual location and warn
 if it diverges from one explicitly provided.
 Extract the class definitions starting with immediate type assertions. Recurse through the precedence lists
 to allow for incomplete specifications. Each class' property definitions are constructed from immediate
 predicate assertions. 
 Add the specifications as declaration forms to the vocabulary.
 Return the vocabulary instance."
  
  (multiple-value-bind (vocabulary-package term)
                       (uri-vocabulary-components uri)
    (declare (ignore term))
    (let ((vocabulary-uri (package-name vocabulary-package)))
      (multiple-value-bind (loaded-uri loaded-resource-uri)
                           (rdf:load-vocabulary (mediator-repository mediator) vocabulary-uri)
        (unless (equal loaded-uri vocabulary-uri)
          (warn "Repository vocabulary base uri does not match given value: ~s != ~s."
                loaded-uri vocabulary-uri))
        (if ru-s
          (unless (equal loaded-resource-uri resource-uri)
            (warn "Repository vocabulary resource uri does not match given value: ~s != ~s."
                  loaded-resource-uri resource-uri))
          (setf resource-uri loaded-resource-uri))
        (let* ((name (first (rassoc vocabulary-uri (repository-namespace-bindings mediator) :test #'equal)))
               (vocabulary (make-instance 'vocabulary
                             :name (or name vocabulary-uri)
                             :uri vocabulary-uri
                             :resource-uri resource-uri)))
          (rdf:project-graph mediator vocabulary)
          vocabulary)))))



(:documentation  rdf:repository-value rdf:model-value
  "The repository-value and model-value functions map between the literal and resource domains
 in the respective rdf repository and the clos data model. Each is defined in terms of two parameters, the
 repository mediator and the data object. The resource identifier path relates the values in the data model,
 which concerns resource instances and their designators, which are represented as symbols or various uri
 objects, with those of the respective repository, each of which has its own uri representation. The literal
 path concerns lisp data objects - numbers and strings, which are represented in each repository as objects
 which wrap strings to combine them with type information.

 The operators construct/deconstruct instances as required and chace the relation in an identity map.
 For resources the map is two-way, while for literals just the data-to-rdf mapping implements an
 'equal' identity.")


(defmethod rdf:model-value :around ((mediator repository-mediator) (repository-value t))
  "A default wrapper method first looks in the cache, and delegates to the repository-specific method if there
 is a miss. Iff the specialized result differs, cache the correspondence."
  (or (gethash repository-value (mediator-repository2model-value-map mediator))
      (let ((model-value (call-next-method)))
        (cond ((eq model-value repository-value)
               repository-value)
              (t
               (register-value mediator model-value repository-value)
               model-value)))))
                               

(defmethod rdf:repository-value :around ((mediator repository-mediator) (model-value t))
  "A default wrapper method first looks in the cache, and delegates to the repository-specific method if there
 is a miss. Iff the specialized result differs, cache the correspondence."
  (or (gethash model-value (mediator-model2repository-value-map mediator))
      (let ((repository-value (call-next-method)))
        (cond ((eq repository-value model-value)
               model-value)
              (t
               (register-value mediator model-value repository-value)
               repository-value)))))


(defgeneric register-value (mediator model-value repository-value)
  (:documentation "Register the equivalence between a model value (a symbol, uuid, string, or number) and a
 repository value (a URI or literal) in the context of this mediated repository.
 Double-check for a previous equivalent and require any found correspondence to be equivalent to the new one.
 Returns the two values.")
  
  (:method ((mediator repository-mediator) model-value repository-value)
    (flet ((register (direction table key new)
             (multiple-value-bind (old old-t) (gethash key table)
               (if old-t
                 (cond ((rdf:equal old new) old)
                       ((and (rdf::literal-p old) (rdf::literal-p new))
                        ;; if both are literals, with identical strings, ignore it
                        old)
                       (t (cerror "Replace the value." "~a values conflict: ~s: new ~s != old ~s."
                                  direction key new old)
                          (setf (gethash key table) new)))
                 (setf (gethash key table) new)))))
      (register "model->repository" (mediator-model2repository-value-map mediator) model-value repository-value)
      (register "repository->model" (mediator-repository2model-value-map mediator) repository-value model-value))
  
    (values model-value repository-value)))


(defgeneric unregister-value (mediator model-value repository-value)
  (:documentation "Remove the given pair from the mediator's immediate repository-model values maps.
 This does _not_ trace parent chains, as that actions depends on more context.")

  (:method ((mediator repository-mediator) model-value repository-value)
    (values (when model-value (remhash model-value (mediator-model2repository-value-map mediator)))
            (when repository-value (remhash repository-value (mediator-repository2model-value-map mediator))))))


(defgeneric canonicalize-identifier (mediator identifier)
  (:method ((mediator repository-mediator) (identifier string))
    (funcall (mediator-identifier-function mediator) identifier))
  (:method ((mediator repository-mediator) (identifier symbol))
    (funcall (mediator-identifier-function mediator) identifier)))


;;;
;;; abstract operators

(defmethod rdf:delete-statement :before ((mediator t) (statement t))
  (if (repository-indelible? mediator)
    (error "Statements are indelible.")))

(defmethod rdf:delete-statement ((mediator repository-mediator) (statement rdf:triple))
  "if the repository is indelible cause an error, but if the
 repository permits revisions, remove the statement."
  (delete-statement* mediator (triple-subject statement) (triple-predicate statement) (triple-object statement)
                     (or (rdf:context statement) (mediator-default-context mediator))))


(defmethod rdf:insert-statement ((mediator repository-mediator) (statement rdf:triple))
  (unless (triple-id statement)
    (add-statement* mediator (triple-subject statement) (triple-predicate statement) (triple-object statement)
                    (or (rdf:context statement) (mediator-default-context mediator)))))


(defmethod rdf:has-statement? ((mediator repository-mediator) (statement rdf:triple))
  (flet ((probe (statement)
           (declare (ignore statement))
           (return-from rdf:has-statement? t)))
    (declare (dynamic-extent #'probe))
    (map-statements* #'probe mediator (triple-subject statement) (triple-predicate statement) (triple-object statement)
                     (or (rdf:context statement) (mediator-default-context mediator)))))


(defmethod rdf:has-context? ((mediator repository-mediator) (context t))
  (flet ((probe (statement)
           (declare (ignore statement))
           (return-from rdf:has-context? t)))
    (declare (dynamic-extent #'probe))
    (map-statements* #'probe mediator nil nil nil context)))


(defmethod rdf:has-object? ((mediator repository-mediator) (object t))
  (flet ((probe (statement)
           (declare (ignore statement))
           (return-from rdf:has-object? t)))
    (declare (dynamic-extent #'probe))
    (map-statements* #'probe mediator nil nil object nil)))


(defmethod rdf:has-predicate? ((mediator repository-mediator) (predicate t))
  (flet ((probe (statement)
           (declare (ignore statement))
           (return-from rdf:has-predicate? t)))
    (declare (dynamic-extent #'probe))
    (map-statements* #'probe mediator nil predicate nil nil)))


(defmethod rdf:has-subject? ((mediator repository-mediator) (subject t))
  (flet ((probe (statement)
           (declare (ignore statement))
           (return-from rdf:has-subject? t)))
    (declare (dynamic-extent #'probe))
    (map-statements* #'probe mediator subject nil nil nil)))



(:documentation rdf:project-graph rdf:load-vocabulary
  "Extract the stw vocabulary after having loaded it into a repository"
  (rdf:load-vocabulary (mediator-repository (wilbur-mediator))
                       "http://zbw.eu/namespaces/zbw-extensions/zbw-extensions.rdf")
  (rdf:load-vocabulary (mediator-repository (wilbur-mediator))
                       "http://zbw.eu/namespaces/zbw-extensions/")
  (rdf:project-graph (wilbur-mediator)
               (make-instance 'vocabulary :name "xbw" 
                              :uri "http://zbw.eu/namespaces/zbw-extensions/"
                              :resource-uri "http://zbw.eu/namespaces/zbw-extensions/zbw-extensions.rdf")))


(defmethod rdf:project-graph ((triple rdf:triple) (mediator repository-mediator))
  "Given a QUAD statement and a MEDIATOR, add the denoted triple to the repository repository, with optional
 temporally qualified association to a graph. The implementation depends on the repository schema."
  
  (unless (triple-id triple)
    (add-statement* mediator (triple-subject triple) (triple-predicate triple) (triple-object triple)
                    (or (rdf:context triple) (mediator-default-context mediator)))))


(defmethod rdf:project-graph ((enumerator function) (mediator repository-mediator))
  (flet ((insert-statement (statement)
           (rdf:insert-statement mediator statement)))
    (declare (dynamic-extent #'insert-statement))
    (funcall enumerator #'insert-statement)))


(defmethod rdf:project-graph ((mediator repository-mediator) (vocabulary vocabulary))
  "Extract the vocabulary's definitions from the repository.
 nb. query across contexts to collect definitions from all sources."

  ;; first, extract and collect the first-order vocabulary definitions
  (let ((vocabulary-uri (rdf:vocabulary-uri vocabulary))
        (vocabulary-resource-uri (rdf:vocabulary-resource-uri vocabulary))
        (definitions ())
        (definition-classes ())
        (missing-classes ())
        (package nil))
    (map nil #'(lambda (statement)
                 (when (or (rdf:query mediator :subject (rdf:subject statement) :predicate '{rdf}type :object '{rdfs}Class
                                      :context nil)
                           (rdf:query mediator :subject (rdf:subject statement) :predicate '{rdf}type :object '{owl}Class
                                      :context nil))
                   (push (rdf:repository-class-definition mediator (rdf:subject statement)) definitions)))
         (remove-duplicates
          (append (rdf:query mediator :predicate '{rdfs}isDefinedBy :object (repository-uri mediator vocabulary-uri)
                             :context nil)
                  (unless (equal vocabulary-resource-uri vocabulary-uri)
                    (rdf:query mediator :predicate '{rdfs}isDefinedBy :object (repository-uri mediator vocabulary-resource-uri)
                               :context nil))
                  ;; heavy-handed, but the way to find out what was in the document
                  (rdf:query mediator :context (repository-uri mediator vocabulary-resource-uri)))
          :key #'rdf:subject))
      
    ;; next, given any first-order definitions, continue to walk the class-precedence and property type graph
    ;; until it closes, as many classes (eg {foaf}Agent and {foaf}Group) include no isDefinedBy assertion.
    (setf definition-classes (mapcar #'second definitions))
    (setf (vocabulary-definitions vocabulary) definitions)
    (setf package (find-package (vocabulary-uri vocabulary)))
    (assert (packagep package) () "Missing vocabulary package: ~s." vocabulary)
    
    (do ((definition (pop definitions) (pop definitions)))
        ((null definition))
      (dolist (sd (fourth definition))
        (let ((datatype (getf (rest sd) :datatype)))
          ;; iff the slot's type is an unknown class, add it
          (when (and (eq (symbol-package datatype) package)
                     (not (find datatype definition-classes))
                     (rdf:query mediator :subject datatype :predicate '{rdf}type :object '{rdfs}Class :context nil))
            (push datatype missing-classes))))
      (dolist (superclass (third definition))
        ;; iff a superclass is an unknown class, add it
        (when (and (eq (symbol-package superclass) package)
                   (not (find superclass definition-classes)))
          (pushnew superclass missing-classes)))
      
      ;; generate and collect the definitions used by this class
      (do ((missing (pop missing-classes) (pop missing-classes)))
          ((null missing))
        (let ((missing-definition (repository-class-definition mediator missing)))
          (push missing definition-classes)
          (push missing-definition (vocabulary-definitions vocabulary))
          (push missing-definition definitions)))))
        
  ;; return the elaborated vocabulary
  vocabulary)

(defmethod rdf:query ((mediator repository-mediator) &key subject predicate object
                      (context (mediator-default-context mediator)) continuation offset limit)
  "The base method aligns the arguments, provides a default value for context, establishes a
 continuation constrained by offset and limit, and invokes map-statements*. It captures the
 results and arranges to either invoke an argument continuation on a statement with dynamic
 extent, or if no continuation was provided, to collect and return the results as a statement list."
  
  (dsu:collect-list (collect)
    (flet ((dynamic-collect (statement)
             (when (or (null offset) (minusp (decf offset)))
               (if (or (null limit) (not (minusp (decf limit))))
                 (collect (copy-statement statement))
                 (return))))
           (static-collect (statement)
             (when (or (null offset) (minusp (decf offset)))
               (if (or (null limit) (not (minusp (decf limit))))
                 (collect statement)
                 (return))))
           (constrained-continue (statement)
             (when (or (null offset) (minusp (decf offset)))
               (if (or (null limit) (not (minusp (decf limit))))
                 (funcall continuation statement)))))
      (declare (dynamic-extent #'dynamic-collect #'static-collect #'constrained-continue))
      (map-statements* (if continuation
                         (if (or offset limit) #'constrained-continue continuation)
                         (if (mediator-maps-dynamic-extent? mediator)
                           #'dynamic-collect #'static-collect))
                       mediator subject predicate object context))))


;;;
;;; concrete primitive operators: the interfaces


(defgeneric add-statement* (mediator subject predicate object context)
  (:documentation "Adds a single statement corresponding to the arguments to the repository.
 All constituents must be provided. If is is determined that the statement already exists, invokes
 duplicate-statement on the mediator and the constituents."))


(defgeneric delete-statement* (mediator subject predicate object context)
  (:documentation "Removes the single statement corresponding to the arguments from the repository.
 All constituents must be provided."))


(defgeneric map-statements* (continuation mediator subject predicate object context)
  (:documentation "Maps an operator over all designated statements.
 CONTINUATION : (function (statement) t) : applied to the respective statements in turn
 MEDIATOR : repository-mediator
 SUBJECT : (or literal identifier null)
 PREDICATE : (or literal identifier null)
 OBJECT : (or literal identifier null)
 CONTINUATION : (or literal identifier null)

 As invoked from query, if no context is provided, the repository's default context is substituted.
 Other components remain null, which serves as a wild-card."))


(defgeneric duplicate-statement (mediator  &key statement)
  (:documentation "Invoked from add-statement* if the repository indicates that the statement
 already exists. The base method signals the duplicate-statement condition, but returns if it
 is not handled.")

  (:method ((mediator repository-mediator) &key statement)
    "The base method signals the condition and returns if it is not handled."
    (signal 'duplicate-statement :mediator mediator :statement statement)))


;;;
;;; internal operators : extracting schema from a repository

(defgeneric rdf:repository-class-definition (repository identifier)
  (:documentation "Given a REPOSITORY and a class IDENTIFIER, construct a class definition based on the
 repository's assertions about the  class. Extract the supertypes based on {rdfs}subClassOf, slots based on
 {rdfs}domain, and documentation based on {rdfs}comment. Assert the class name as the datatype.")

  (:method ((mediator repository-mediator) uri)
    (flet ((object-value (stmt) (model-value mediator (rdf:object stmt))))
      (declare (dynamic-extent #'object-value))
      (let ((supertypes (mapcar #'object-value (rdf:query mediator :subject uri :predicate '{rdfs}subClassOf
                                                          :context nil)))
            (comments (mapcar #'object-value (rdf:query mediator :subject uri :predicate '{rdfs}comment
                                                        :context nil)))
            (properties (mapcar #'(lambda (statement) (rdf:repository-property-definition mediator (rdf:subject statement)))
                                (rdf:query mediator :object uri :predicate '{rdfs}domain
                                           :context nil)))
            (name (rdf:model-value mediator uri)))
        
        `(rdf:defclass ,name ,supertypes
           ,properties
           (:datatype ,name)
           ,@(when comments `(:documentation ,(format nil "~{~a~^~}" comments))))))))


(defgeneric rdf:repository-property-definition (repository identifier)
  (:documentation "Given a REPOSITORY and a predicate IDENTIFIER, construct a property definition based on the
 repository's assertions about the  predicate.")

  (:method ((mediator repository-mediator) uri)
    (flet ((model-value (uri) (model-value mediator uri)))
      (declare (dynamic-extent #'model-value))
      (let ((types (mapcar #'model-value
                           (mapcar #'rdf:object (rdf:query mediator :subject uri :predicate '{rdfs}range
                                                           :context nil))))
            (comments (mapcar #'rdf:object (rdf:query mediator :subject uri :predicate '{rdfs}comment
                                                      :context nil)))
            (name (model-value uri)))
        
        `(,name :type ,(uri-type mediator types)
                :datatype ,(if types (if (rest types) `(or ,@types) (first types)) '{rdfs}Literal)
                ,@(when comments `(:documentation ,(format nil "~{~a~^~}" comments))))))))


(defgeneric respository-schema-types (repository vocabulary-uri)
  (:method ((mediator repository-mediator) (uri t))
    (loop for statement in (rdf:query mediator :predicate '{rdfs}isDefinedBy :object (repository-uri mediator uri)
                                      :context nil)
          for subject = (rdf:subject statement)
          when (find-class (rdf:type-of mediator subject ) nil)
          collect (rdf:model-value mediator subject))))


(defgeneric uri-type (mediator uri-list)
  (:documentation "Convert a list of type resource URI into a Lisp type. A single type is mapped as a symbol.
 A list is converted into a disjunctive type.")

  (:method ((mediator repository-mediator) (uri-list null))
    t)

  (:method ((mediator repository-mediator) (uri-list cons))
    (let ((types (mapcar #'(lambda (uri) (model-value mediator uri)) uri-list)))
      (if (rest types)
        `(or ,@types)
        (first types)))))



;;; for a g5x32bit md5 / sha1 == 2.6 / 5.3
(defun compute-spoc-md5-id (&optional subject predicate object context)
  (let* ((p-pos (length subject))
         (o-pos (+ p-pos (length predicate)))
         (c-pos (+ o-pos (length object)))
         (length (+ c-pos (length context)))
         (buffer (make-array length :element-type '(unsigned-byte 8))))
    (declare (type fixnum length)
             (type (simple-array (unsigned-byte 8) (*)) buffer)
             (dynamic-extent buffer))
    (replace buffer subject)
    (replace buffer predicate :start1 p-pos)
    (replace buffer object :start1 o-pos)
    (replace buffer context :start1 c-pos)
    (ironclad:digest-sequence 'crypto:md5 buffer)))

(defun compute-spoc-sha1-id (&optional subject predicate object context)
  (let* ((p-pos (length subject))
         (o-pos (+ p-pos (length predicate)))
         (c-pos (+ o-pos (length object)))
         (length (+ c-pos (length context)))
         (buffer (make-array length :element-type '(unsigned-byte 8))))
    (declare (type fixnum length)
             (type (simple-array (unsigned-byte 8) (*)) buffer)
             (dynamic-extent buffer))
    (replace buffer subject)
    (replace buffer predicate :start1 p-pos)
    (replace buffer object :start1 o-pos)
    (replace buffer context :start1 c-pos)
    (ironclad:digest-sequence 'ironclad:sha1 buffer)))


(defun compute-spoc-id (subject predicate object context)
  (compute-spoc-md5-id subject predicate object context))


(defun compute-spoc-sha1-hex-id (&optional subject predicate object context)
  ;; as long as the base function does not pad for missing elements
  (with-output-to-string (stream)
    (loop for elt across (compute-spoc-sha1-id subject predicate object context)
          do (format stream "~(~2,'0x~)" elt))))

;;; (compute-spoc-hex-id #(1) #(2) #(3) #(4))
;;; (compute-spoc-hex-id nil #(2) #(3) nil)
;;; (compute-spoc-hex-id #(2) nil #(3) nil)
;;; (compute-spoc-hex-id nil (binary "<http://ar.to/#self>") nil nil)
;;; (compute-spoc-hex-id nil nil nil nil)

(defgeneric utf-8 (object)
  (:method ((object string))
    (trivial-utf-8:string-to-utf-8-bytes object)))


(:documentation rdf:model-value rdf:repository-value
   "The default method for a repository mediator encodes values as byte vectors. The structure is
 a single-element thrift struct. Each encoding method is an in-line field encoder. The single decoder
 fuction decodes the field header number and uses the field number to determine the type expected for the field.
 The language tags could be encoded as an additional field (#\a ?), but in order to do
 anything with them, one would need combine the tag with the string in a 'string' object to represent them
 outside of the repository.

   The base type discrimination for numbers distinguishes:
 
  - floating point as float and double-float
  - integer as i08, i16, i32, i64, and integer

  The former are always distinct, but can exclude one type if the runtime distinguishes three floating point classes.")


(defgeneric repository-uri (mediator namestring)
  (:documentation " map between uri and symbol representation")

  (:method ((mediator repository-mediator) (uri-namestring string))
    "The base method for mediators computes the respective symbol."
    (uri-namestring-identifier uri-namestring))

  (:method ((mediator repository-mediator) (uri symbol))
    uri))


(defgeneric camel-dash-canonicalizer (identifier)
  (:method ((string string))
    (let ((result (make-array (length string) :element-type 'character :fill-pointer 0 :adjustable t))
          (case :upper))
      (loop for c across string
            do (ecase case
                 (:lower (cond ((upper-case-p c)
                                (setf case :upper)
                                (vector-push-extend #\- result)
                                (vector-push-extend c result))
                               (t
                                (vector-push-extend (char-upcase c) result))))
                 (:upper (cond ((upper-case-p c)
                                (vector-push-extend c result))
                               (t
                                (setf case :lower)
                                (vector-push-extend (char-upcase c) result))))))
      (subseq result 0)))
  (:method ((symbol symbol))
    (let* ((string (symbol-name symbol))
           (result (make-array (length string) :element-type 'character :fill-pointer 0 :adjustable t))
           (state :letter))
      (loop for c across string
            do (ecase state
                 (:dash (setf state :letter)
                        (vector-push-extend (char-upcase c) result))
                 (:letter (case c
                            (#\- (setf state :dash))
                            (t (vector-push-extend (char-downcase c) result))))))
      (subseq result 0))))


(defgeneric identifier-identity (identifier)
  (:method ((string string)) string)
  (:method ((identifier symbol)) (symbol-name identifier)))


(thrift:def-struct "repository_value"
  "For use encoding rdf object values."
  (("string" nil  :id #.(char-code #\S) :type string :optional t)
   ("double" nil  :id #.(char-code #\d) :type thrift:double :optional t)
   ("float" nil   :id #.(char-code #\f) :type thrift:float :optional t)        ; inefficient, otherwise extend thrift
   ("i08" nil     :id #.(char-code #\B) :type thrift:i08 :optional t)
   ("i16" nil     :id #.(char-code #\U) :type thrift:i16 :optional t)
   ("i32" nil     :id #.(char-code #\I) :type thrift:i32 :optional t)
   ("i64" nil     :id #.(char-code #\L) :type thrift:i64 :optional t)
   ("integer" nil :id #.(char-code #\n) :type string :optional t)
   ("symbol" nil  :id #.(char-code #\y) :type thrift:binary :optional t)
   ("uri" nil     :id #.(char-code #\r) :type thrift:binary :optional t)
   ("uuid" nil    :id #.(char-code #\i) :type thrift:binary :optional t)
   ("binary" nil  :id #.(char-code #\b) :type thrift:binary :optional t)))


(defun make-vector-protocol (&rest args &key vector length)
  (declare (dynamic-extent args) (ignore vector length))
  (let ((transport (apply #'make-instance 'thrift:vector-stream-transport args)))
    (make-instance 'thrift:binary-protocol
           :direction :io 
           :input-transport transport
           :output-transport transport)))

(defun vector-input-protocol (vector)
  "Return the global vector-protocol with the given VECTOR as its input source. If no protocol instance
 is present make a new one."
  (if *vector-input-protocol*
    (setf (thrift:vector-stream-vector (thrift:protocol-output-transport *vector-input-protocol*))  vector)
    (setq *vector-input-protocol* (make-vector-protocol :vector vector)))
  *vector-input-protocol*)

(defun vector-output-protocol ()
  (or *vector-input-protocol*
      (setq *vector-input-protocol* (make-vector-protocol))))

(defmacro with-input-from-vector-stream ((vsp &key vector) &body body)
  `(let ((,vsp (vector-input-protocol ,vector)))
    ,@body))

(defmacro with-output-to-vector-stream ((vsp &rest args) &body body)
  `(let ((,vsp (vector-output-protocol ,@args)))
    ,@body
    (thrift:vector-stream-vector (thrift:protocol-output-transport ,vsp))))


(defmethod rdf:model-value ((mediator repository-mediator) (object vector))
  (with-input-from-vector-stream (stream :vector object)
    (multiple-value-bind (name id type) (thrift:stream-read-field-begin stream)
      (declare (ignore name type))
      (ecase id
        (#.(char-code #\S) (thrift:stream-read-string stream))
        (#.(char-code #\d) (thrift:stream-read-double stream))
        (#.(char-code #\f) (thrift:stream-read-float stream))
        (#.(char-code #\B) (thrift:stream-read-i08 stream))
        (#.(char-code #\U) (thrift:stream-read-i16 stream))
        (#.(char-code #\I) (thrift:stream-read-i32 stream))
        (#.(char-code #\L) (thrift:stream-read-i64 stream))
        (#.(char-code #\n) (parse-integer (thrift:stream-read-string stream)))
        (#.(char-code #\y) (flet ((canonicalize (fragment) (canonicalize-identifier mediator fragment)))
                             (declare (dynamic-extent #'canonicalize))
                             (uri-namestring-identifier (thrift:stream-read-string stream) #'canonicalize)))
        (#.(char-code #\r) (puri:parse-uri (thrift:stream-read-string stream)))
        (#.(char-code #\i) (uuid:byte-array-to-uuid (thrift:stream-read-binary stream)))
        (#.(char-code #\b) (thrift:stream-read-binary stream))))))


(defmethod rdf:repository-value ((mediator repository-mediator) (value string))
  (with-output-to-vector-stream (stream)
    (thrift:stream-write-struct stream (thrift:list (cons string value)) 'repository-value)))


(defmethod rdf:repository-value ((mediator repository-mediator) (value float))
  (with-output-to-vector-stream (stream)
    (thrift:stream-write-struct stream (thrift:list (cons float value)) 'repository-value)))


(defmethod rdf:repository-value ((mediator repository-mediator) (value double-float))
  (with-output-to-vector-stream (stream)
    (thrift:stream-write-struct stream (thrift:list (cons double value)) 'repository-value)))


(defmethod rdf:repository-value ((mediator repository-mediator) (value integer))
  (with-output-to-vector-stream (stream)
    (etypecase value
     (thrift:i08 (thrift:stream-write-struct stream (thrift:list (cons i08 value)) 'repository-value))
     (thrift:i16 (thrift:stream-write-struct stream (thrift:list (cons i16 value)) 'repository-value))
     (thrift:i32 (thrift:stream-write-struct stream (thrift:list (cons i32 value)) 'repository-value))
     (thrift:i64 (thrift:stream-write-struct stream (thrift:list (cons i64 value)) 'repository-value))
     (integer (let ((value (princ-to-string value)))
                (thrift:stream-write-struct stream (thrift:list (cons integer value)) 'repository-value))))))


(defmethod rdf:repository-value ((mediator repository-mediator) (value symbol))
  (flet ((canonicalize (symbol) (canonicalize-identifier mediator symbol)))
    (declare (dynamic-extent #'canonicalize))
    (let ((uri-namestring (symbol-uri-namestring value #'canonicalize)))
      (with-output-to-vector-stream (stream)
        (thrift:stream-write-struct stream (thrift:list (cons symbol uri-namestring)) 'repository-value)))))


(defmethod rdf:repository-value ((mediator repository-mediator) (identifier uuid:uuid))
  (let ((bytes (uuid:uuid-to-byte-array identifier)))
    (with-output-to-vector-stream (stream)
      (thrift:stream-write-struct stream (thrift:list (cons binary bytes)) 'repository-value))))


(defmethod rdf:repository-value ((mediator repository-mediator) (value puri:uri))
  (let ((uri-namestring (princ-to-string value)))
    (with-output-to-vector-stream (stream)
      (thrift:stream-write-struct stream (thrift:list (cons uri uri-namestring)) 'repository-value))))


;;; test default value representation

#-lispworks
(let ((rm (make-instance 'repository-mediator :vocabularies nil :repository nil))
      (values `("asdf" 2.0s0 2.0d0 1 ,(expt 2 8) ,(expt 2 16) ,(expt 2 32) ,(expt 2 64)
                #u"http://test" ,(uuid:make-v1-uuid))))
  (flet ((model-repository-value (x)
           (rdf:model-value rm (rdf:repository-value rm x))))
    (assert (every #'rdf:equal values (mapcar #'model-repository-value values)) ()
            "Some model->repository->model value failed:~% ~s~% ~s"
            values
            (mapcar #'(lambda (x) (rdf:repository-value rm x)) values))))
#+lispworks
(warn "not testing thrift encoding")

;;;
;;; versioning

(:documentation repository-next-object-version repository-get-object-version repository-commit-object-version
  "Each active object is associated with a lock in the repository. It is created when a new-persistent object
 is saved. The lock is global and contains the object's current version id.

 In order to progress to a new version, the attempt to advance the version succeeds if there is no intervening
 change and no change in progress. This is determined by an attempt to grab the subject's lock with the
 current local version. This succeeds if the lock was 'empty' and the version unchanged. If the versions do not
 match, some other update has committed, the lock cleared back to the stored version and the local commit fails.
 If it is already 'full', some other update is in progress, and the local cmmit fails.

 If the lock was grabbed, the commit proceeds, the subject's statements are written with a new graph id as
 context, that graph's metadata is written, and, as the last step, the object's version lock is cleared to
 the new version id.")


(defgeneric repository-lock-object-version (mediator subject-id version-id)
  (:documentation "Attempt to grab the object's version lock. This succeeds if the lock was 'empty'
 and the version is unchanged from the local object's.")

  (:method ((mediator repository-mediator) (subject-id t) (current-version-id t))
    (multiple-value-bind (stored-version-id locked-p)
                         (nbfeb-tfas mediator subject-id current-version-id)
      ;; if it is already locked, then another process is updating. this update fails.
      (unless locked-p
        ;; if it wasn't locked, check if the stored version is still the current.
        (cond ((rdf:equal current-version-id stored-version-id)
               ;; if it is, the clobal state is unchanged, so this change can proceed
               t)
              (t
               ;; otherwise, there was an intervening change. roll this one back and fail
               (nbfeb-sac mediator subject-id stored-version-id)))))))


(defgeneric repository-get-object-version (mediator subject-id)
  (:method ((mediator repository-mediator) (subject-id t))
    (nbfeb-load mediator subject-id)))


(defgeneric repository-commit-object-version (mediator subject-id new-version-id)
  (:documentation "Commit the new version by storing in the subject's lock and releasing the lock.")

  (:method ((mediator repository-mediator) (subject-id t) (new-version-id t))
    "Store the new if an release the lock."
    (nbfeb-sac mediator subject-id new-version-id)))


(defgeneric repository-write-transaction-metadata (mediator transaction)
  (:method ((mediator repository-mediator) (transaction transaction-open))
    (let ((graph (mediator-default-context mediator)))
      (add-statement* mediator (transaction-id transaction) '{rdf}type '{time}Interval graph)
      (add-statement* mediator (transaction-id transaction) '{rdfs}isDefinedBy (mediator-id mediator) graph)
      (add-statement* mediator (transaction-id transaction) '{time}hasBeginning (transaction-start transaction) graph)
      (add-statement* mediator (transaction-id transaction) '{time}hasEnd (transaction-end transaction) graph))))


(defvar +feb-retry-limit+ 10)
(defvar +feb-retry-delay+ 0.1)
(defvar +feb-false+ 0)
(defvar +feb-true+ 1)
(defvar +feb-context+ '|RDF|::|feb|)      ; as symbols to intern them w/o constaint
(defvar +feb-load+ '|RDF|::|feb-load|)
(defvar +feb-sac+ '|RDF|::|feb-sac|)
(defvar +feb-sas+ '|RDF|::|feb-sas|)
(defvar +feb-tfas+ '|RDF|::|feb-tfas|)
(defvar +feb-bit+ '|RDF|::|feb-bit|)
(defvar +feb-value+ '|RDF|::|feb-value|)



(defgeneric nbfeb-load (mediator location-id)
  (:documentation "Given a location, return its current flag and value.")

  (:method ((mediator repository-mediator) location-id)
    "Return the decoded state from the FEB location."
    ;; do not mark the location with an operation as this makes no
    (dotimes (count +feb-retry-limit+)
      (let* ((statements (rdf:query mediator :subject location-id :context +feb-context+))
             (bit (find +feb-bit+ statements :key #'rdf:predicate))
             (value (find +feb-value+ statements :key #'rdf:predicate)))
        (when (and bit value (null (cddr statements)))
          (return-from nbfeb-load
            (values (model-value mediator (rdf:object value)) (model-value mediator (rdf:object bit)))))))))


(defgeneric nbfeb-sac (mediator location-id value)
  (:documentation "Given a location, clear its flag and store the value.
 Return the old flag and value.")

  (:method ((mediator repository-mediator) location-id value)
    (flet ((do-sac (old-bit old-value)
             (unless (eql +feb-false+ (model-value mediator (rdf:object old-bit)))
               (delete-statement mediator old-bit)
               (add-statement* mediator location-id +feb-bit+ +feb-false+ +feb-context+))
             (delete-statement mediator old-value)
             (add-statement* mediator location-id +feb-value+ value +feb-context+)))
      (declare (dynamic-extent #'do-sac))
      (call-with-isolated-feb #'do-sac mediator location-id +feb-sac+))))


(defgeneric nbfeb-sas (mediator location-id value)
  (:documentation "Given a location, set its flag and store the value.
 Return the old flag and value.")

  (:method ((mediator repository-mediator) location-id value)
    (flet ((do-sas (old-bit old-value)
             (unless (eql +feb-true+ (model-value mediator (rdf:object old-bit)))
               (delete-statement mediator old-bit)
               (add-statement* mediator location-id +feb-bit+ +feb-true+ +feb-context+))
             (delete-statement mediator old-value)
             (add-statement* mediator location-id +feb-value+ value +feb-context+)))
      (declare (dynamic-extent #'do-sas))
      (call-with-isolated-feb #'do-sas mediator location-id +feb-sas+))))


(defgeneric nbfeb-tfas (mediator location-id value-id)
  (:documentation "Given a location, test its current flag. if clear, set the flag and store the value.
 Return the old flag and value.")

  (:method ((mediator repository-mediator) location-id value)
    (flet ((do-tfas (old-bit old-value)
             (when (eql +feb-false+ (model-value mediator (rdf:object old-bit)))
               (delete-statement mediator old-bit)
               (add-statement* mediator location-id +feb-bit+ +feb-false+ +feb-context+)
               (delete-statement mediator old-value)
               (add-statement* mediator location-id +feb-value+ value +feb-context+))))
      (declare (dynamic-extent #'do-tfas))
      (call-with-isolated-feb #'do-tfas mediator location-id +feb-tfas+))))


(defun call-with-isolated-feb (function mediator location-id feb-op)
  (dotimes (count +feb-retry-limit+)
    (add-statement* mediator location-id feb-op (mediator-id mediator) +feb-context+)
    (let* ((statements (rdf:query mediator :subject location-id :context +feb-context+))
           (old-bit (find +feb-bit+ statements :key #'rdf:predicate))
           (old-value (find +feb-value+ statements :key #'rdf:predicate)))
      (when (and old-bit old-value (null (cdddr statements)))
        (unwind-protect (funcall function old-value old-bit)
          (delete-statement* mediator location-id feb-op (mediator-id mediator) +feb-context+))
        (return-from call-with-isolated-feb
          (values (model-value mediator (rdf:object old-value)) (model-value mediator (rdf:object old-bit))))))
    ;; otherwise retry
    (delete-statement* mediator location-id feb-op (mediator-id mediator) +feb-context+)
    (sleep +feb-retry-delay+))
  (rdf:feb-timeout-error :mediator mediator :operation +feb-tfas+))

(defgeneric nbfeb-initialize (mediator location-id value)
  (:documentation "Given a location, initialize its flag to falseand store the value.
 Return the old flag and value.")

  (:method ((mediator repository-mediator) location-id value)
    (add-statement* mediator location-id +feb-bit+ +feb-false+ +feb-context+)
    (add-statement* mediator location-id +feb-value+ value +feb-context+)))
