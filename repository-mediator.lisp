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
  "The abstract resource-mediator class defines the interface to linked data models and repositories.
 It comprises metadata required to manage access to a concrete in-memory model or an external persistent store,
 maintains object identity, and transform representations, and bridge from an uniform opearation interface to
 the each concrete implementation. The interface comprises an operators set adapted from several established
 RDF libraries, [RDF2Go](http://semanticweb.org/wiki/RDF2Go), [RDF.rb]
  including default method implementations
 for any abstract operators. Operators which are readily implemented in terms of a store's concrete interface,
 need no abstract implementation. They are specialized directly for the respective concrete class

 Abstract methods:
 - load-repository
 - save-repository
 - repository-value
 - model-value
 - ensure-vocabulary
 - find-vocabulary
 - load-vocabulary
 - query
 - find-class
 - repository-class-definition
 - repository-property-definition
 - project-graph

 Specialized methods (see, eg. wilbur-mediator implementations):
 - has-statement?
 - has-subject?
 - has-predicate?
 - has-object?"))


(defparameter *resource-mediator.default* '(wilbur-mediator))


(defclass resource-mediator ()
  ((store
    :initform (error "store required") :initarg :store
    :reader repository-store :reader source-store
    :documentation "The repository store instance. Each concrete class entails its own specification form
     and must provide a default value if no initialization argument is given.")
   (transaction-cache
    :initform (make-hash-table)
    :reader repository-transaction-cache
    :documentation "Caches the instances created and/or modified during a transaction.")
   (instance-cache
    :initform (make-hash-table :test 'equal)
    :reader repository-instance-cache
    :documentation "URI-keyed cache for the instances present in the source repository and read into the
     application. Registers both transactiona and non-transactional instances to support interned
     instantiation. It needs to be weak, but that is not possible portably. An alternative would be weak
     entries, in which case it could as well be a weak avl/b+tree.")
   (state
    :initform rdf:non-transactional
    :reader repository-state :writer setf-repository-state
    :documentation "Indicates the mediated transaction state.")
   (vocabularies
    :initform nil :type list
    :reader repository-vocabularies :writer setf-repository-vocabularies
    :documentation "A list of URI namestrings which designate the vocabularies known to the source.
     The value is initialized from (subject . (query nil {rdf}:type {owl}:Ontology)) when the source is
     connected and is updated whenever a vocabulary is loaded.")
   (identifier-function
    :initform 'identifier-identity :initarg :identifier-function
    :reader repository-identifier-function
    :documentation "The canonicalization function for term names mapped between the storage and the model
     representations according to the convention that model terms are symbols while storage terms
     are strings.")
   (store2model-value-map
    :initform (make-hash-table :test 'equal)
    :reader repository-store2model-value-map
    :documentation "Maps resource URI to program symbols. The URI representation depends on the repository.")
   (model2repository-value-map
    :initform (make-hash-table :test 'equal)
    :reader repository-model2repository-value-map
    :documentation "Maps model identifiers to store value. The representation depends on the repository.")
   (default-context
     :initform (concatenate 'string "urn:sha1:" (compute-spoc-sha1-hex-id ))
     :reader repository-default-context
     :documentation "The URN used to identify the graph to used as the repository's default context."))

  (:default-initargs
    :vocabularies (list *rdf-vocabulary* *rdfs-vocabulary* *owl-vocabulary*
                        *xsd-vocabulary*))
  (:documentation "A resource-mediator encapsulates a concrete triple store to provide a standard repository
 interface to that store's operators and state and mediates between resource instances and the store.
 Concrete specializations are defined as
 - wilbur-mediator : for wilbur[1]
 - agraph-mediator : for allegrograph[2]

 ---
 [1]: http://wilbur-rdf.sourceforge.net/
 [2]: http://www.franz.com/allegrograph
 "))
  
(define-condition duplicate-statement (simple-error)
  ((store :initarg :store :reader error-store)
   (statement :initarg :statement :reader error-statement))
  (:report (lambda (c stream)
             (format stream "Statement exists in repository: ~a, ~s."
                     (error-store c) (error-statement c)))))


(defmethod print-object ((mediator resource-mediator) (stream t))
  (print-unreadable-object (mediator stream :identity t :type t)
    (format stream "mediating: ~a x ~a"
             (repository-store mediator)
             (repository-state mediator))))


(def-class-constructor resource-mediator (object &rest args)
  (:method ((null null) &key)
    nil)
  (:method ((default (eql t)) &key)
    (resource-mediator *resource-mediator.default*)))


(defmethod initialize-instance :after ((instance resource-mediator)
                                       &key vocabularies)

  "Complete initialization by incorporating vocabularies"
  (dolist (vocabulary vocabularies)
    (ensure-vocabulary instance vocabulary))
  #+ccl
  (ccl:terminate-when-unreachable instance))

#+ccl
(defmethod ccl:terminate ((object resource-mediator))
  (repository-close object))



(defgeneric repository-clear-instance-cache (source)
  (:method ((source resource-mediator))
    (clrhash (repository-instance-cache source))))


(defmethod repository-close ((repository resource-mediator))
  ;; the base method does nothing
  nil)

(defmethod rdf:project-graph ((enumerator function) (destination resource-mediator))
  (flet ((insert-statement (statement)
           (rdf:insert-statement destination statement)))
    (declare (dynamic-extent #'insert-statement))
    (funcall enumerator #'insert-statement)))


(defmethod rdf:load-repository ((repository resource-mediator) (location pathname))
  (rdf:project-graph location repository)
  (values (rdf:repository-count repository)
          location))


(defmethod rdf:query ((source resource-mediator) &rest args)
  "The base method translates the query constraints for the store and delegates to its implementation."
  (declare (dynamic-extent args))
  (destructuring-bind (&key subject predicate object graph &allow-other-keys) args
    (apply #'rdf:query (repository-store source)
           :subject (rdf:repository-value source subject)
           :predicate (rdf:repository-value source predicate)
           :object (rdf:repository-value source object)
           :graph (rdf:repository-value source graph)
           args)))


(defmethod rdf:save-repository ((source resource-mediator) location)
  (rdf:save-repository (repository-store source) location))


;;;
;;; identifier functions map between uri and symbol representation

(defmethod store-uri ((source resource-mediator) (uri-namestring string))
  "The base method for mediators computes the respective symbol."
  (uri-namestring-identifier uri-namestring))

(defmethod store-uri ((source resource-mediator) (uri symbol))
  uri)

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


(:documentation  rdf:repository-value rdf:model-value
  "The repository-value and model-value functions map between the literal and resource domains
 in the respective rdf store and the clos data model. Each is defined in terms of two parameters, the
 store mediator and the data object. The resource identifier path relates the values in the data model,
 which concerns resource instances and their designators, which are represented as symbols or various uri
 objects, with those of the respective store, each of which has its own uri representation. The literal
 path concerns lisp data objects - numbers and strings, which are represented in each store as objects
 which wrap strings to combine them with type information.

 The operators construct/deconstruct instances as required and chace the relation in an identity map.
 For resources the map is two-way, while for literals just the data-to-rdf mapping implements an
 'equal' identity.")

(defmethod rdf:model-value :around ((source resource-mediator) (repository-value t))
  "A default wrapper method first looks in the cache, and delegates to the repository-specific method if there
 is a miss. Iff the specialized result differs, cache the correspondence."
  (or (gethash repository-value (repository-store2model-value-map source))
      (let ((model-value (call-next-method)))
        (cond ((eq model-value repository-value)
               repository-value)
              (t
               (register-value source model-value repository-value)
               model-value)))))

(defmethod rdf:repository-value :around ((source resource-mediator) (model-value t))
  "A default wrapper method first looks in the cache, and delegates to the repository-specific method if there
 is a miss. Iff the specialized result differs, cache the correspondence."
  (or (gethash model-value (repository-model2repository-value-map source))
      (let ((repository-value (call-next-method)))
        (cond ((eq repository-value model-value)
               model-value)
              (t
               (register-value source model-value repository-value)
               repository-value)))))


(defgeneric register-value (source model-value repository-value)
  (:documentation "Register the equivalence between a model value (a symbol, uuid, string, or number) and a
 store value (a URI or literal) in the context of this mediated store.
 Double-check for a previous equivalent and require any found correspondence to be equivalent to the new one.
 Returns the two values.")
  
  (:method ((source resource-mediator) model-value repository-value)
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
      (register "model->store" (repository-model2repository-value-map source) model-value repository-value)
      (register "store->model" (repository-store2model-value-map source) repository-value model-value))
  
    (values model-value repository-value)))


(defgeneric unregister-value (source model-value repository-value)
  (:documentation "Remove the given pair from the mediator's immediate store-model values maps.
 This does _not_ trace parent chains, as that actions depends on more context.")

  (:method ((source resource-mediator) model-value repository-value)
    (values (when model-value (remhash model-value (repository-model2repository-value-map source)))
            (when repository-value (remhash repository-value (repository-store2model-value-map source))))))


(defgeneric canonicalize-identifier (source identifier)
  (:method ((source resource-mediator) (identifier string))
    (funcall (repository-identifier-function source) identifier))
  (:method ((source resource-mediator) (identifier symbol))
    (funcall (repository-identifier-function source) identifier)))



(:documentation  rdf:ensure-vocabulary rdf:find-vocabulary rdf:load-vocabulary
  "Support RDF schema by translating them into CLOS. Integrate them into the respective repository
 mediator to govern term mapping and to provide class definitions. Mediate the definition process through
 the repository's store in order to mitigate variations, inconsistencies, and general insufficiency
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
 - rdf:type-of : (identifier) given a resource identifier, return the type as cached or as asserted in the store.
 - rdf:find-class : (source identifier) given a class URI, locate or import the class definition.
 - rdf:load-vocabulary (source &key uri) : retrieve the vocabulary specification, parse it to
   extract the namespace and the schema and save them in the namespace registry. succeeds only
   with self-contained schema - those which are just properties of other schema yield just the
   terms, but no classes. (eg. http://purl.org/net/vocab/2003/11/photo.rdf)

 ---
 [1] : http://www.ilrt.bris.ac.uk/discovery/rdf-dev/purls/papers/QL98-distributed/
 ")


(defmethod rdf:ensure-vocabulary ((source resource-mediator) (uri string) &rest args)
  (or (find-vocabulary source uri)
      ;; otherwise continue to load it and incorporate its terms
      (ensure-vocabulary source (apply #'load-vocabulary source uri args))))


(defmethod rdf:ensure-vocabulary ((source resource-mediator) (vocabulary vocabulary) &key)
  (unless (find vocabulary (repository-vocabularies source))
    (rdf:load-vocabulary source vocabulary))
  vocabulary)


(defmethod rdf:ensure-vocabulary ((source resource-mediator) (uri symbol) &rest args)
  (apply #'rdf:ensure-vocabulary source (package-name (symbol-package uri)) args))


(defmethod rdf:find-vocabulary ((source resource-mediator) (uri string))
  (dolist (vocabulary (repository-vocabularies source))
    (when (uri-match-p uri (vocabulary-uri vocabulary))
      ;; if the namespace is already present - on the basis of bindings, then return.
      (return vocabulary))))


(defmethod (setf rdf:find-vocabulary) ((value null) (source resource-mediator) (uri string))
  (flet ((vocabulary-match-p (vocabulary)
           (let ((v-uri (vocabulary-uri vocabulary)))
             (or (uri-match-p uri v-uri) (uri-match-p v-uri uri)))))
    (declare (dynamic-extent #'vocabulary-match-p))
    (setf-repository-vocabularies (remove-if #'vocabulary-match-p (repository-vocabularies source))
                                  source))
  nil)

(defmethod (setf rdf:find-vocabulary) ((vocabulary vocabulary) (source resource-mediator) (uri string))
  (flet ((vocabulary-match-p (vocabulary)
           (let ((v-uri (vocabulary-uri vocabulary)))
             (or (uri-match-p uri v-uri) (uri-match-p v-uri uri)))))
    (declare (dynamic-extent #'vocabulary-match-p))
    (setf-repository-vocabularies (cons vocabulary
                                        (remove-if #'vocabulary-match-p (repository-vocabularies source)))
                                  source))
  vocabulary)


(defmethod rdf:load-vocabulary ((source resource-mediator) (vocabulary vocabulary)
                                &key (resource-uri (vocabulary-uri vocabulary)))
  "Incorporate a vocabulary definition into a source.
 SOURCE : resource-mediator
 VOCABULARY : vocabulary

 Add the vocabulary to the source and include it's terms in the source's identifier map.
 Replaces an existing instance, but an existing instance, but does not attempt to expunge its terms."

  ;; update the local registry
  (setf (rdf:find-vocabulary source resource-uri) vocabulary)
    
  ;; augment the identifier cache
  (loop for (symbol . uri-namestring) in (vocabulary-identifier-map vocabulary)
        do (register-value source symbol (store-uri source uri-namestring)))
  
  ;; return the vocabulary
  vocabulary)


(defmethod rdf:load-vocabulary ((source resource-mediator) (uri string) &key (resource-uri nil ru-s))
  "Load the schema into the repository store based a vocabulary uri. Note the actual location and warn
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
                           (rdf:load-vocabulary (repository-store source) vocabulary-uri)
        (unless (equal loaded-uri vocabulary-uri)
          (warn "Repository vocabulary base uri does not match given value: ~s != ~s."
                loaded-uri vocabulary-uri))
        (if ru-s
          (unless (equal loaded-resource-uri resource-uri)
            (warn "Repository vocabulary resource uri does not match given value: ~s != ~s."
                  loaded-resource-uri resource-uri))
          (setf resource-uri loaded-resource-uri))
        (let* ((name (first (rassoc vocabulary-uri (repository-namespace-bindings source) :test #'equal)))
               (vocabulary (make-instance 'vocabulary
                             :name (or name vocabulary-uri)
                             :uri vocabulary-uri
                             :resource-uri resource-uri)))
          (rdf:project-graph source vocabulary)
          vocabulary)))))


(defmethod rdf:project-graph ((source resource-mediator) (vocabulary vocabulary))
  "Extract the vocabulary's definitions from the repository."

  ;; first, extract and collect the first-order vocabulary definitions
  (let ((vocabulary-uri (rdf:vocabulary-uri vocabulary))
        (vocabulary-resource-uri (rdf:vocabulary-resource-uri vocabulary))
        (definitions ())
        (definition-classes ())
        (missing-classes ())
        (package nil))
    (map nil #'(lambda (statement)
                 (when (or (rdf:query source :subject (rdf:subject statement) :predicate '{rdf}type :object '{rdfs}Class)
                           (rdf:query source :subject (rdf:subject statement) :predicate '{rdf}type :object '{owl}Class))
                   (push (rdf:repository-class-definition source (rdf:subject statement)) definitions)))
         (append (rdf:query source :predicate '{rdfs}isDefinedBy :object (store-uri source vocabulary-uri))
                 (unless (equal vocabulary-resource-uri vocabulary-uri)
                   (rdf:query source :predicate '{rdfs}isDefinedBy :object (store-uri source vocabulary-resource-uri)))))
      
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
                     (rdf:query source :subject datatype :predicate '{rdf}type :object '{rdfs}Class))
            (push datatype missing-classes))))
      (dolist (superclass (third definition))
        ;; iff a superclass is an unknown class, add it
        (when (and (eq (symbol-package superclass) package)
                   (not (find superclass definition-classes)))
          (pushnew superclass missing-classes)))
      
      ;; generate and collect the definitions used by this class
      (do ((missing (pop missing-classes) (pop missing-classes)))
          ((null missing))
        (let ((missing-definition (repository-class-definition source missing)))
          (push missing definition-classes)
          (push missing-definition (vocabulary-definitions vocabulary))
          (push missing-definition definitions)))))
        
  ;; return the elaborated vocabulary
  vocabulary)


(:documentation rdf:project-graph rdf:load-vocabulary
  "Extract the stw vocabulary after having loaded it into a store"
  (rdf:load-vocabulary (repository-store (wilbur-mediator)) "http://zbw.eu/namespaces/zbw-extensions/zbw-extensions.rdf")
  (rdf:load-vocabulary (repository-store (wilbur-mediator)) "http://zbw.eu/namespaces/zbw-extensions/")
  (rdf:project-graph (wilbur-mediator)
               (make-instance 'vocabulary :name "xbw" 
                              :uri "http://zbw.eu/namespaces/zbw-extensions/"
                              :resource-uri "http://zbw.eu/namespaces/zbw-extensions/zbw-extensions.rdf")))


(defgeneric rdf:repository-class-definition (repository identifier)
  (:documentation "Given a REPOSITORY and a class IDENTIFIER, construct a class definition based on the
 store's assertions about the  class. Extract the supertypes based on {rdfs}subClassOf, slots based on
 {rdfs}domain, and documentation based on {rdf}comment. Assert the class name as the datatype.")

  (:method ((source resource-mediator) uri)
    (flet ((object-value (stmt) (model-value source (rdf:object stmt))))
      (declare (dynamic-extent #'model-value))
      (let ((supertypes (mapcar #'object-value (rdf:query source :subject uri :predicate '{rdfs}subClassOf)))
            (comments (mapcar #'object-value (rdf:query source :subject uri :predicate '{rdf}comment)))
            (properties (mapcar #'(lambda (statement) (repository-property-definition source (rdf:subject statement)))
                                (rdf:query source :object uri :predicate '{rdfs}domain)))
            (name (rdf:model-value source uri)))
        
        `(rdf:defclass ,name ,supertypes
           ,properties
           (:datatype ,name)
           ,@(when comments `(:documentation ,(format nil "~{~a~^~}" comments))))))))


(defgeneric rdf:repository-property-definition (repository identifier)
  (:documentation "Given a REPOSITORY and a predicate IDENTIFIER, construct a property definition based on the
 store's assertions about the  predicate.")

  (:method ((source resource-mediator) uri)
    (flet ((model-value (uri) (model-value source uri)))
      (declare (dynamic-extent #'model-value))
      (let ((types (mapcar #'model-value
                           (mapcar #'rdf:object (rdf:query source :subject uri :predicate '{rdfs}range))))
            (comments (mapcar #'rdf:object (rdf:query source :subject uri :predicate '{rdf}comment)))
            (name (model-value uri)))
        
        `(,name :type ,(uri-type source types)
                :datatype ,(if types (if (rest types) `(or ,@types) (first types)) '{rdfs}Literal)
                ,@(when comments `(:documentation ,(format nil "~{~a~^~}" comments))))))))


(defgeneric respository-schema-types (repository vocabulary-uri)
  (:method ((source resource-mediator) (uri t))
    (loop for statement in (rdf:query source :predicate '{rdfs}isDefinedBy :object (store-uri source uri))
          for subject = (rdf:subject statement)
          when (find-class (rdf:type-of source subject ) nil)
          collect (rdf:model-value source subject))))


(defmethod rdf:find-class ((source resource-mediator) (name symbol) &key (error-p t))
  (or (rdf:find-class (class-of source) name :error-p nil)
      (let ((vocabulary (ensure-vocabulary source name)))
        (when vocabulary
          (or (let ((definition (rdf:find-class vocabulary name :error-p nil)))
                (when definition
                  (prog1 (eval definition)
                    (dolist (superclass (third definition))
                      (rdf:find-class source superclass)))))
              (let ((definition (rdf:repository-class-definition source (store-uri source name)))
                    (succeeded nil))
                (when definition
                  ;; handle circular references, but don't leave erroneous definitions registered
                  (setf (rdf:find-class vocabulary name) definition)
                  (unwind-protect (prog1 (eval definition)
                                    (dolist (superclass (third definition))
                                      (rdf:find-class source superclass))
                                    (setf succeeded t))
                    (unless succeeded
                      (setf (rdf:find-class vocabulary name) nil)
                      (setf (find-class name) nil))))))))
      (when error-p
        (rdf:class-not-found (find-class 'resource-class) name))))


(defmethod rdf:find-class ((source resource-mediator) (identifier t) &rest args)
  (declare (dynamic-extent args))
  (apply #'rdf:find-class source (rdf:model-value source identifier) args))


(defgeneric uri-type (source uri-list)
  (:documentation "Convert a list of type resource URI into a Lisp type. A single type is mapped as a symbol.
 A list is converted into a disjunctive type.")

  (:method ((source resource-mediator) (uri-list null))
    t)

  (:method ((source resource-mediator) (uri-list cons))
    (let ((types (mapcar #'(lambda (uri) (model-value source uri)) uri-list)))
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

(defmacro with-input-from-vector-stream ((vsp &key vector) &body body)
  `(let ((,vsp (make-vector-protocol :vector ,vector)))
    ,@body))

(defmacro with-output-to-vector-stream ((vsp &rest args) &body body)
  `(let ((,vsp (make-vector-protocol ,@args)))
    ,@body
    (let ((vs (thrift:protocol-output-transport ,vsp)))
      (thrift:vector-stream-vector vs))))

(defmethod rdf:model-value ((source resource-mediator) (object vector))
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
        (#.(char-code #\y) (flet ((canonicalize (fragment) (canonicalize-identifier source fragment)))
                             (declare (dynamic-extent #'canonicalize))
                             (uri-namestring-identifier (thrift:stream-read-string stream) #'canonicalize)))
        (#.(char-code #\r) (puri:parse-uri (thrift:stream-read-string stream)))
        (#.(char-code #\i) (uuid:byte-array-to-uuid (thrift:stream-read-binary stream)))
        (#.(char-code #\b) (thrift:stream-read-binary stream))))))

(defmethod rdf:repository-value ((source resource-mediator) (value string))
  (with-output-to-vector-stream (stream :length (+ (length value) 7))
    (thrift:stream-write-struct stream (thrift:list (cons string value)) 'repository-value)))

(defmethod rdf:repository-value ((source resource-mediator) (value single-float))
  (with-output-to-vector-stream (stream)
    (thrift:stream-write-struct stream (thrift:list (cons float value)) 'repository-value)))

(defmethod rdf:repository-value ((source resource-mediator) (value double-float))
  (with-output-to-vector-stream (stream)
    (thrift:stream-write-struct stream (thrift:list (cons double value)) 'repository-value)))

(defmethod rdf:repository-value ((source resource-mediator) (value integer))
  (with-output-to-vector-stream (stream)
    (etypecase value
     (thrift:i08 (thrift:stream-write-struct stream (thrift:list (cons i08 value)) 'repository-value))
     (thrift:i16 (thrift:stream-write-struct stream (thrift:list (cons i16 value)) 'repository-value))
     (thrift:i32 (thrift:stream-write-struct stream (thrift:list (cons i32 value)) 'repository-value))
     (thrift:i64 (thrift:stream-write-struct stream (thrift:list (cons i64 value)) 'repository-value))
     (integer (let ((value (princ-to-string value)))
                (thrift:stream-write-struct stream (thrift:list (cons integer value)) 'repository-value))))))

(defmethod rdf:repository-value ((source resource-mediator) (value symbol))
  (flet ((canonicalize (symbol) (canonicalize-identifier source symbol)))
    (declare (dynamic-extent #'canonicalize))
    (let ((uri-namestring (symbol-uri-namestring value #'canonicalize)))
      (with-output-to-vector-stream (stream)
        (thrift:stream-write-struct stream (thrift:list (cons symbol uri-namestring)) 'repository-value)))))

(defmethod rdf:repository-value ((source resource-mediator) (identifier uuid:uuid))
  (let ((bytes (uuid:uuid-to-byte-array identifier)))
    (with-output-to-vector-stream (stream)
      (thrift:stream-write-struct stream (thrift:list (cons binary bytes)) 'repository-value))))


(defmethod rdf:repository-value ((source resource-mediator) (value puri:uri))
  (let ((uri-namestring (princ-to-string value)))
    (with-output-to-vector-stream (stream)
      (thrift:stream-write-struct stream (thrift:list (cons uri uri-namestring)) 'repository-value))))

(let ((rm (make-instance 'resource-mediator :vocabularies nil :store nil))
      (values `("asdf" 2.0s0 2.0d0 1 ,(expt 2 8) ,(expt 2 16) ,(expt 2 32) ,(expt 2 64)
                #u"http://test" ,(uuid:make-v1-uuid))))
  (flet ((model-repository-value (x)
           (rdf:model-value rm (rdf:repository-value rm x))))
    (assert (every #'rdf:equal values (mapcar #'model-repository-value values)) ()
            "Some model->repository->model value failed:~% ~s~% ~s"
            values
            (mapcar #'(lambda (x) (rdf:repository-value rm x)) values))))
          
