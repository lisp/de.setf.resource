;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.resource.implementation; -*-

(in-package :de.setf.resource.implementation)

(:documentation
  "This file defines a the API for the `de.setf.resource` CLOS linked data library."
  
  (copyright
   "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
   "'de.setf.resource' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.resource' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.resource' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/).")
  
  (description "The interface operator names follow from the standard RDF terms. The structure intends to be
 consistent with the spira.rb precedents, with allowance for standard Lisp idioms.
 The central terms are
 - rdf:statement  : the combination of an rdf:subject, rdf:predicate, and rdf:object
 - rdf:subject : a resource in a statement
 - rdf:predicate : property relating subject and object in a statement
 - rdf:object : a resource in a statement
 See the rdf-schema[1] tr for background on the terms.

 Other resource models:
  RAP[2] : goes only as far as addProperty, lispProperties
  Jena[3] - the OWL-based model concerns just inference within and externalized from the rdf graph, but not with
   reified classes
 ---
 [1] : http://www.w3.org/TR/rdf-schema/
 [2] : http://www.seasr.org/wp-content/plugins/meandre/rdfapi-php/doc/tutorial/introductionToRAP.htm
 [3] : http://jena.sourceforge.net/how-to/model-factory.html
 "))


(defgeneric rdf:class-not-found (metaclass type)
  (:documentation "Invoked from rdf:find-class when no metaclass instance comprises the statement.
 The base method signals an rdf:class-not-found-error."))


(defgeneric rdf:commit (object)
  (:documentation "Given a new or modified persistent object, write content to the persistent repository, and
 make the object hollow. This performs both of the phases which complete a transaction in order to prevent
 stale state."))


(defgeneric rdf:context (statement)
  (:documentation "Return the statement's context graph. If the statement is a triple, return nil.")
  (:method ((statement null)) nil))


(defgeneric rdf:delete (object)
  (:documentation "Given a persistent object, delete all concrete statements which include it as subject.
 If the objects is hollow, it comprises its URI only. This is used to retrieve anew and delete all related
 assertions form the repository. If the object has been read from the repository, the related statements are
 cached in relation to the slots. These cached statements are deleted from the repository."))


(defgeneric rdf:delete-subject (context subject)
  (:documentation "Delete all statements relatd to this object from the given CONTEXT.
 CONTEXT : (or repository resource-object)
 SUBJECT : identifier

 If the context is a repository, remove all triples with that subject. If the context is an
 object remove it fron it's repository's index."))


(defgeneric rdf:delete-statement (context statement)
  (:documentation "Delete the statement from the given CONTEXT.
 CONTEXT : (or repository resource-object)
 STATEMENT : rdf:statement

 If the context is a repository, remove the statement's triple. If the context is an
 object unbind the predicated property."))


(defgeneric rdf:ensure-instance (context identifier)
  (:documentation "Attempt to find the designated instance relative to the context. (cf find-instance).
 CONTEXT : (or resource-class repository-mediator)
 IDENTIFIER : identifier

 The identifier forms depend on the context. For a mediated context, any model representation is mapped to
 that of the respective repository. If no instance exists create one.")

  (:method ((class-name symbol) identifier)
    (rdf:ensure-instance (find-class class-name) identifier)))


(defgeneric rdf:ensure-vocabulary (mediator uri &key)
  (:documentation "Iff the vocabulary is not integrated into the source mediator, load it from the source,
 incorporate its terms and register it as a handle on the class declarations."))


(defgeneric rdf:equal (object1 object2)
  (:documentation "Return true iff the two objects denote the same resource or literal.
 Resource objects map to their respective URI, URI are compared bu component, and other
 domain-model objects according to repository mediator.")

  (:method ((object1 t) (object2 t))
    (equalp object1 object2))

  (:method ((object1 puri:uri) (object2 puri:uri))
    (macrolet ((part-equal (accessor)
                 `(equal (,accessor object1) (,accessor object2))))
      (and (part-equal puri:uri-scheme)
           (part-equal puri:uri-host)
           (part-equal puri:uri-port)
           (part-equal puri:uri-path)
           (part-equal puri:uri-query)
           (part-equal puri:uri-fragment)
           (part-equal puri:uri-plist)))))


(defgeneric rdf:evict (object)
  (:documentation "Remove the object from a transaction's state and clear its content.
 Permitted in the clean-persistent state only."))


(defgeneric rdf:find-class (context type &key error-p)
  (:documentation "Find the class designated by TYPE in the specifiec CONTEXT.
 CONTEXT : (or resource-class repository-mediator)
 TYPE : rdf:identifier
 If the context is a resource-class, look for an instance of its vocabulary or delegate to its source.
 If the context is a repository-mediator, seach the vocabularies.
 If none is found and error-p is true, apply the metaclass' class-not-found-function to it and the type.
 By default this signals a continuable class-not-found-error.")

  (:method ((metaclass symbol) type &rest args)
    "Allow class designators for the metaclasses as they are defined globally."
    (declare (dynamic-extent args))
    (apply #'rdf:find-class (find-class metaclass) type args)))


(defgeneric rdf:find-instance (context identifier)
  (:documentation "Find the designated instance relative to the class.
 If the designator is a string, interpret it as a resource identifier - either absolute or just the fragment.
 If it is an expression, treat it as a query against the class' default source.")
  
  (:method ((class-name symbol) identifier)
    (rdf:find-instance (find-class class-name) identifier)))

(defgeneric (setf rdf:find-instance) (instance class identifier)
  (:documentation "Find the designated instance relative to the class.
 If the designator is a string, interpret it as a resource identifier - either absolute or just the fragment.
 If it is an expression, treat it as a query against the class' default source.")

  (:method (instance (class-name symbol) identifier)
    (setf (rdf:find-instance (find-class class-name) identifier) instance)))


(defgeneric rdf:find-vocabulary (mediator uri)
  (:documentation "Attempt to locate a vocabulary given its base URI or that of a term.
 Return nil if none is found."))


(defgeneric rdf:has-context? (repository context)
  (:documentation "Return true of the repository state includes an assertion with the object
 REPOSITORY : repository
 OBJECT : t"))


(defgeneric rdf:has-predicate? (repository predicate)
  (:documentation "Return true of the repository state includes an assertion with the predicate
 REPOSITORY : (or resource-object repository)
 PREDICATE : identifier"))


(defgeneric rdf:has-object? (repository object)
  (:documentation "Return true of the repository state includes an assertion with the object
 REPOSITORY : repository
 OBJECT : t"))


(defgeneric rdf:has-statement? (repository statement)
  (:documentation "Return true of the repository state includes the assertion
 REPOSITORY : (or resource-object repository)
 STATEMENT : statement"))


(defgeneric rdf:has-subject? (repository subject)
  (:documentation "Return true of the context state includes an assertion about the subject
 REPOSITORY : (or resource-object repository)
 SUBJECT : identifier"))


(defgeneric rdf:id (statement)
  (:documentation "Return the statement's id if it has one.")
  (:method ((statement null)) nil))


(defgeneric rdf:insert-statement (repository statement)
  (:documentation "Assert the statement in the given repository.
 REPOSITORY : (or resource-object repository)
 STATEMENT : rdf:statement

 If the context is a repository, add the statement as a triple. If the context is an
 object bind the predicated object to the subject."))


(defgeneric rdf:instance-not-found (class identifier)
  (:documentation "Invoked from find-instance if the given uri does not designate an instance
 in the context of the class and/or its respective persistent repository.")

  (:method ((class standard-class) (identifier t))
    (rdf:instance-not-found-error :class class :uri identifier)))


(defgeneric rdf:load-repository (repository location)
  (:documentation "Load a REPOSITORY from a give LOCATION.
 REPOSITORY : repository-mediator
 LOCATION : (or pathname uri stream)

 Reads RDF content from a location and adds the triples to the mediated repository."))


(defgeneric rdf:load-repository-as (repository location form)
  (:documentation "Decode the content from the source LOCATION as FORM and load it into the
 repository."))  


(defgeneric rdf:load-vocabulary (repository vocabulary-uri &key )
  (:documentation "Load the vocabulary schema identified by the given URI into the runtime.
 SOURCE : (or repository-mediator pathname) 
 URI : string : a URI namestring which locates the vocabulary schema directly, or a URI reference,
   which identifies one of the vocabulary terms.
 VALUE : vocabulary : the instantiated vocabulary.
 :RESOURCE-VOCABULARY : some methods accept the keyword to specify the vocabulary uri explicitly

 Given a repository as the context, the URI is adjusted as required to identify
 the vocabulary and used to load the vocabulary into the repository. The vocabulary class definitions are extracted
 from the repository and used to initialize the vocabulary.

 GIven a pathname as the context, the file is read as a vocabulary definition.

 Note that this process relies on {rdfs}isDefinedBy assertions to recognize the vocabulary's definitions.
 It also does not attempt to derive class definitions from autonomous predicate definitions with
 vocabulary cross-references, but instead limits the definition to classes."))


(defgeneric rdf:make-persistent (object)
  (:documentation "When applied to a transient object in a transaction, the object is registered to
 be stored when the transaction completes and the state is changed to mark it new-persistent, in which state
 it maintains a modification history."))


(defgeneric rdf:map-property-slots (function resource-object)
  (:documentation "Map the function over the subject's predicate slot definitions.
 FUNCTION : function : The function should accept one argument, the property definition object.
 SUBJECT : resource-object
 VALUES : t : the base method returns the subject instance")
  (:method-combination progn)

  (:method progn ((function t) (subject t))
           subject))


(defgeneric rdf:map-property-predicates (function subject)
  (:documentation "Map the function over the context's property slot predicates.
 FUNCTION : function : The function should accept one argument, the property predicate identifier.
 SUBJECT : resource-object
 VALUES : t : the base method returns the subject instance

 The function should accept one argument, the predicate. For property classes
 implemented in terms of map-property-slots. For repositories, implemented in terms
 of the respective statement enumeration mechanism.")
  (:method-combination progn)

  (:method progn ((function t) (subject t))
           subject))


(defgeneric rdf:map-property-values (function subject)
  (:documentation "Map the function over the subject's predicate slots' values.
 FUNCTION : function: The function should accept one argument, the property value.
 SUBJECT : resource-object
 VALUES : t : the base method returns the subject instance

  The function is called with each individual _model-domain_ value in turn. Slots' sequence values are passed
 element-by-element. Unbound slots of a resource-object are skipped. Meathods are combined by progn and the
 default  method returns the context object. The base method defined for resource-object iterates over all
 prototypal properties and each resource-class definition generates a method for the respective direct slots.")
  (:method-combination progn)

  (:method progn ((function t) (subject t))
    subject))


(defgeneric rdf:map-statements (function source)
  (:documentation "This is a special projection case, in which the destination is a consumer function and the
 source applies that function to each statement it contains.")

  (:method ((function function) source)
    (rdf:project-graph source function)))


(defgeneric rdf:model-value (repository object)
  (:documentation "Map the given OBJECT from the REPOSITORY store's domain to the model domain.
 Decode literal values according to their annotation and transform URI into uuid or symbols.
 Each source implements its own methods for literals and URI to operate on the respective
 representation. Return objects already in the model domain unchanged.")
  (:argument-precedence-order object repository)

  (:method ((mediator t) (value t))
    "The default method serves as the primary for :around's which short-circuit the cache"
    value)
  (:method ((source t) (value null))
    nil)
  (:method ((map null) (value t))
    ;; terminate at the end of the cache chain
    nil)

  (:method ((source t) (value symbol))
    value)
  (:method ((source t) (value string))
    value)
  (:method ((source t) (value number))
    value)

  (:method ((map hash-table) (repository-value t))
    "One path of the process devolves to a search of the mediator's cached objets.
     In which case, the caches can be chained."
    (or (gethash repository-value map)
        (rdf:model-value (hash-table-parent map) repository-value))))


(defgeneric rdf:object (statement)
  (:documentation "Return the statement's object as a URI, a node, or a literal, as per
 the source repository's representation.")
  (:method ((statement null)) nil))

(defgeneric rdf:object-value (source statement)
  (:documentation "Return the statement's object as a URI or a literal in the model domain, as
 translated by the repository mediator."))


(defgeneric rdf:predicate (statement)
  (:documentation "Return the statement's predicate as a URI, a node, or a literal, as per
 the source repository's representation.")
  (:method ((statement null)) nil))

(defgeneric rdf:predicate-value (repository statement)
  (:documentation "Return the statement's predicate as a URI or a literal in the model domain, as
 translated by the repository mediator."))


(defgeneric rdf:project-graph (source destination)
  (:documentation "Project a model/graph/repository from a source onto a destination.
 SOURCE : (or stream list resource-object repository-mediator function)
 DESTINATION : (or stream list resource-object repository-mediator function)

 The source can be in the form of a repository, a concrete statement extension, an enumerable extension, or a
 model instance. For a model which exists as a concrete instance of a statement extension, this projects
 the statement set, for example, by iterating the set and adding each statement to the destination. For a
 repository, the extent is constrained by combining the repository with a selection query.

 The destination can be specified as a single instance, in which case each applicable statement is applied to
 the instance in turn to the respective property slot, a class, in which case an instance extension results,
 based on those statements which pertain to instances of the class, or a meta-class, in which case an
 extension results based on the entire ontology comprised by the meta-class.

 Default methods are defined for stream, list, resource-object, and repository-mediator to operate on
 the respective content.")

  (:method ((source stream) (function function))
    (let ((statement nil))
    (loop (unless (setf statement (read source nil nil))
            (return))
          (funcall function statement))))

  (:method ((graph list) (destination function))
    (dolist (s graph)
      (funcall destination s)))

  (:method ((graph list) (destination t))
    (dolist (s graph)
      (rdf:project-graph s destination)))

  (:method ((source t) (destination symbol))
    (rdf:project-graph source (find-class destination)))

  (:method ((enumerator function) (consumer function))
    "Combine two operators as an enumerator and a consumer by applying the enumerator to the consumer.
 The former then the latter with the successive elements."
    (funcall enumerator consumer)))


(defgeneric rdf:property-missing (class object statement operation &optional value)
  (:documentation "Invoked when an attempt is made to access a predicate property in object
 an object through one of the operations property-value, (setf property-value), property-boundp,
 property-exists-p, property-makunbound, get-statements, insert-statement, or remove-statement.
 The base method for the class resource-object signals a property-missing-error with the object and
 statement, and provides the continuations ignore, which returns no values, and redefine, which redefines
 the class to include the property ans retries the operation, and use-value, which returns the value
 provided.")

  (:method ((class standard-class) (object standard-object) property-name operation &optional value)
    "The base method signals a property-missing-error."
    (rdf:property-missing-error :object object :predicate property-name :operation operation
                                :value value)))


(defgeneric rdf:property-read-only (class object property-name operation value)
  (:documentation "Invoked when an attempt is made to modify a predicate property which is
 specified as read-only.")

  (:method ((class standard-class) (object standard-object) property-name operation value)
    (rdf:property-read-only-error :object object
                                  :predicate property-name :operation operation :value value)))


(defgeneric rdf:query (repository &key subject predicate object context continuation offset limit)
  (:documentation "Perform a query against the REPOSITORY. Permit  SUBJECT PREDICATE OBJECT and CONTEXT
 constraints.

 REPOSITORY : repository-mediator : the RDF repository mediator
 SUBJECT, PREDICATE, OBJECT, GRAPH : t : a value in the model domain
 CONTINUATION : (or function null) : an optional function of one argument, a statement in the store domain.
 VALUE : list : if no continuation is supplied, a list of the selected statement in the store domain.

 The given statement constituents are transformed from the model domain to the respective repositor's literal
 and resource domains and formulated as a query in the rpository's terms, whereby the NIL value denotes a
 wildcard. If no argument is provided for CONTEXT, the repository's default context applies.
 If a continuation is supplied it is applied in turn to each result statement. Otherwise a statement list is
 returned. The possible constraints depend on the initial argument type.

 - When applied to a repository-mediator the constraints can be
   - an object : which delegates to its URI
   - an URI, which is interpreted as the subject uri
 - When applied to a resource-object the constraint can be
   - an URI, which is interpreted as a predicate
   - nil, which is a wildcard
   - an object or a literal, which is interpreted as a constraint on the object.

 to do: pattern, sparql, and other query forms."))


(defgeneric rdf:read-properties (object)
  (:documentation "Given an hollow object, or a modified persistent object retrieve the first-order
 predicated properties and bind them as per the class definition. Absent properties remain unbound, while
 additional properties signal a continuable predicate-missing error. Arity is managed as per the slot
 definition. Should multiple values appear for an atomic slot, that causes a continuable error.
 If the object is modified-persistent, discard any modifications. Any other intiial state signals an error."))


(defgeneric rdf:repository-clear (repository)
  (:documentation "Remove all statements from the repository."))


(defgeneric rdf:repository-close (repository)
  (:documentation "Close the repository and release any instance-specific resources."))


(defgeneric rdf:repository-count (repository)
  (:documentation "Return the count of statements in the repository."))


(defgeneric rdf:repository-indelible? (repository)
  (:documentation "Return true iff the repository is is write-only.
 The abstract class binds a class slot to nil. A concrete repository class may shadow this."))


(defgeneric rdf:repository-empty? (repository)
  (:documentation "Return true iff the repository contains no statement. Each concrete repository class
 must implement this."))


(defgeneric rdf:repository-persistent? (repository)
  (:documentation "Return true iff the repository is persistent. The inverse of repository-transient?.
 The default method returns nil and must be specialized for each concrete repository and/or repository class."))

           
(defgeneric rdf:repository-readable? (repository)
  (:documentation "Return true iff the repository support query operations.
 The default method returns t and must be specialized for each concrete repository and/or repository class."))

           
(defgeneric rdf:repository-transient? (repository)
  (:documentation "Return true iff the repository is not persistent. The inverse of repository-persistent?.
 The default method inverts repository-persistent?."))

           
(defgeneric rdf:repository-writable? (repository)
  (:documentation "Return true iff the repository support insert and delete operations.
 The default method returns nil and must be specialized for each concrete repository and/or repository class."))

           
(defgeneric rdf:require-vocabulary (uri &key pathname)
  (:documentation "Load a vocabulary from the respective file if it has not yet been loaded.
 The file is derived from the path component of the URI, with the addition of the name 'vocabulary', and
 the type 'lisp', and rooted in the directory bound to '*uri-pathname-root*'."))


(defgeneric rdf:save-repository (repository location)
  (:documentation "Load a REPOSITORY from a give LOCATION.
 REPOSITORY : repository-mediator
 LOCATION : (or pathname uri stream)

 Save the content of the mediated data repository as N3-encoded RDF content to the given location.")

  (:method (repository (location pathname))
    (with-open-file (stream location :direction :input)
      (rdf:save-repository repository stream))))


(defgeneric rdf:repository-value (repository object)
  (:documentation "Map the given OBJECT from its representation in the model domain to its representation for
 a particular RDF data REPOSITORY.
 REPOSITORY : repository-mediator
 OBJECT : t : comprises literals (number, string) and resource-object instances or their symbol or
  uri designators.

 Each resource mediator performs the transformation required by its respective source and maintains
 a cache as appropriate to effect whatever identity is required for queries. For example, wilbur's
 interning db class identifies literals and URI, which means the mediator must just ensure that
 resource objects are reduced to their URI designators and maintain identity in the URI-to-object
 direction.  Return objects already in the repository's domain unchanged.")
  (:argument-precedence-order object repository)
  
  (:method ((source t) (value t))
    "The default method serves as the primary for :around's which sort-circuit the cache"
    value)
  (:method ((source t) (value null))
    nil)
  (:method ((map null) (value t))
    ;; terminate at the end of the cache chain
    nil)
  
  (:method  ((map hash-table) (model-value t))
    "One path of the process devolves to a search of the mediator's cached objets.
     In which case, the caches can be chained."
    (or (gethash model-value map)
        (rdf:repository-value (hash-table-parent map) model-value))))


(defgeneric rdf:subject (statement)
  (:documentation "Return the statement's subject as a URI, a node, or a literal, as per
 the source repository's representation.")
  (:method ((statement null)) nil))

(defgeneric rdf:subject-value (repository statement)
  (:documentation "Return the statement's subject as a URI or a literal in the model domain, as
 translated by the repository mediator."))


(defgeneric rdf:type-of (repository identifier)
  (:documentation "Determine the type of the designated resource.
 REPOSITORY : (or repository resource-object)
 IDENTIFIER : rdf:identifier 

 If the designated resource is already present in the model, return its instance type directly.
 Otherwise delegate to the store and retrieve its type as a model-domain identifier. Absent an assertion,
 presume the type {rdfs}Resource."))


(defgeneric unbind-property-slots (object)
  (:documentation "Unbind all property slots in the object.")
  (:method-combination progn)
  (:method progn ((object t))
    object))


(defgeneric rdf:uri (object)
  (:documentation "Return the object's URI."))


(defgeneric rdf:write-properties (object)
  (:documentation "Synchronize the instance content to the persistent repository.
 This is done by inserting new statements and deleting obsolete ones.
 If the object is new, that entails all statements, but if it is already just a projection of the source,
 only the statements which pertain to modified properties must be used."))


