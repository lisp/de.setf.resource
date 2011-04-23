;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.resource.implementation; -*-


(in-package :de.setf.resource.implementation)

(:documentation
  "This file defines the meta-classes for the `de.setf.resource` CLOS linked data library."
 
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
  "`de.setf.resource` is free software: you can redistribute it and/or modify
 it under the terms of version 3 of the GNU Lesser General Public License as published by
 the Free Software Foundation.

 `de.setf.resource` is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 See the GNU Lesser General Public License for more details.

 A copy of the GNU Lesser General Public License should be included with `de.setf.resource`, as `lgpl.txt`.
 If not, see the GNU [site](http://www.gnu.org/licenses/).")

 (description
  "`de.setf.resource` combines several precedents to layer CLOS-MOP metaclass support for RDF schemas onto 
 a Lisp triple store interface, to map typed, class-based CLOS models onto a persistent triple store,
 and project data between CLOS class extensions and RDF graphs.
 It allows class declarations to specify the correspondence between a class' attributes and RDF's predicated
 properties and uses a combination of default rules and CLOS / RDF-schema introspection provides
 internal/external identification both at the level of individual instances and at the class/type level.
 Eventual inconsistencies and adhoc variations are managed through a combination of convention and
 application-handled exceptions.

 The implementation draws on a number of precedents. On one hand the immediate Lisp-RDF interfaces
 like wilbur, swclos, and allegro-graph provide concrete statement-level access to triple stores.
 On the other, higher-level object-persistence libraries, such as the the CL-SQL, PLOB, Elephant packages
 in Lisp, and implementations such as spira in Ruby, and JDO in Java demonstrate how to use metadata to
 support object persistence.

 Each of the object persistence precedents is considered in turn, in order to provide some background
 to the implementation.

 - WOOD[6]: Neglecting proprietary implementation such as STATICE and Object-Store, of
 which the proprietary nature limits a technical evaluation, one of the earliest object-persistence
 implementation was WOOD for MCL. WOOD provided a runtime-specific mapping between Lisp data
 and a continuously updated disk projection.

 - PLOB[5] implemented metaclass-based persistence based on a ? backend by altering instantiation and
 slot accessors to map instance slots onto records on the persistent store.

 - CLSQL provided a instance-based coded similar to PLOB, but re-targeted the backend towards relational 
 databases with SQL interfaces. (backaneds?)

 BKNR Datastore[http://www.cl-user.net/asp/libs/bknr]

 CL-PREVELANCE[http://www.cl-user.net/asp/libs/cl-prevalence]  XML-based externalization.
 Suggested that the requirements of web service architectures were better served by a mechanism
 which just projected snapshots of a servers state onto a persistent image, resurrecting the WOOD
 proposition, but for a broader runtime range.
 instance/slot encoding as alternating elements.

 Elephant[3] : O-Btree - slot-value codec

 Rucksack[4] : another O-R instance-record codec, this time based on it's own persistent heap.

 The principal distinctions are in two dimensions:
 - record v/s attribute based
 - intrinsic keys

 Comparing general persistence architectures:[2]
 The RDF-based persistence architecture is analogous to that of elephant, in that the object is decimated
 into its slots. where elephant identifies each slot value with a _serialized descriptor_ which comprises
 the class, a class-relative oid, and the slot name, in an RDF repository each subject is designated with an URI,
 which is logically independent of the class, and each slot value is specified with a predicate, which
 is logically independent of the slot name.  The mop issues are not strictly relevant, since any change
 is reflected in a new vrsion.
 ! the elephant architecture description says that the oid designates the instance independent of the class facet.
 the latter is used only if necessary to instantiate - but then, there's no reason to indluce it in the slot key.
 ! the persistent slots have no in-memory, bound representation - all access is delegated to the respective storage
 controller. this means that a life-cycle like jdo - where the persistence is dynamic, cannot be implemented
 unless the conversion is effectively a change-class
 ! they also do not manage transient slots as part of a transaction. this is one thing which the jdo life-cycle does

 a persistence architecture:
 there are three aspects:
 - codecs/externalization/serialization
 - object identity / relationships / designation
 - transaction / lifecycle

 the ancillary issues are subsumed
 - caching by identity mechanisms
 - evolution by versioned identity
 - indexing is entirely handled by the repository

 don't bother with slot-value-using-class, encode the protocol in the accessors.
 proxies are not necessary if the instance itself has a full life-cycle.
 extra prodicates can either signal an error or subclass with the additional slot - a single catchall class?
 property-value-using-class is also not necessary, as the properties are prototypical, which means they vary by instance.

 each instance is named. in the spira example the instance name and the value of the 'name' attribute were similar,
 but actually unrelated. The instane uri was extrinsic. In order to support intrinsic uri, the class requires a
 compute-uri operator which can be applied to the instance to generate the uri on demand in order to name the
 new instance in the repository and applied to an initialization argument list to find an external instance in the repository.

 in which classes
 represents the direct content of a persistent relations as well as relational references
 on the basis of declared immediate data and foreign key attributes. The approach is similar to other
 object-relational mappings - such as CL-SQL, but stands apart in that the projection is expressed
 concisely in terms integrated with Ruby's object model.
 Elephant, sets a similar goal, to project CLOS class extensions onto a persistent repository, but targets
 a key-value store, rather than relational. Which means that slot values exist as isolated attributes,
 each of which is independently associated with an instance.

 Any one of the direct Lisp RDF interfaces which make data available as triples, suffices as the primitive
 access layer for this projection. Both wilbur, and allegro-store afford sufficient access to a triple store.
 In the first case an in-memory repository, in the second both immediate and  persistent repositories.

 Identification
 --------------

 The core of CLOS/RDF projection is a mechanism which correlates instances in the value and the type domains.
 In the type domain, it relates CLOS class names to resource identifiers for RDF schema classes.
 In the value domain, it relates CLOS object identity to RDF subject resource identifiers.
 By convention, URI are treated externally as _mostly_ opaque identifiers
 [http://rest.blueoxen.net/cgi-bin/wiki.pl?JeffBone][http://www.w3.org/DesignIssues/Axioms.html],
 with the exception, that a vocabulary component is designated with a fragment identifier relative to absolute
 URI which designates the vocabulary in its entirety. `de.setf.resource` follows this convention when
 managing class definitions by associating each vocabulary with a package according to the absolute URI
 for the vocabulary namespace and using symbols constructed from vocabulary terms and interned in the
 respective package as the name for classes. The convention is extended to cover instances by identifying each
 with a URI which appends an UUID to the class URI as the fragment query parameter ID. For example, for a FOAF person,
 for which the scheam class uri is

    http://xmlns.com/foaf/0.1/Person

 an instance would be identified as 
 
    http://xmlns.com/foaf/0.1/Person?UUID=1234567812345678

 while a beer ontology brewery, for which the schema class URI is

    http://www.purl.org/net/ontology/beer#Brewery

 would be

    http://www.purl.org/net/ontology/beer#Brewery?UUID=8765432187654321

 The ambiguity which arises between the query parameters and the fragment parameters could be
 avoided by adding url syntax rules specific to the identity, but it is simpler to just rely on its suffix
 position.


 generating uri
 - Java2RDF[http://www.agentgroup.unimo.it/wiki/index.php/Java2RDF_tool] class base + name with the default name
 being the result of hashcode();
 - Jenabean[http://code.google.com/p/jenabean/] (ref from Java2RDF as the more popular), requierd the URI as a class construction parameter.
 - sayers-2002 : compute the identifier from the content [http://www.hpl.hp.com/techreports/2002/HPL-2002-216.pdf]
 enforces versioning implicitly, with the additions laterVersion / earlierVersion predicate; could be generated from
 the predicate+object content without subject uri replacement.
 - UUID combination: compute an uuid from the schema url for the class' term, use that as the namespace uri for a type 3/5
 in combination with a time-based uuuid as the individual name


 Protocol
 --------

 rdf:make-instance (class &rest initargs) :
  Create a new instance. The default protocol is standard CLOS cl:standard-class instantiation protocol.
  No additional slot value processing occurs beyond distinguishing the the initial state as modified-new
  if arguments were provided.
  The new instance is transient. It must either be registered with the repository or bound to a slot in an
  already persistent instance.
  No identifier is generated until the instance is written to the repository.

 rdf:find-instance (class . designators) :
  Retrieve the designated instance from the class' persistence repository.

 rdf:find-instance-with-repository (class repository . designators) :
  Retrieve the designated instance of the given class from the given persistent repository.
  The resource can be designated with a URI, or it can be specified by associations, in which case
  the related subject URI are retrieved, to be used as resource designators. count, offset, and order-by
  parameters arrange and subset the result. an if-does-not-exist parameter determines whether a null result
  signals an error.

 rdf:make-class (designator) :
  instantiates a class. The designator can be a Lisp class designator, an RDF schema URI or symbol, or
  a set of attributes. In the first case the standard CLOS protocol applies. In the case of a schema
  class name, the designator is interned as the equivalent Lisp symbol and treated as such. In the case
  of an attribute set, the closest structural match is found and that class is used - with possible
  prototype extensions.

 rdf:project-graph (source destination)
  Project data between instance and rdf models. Permitted (source x destination) combinations are
    (resource-object repository)
    ((set resource-object) repository)
    ((set statement) (resource-class + resource-object))
    (query (repository + resource-class))

  When a resource-object is projected onto a repository, the first step is to construct a URI for it.
  The operation is delegated from the respective instance's class to its bound class-compute-uri-function.
  Two implementation are available: compute-temporal-uri and compute-content-uri.
  The first method generates a time-dependent UUID based on the system time and the network node id.
  This is unique to the granularity if a type-1 UUID.
  The second method computes a hash of the instance's state and combines that with the class' schema's
  namespace as a type-5 UUID. In order to ensure uniqueness, one facet of the state is the previous
  identity, for which the intial value is a type-1 UUID. Update conflicts are indicated by asserting a
  nextVersion in the same transaction, for which a uniqueness constraint exists on the subject URI.
  An alternative to the opaque URI form, the UUID is attached to the namespace URI for the class'
  vocabulary as a fragment parameter.

  The first forms provide the quality, that the URI is completely opaque. In the first version it can be generated
  entirely independent of the repository. The latter three forms require knowledge of the schema URI to form
  the UUID and/or URI stem.

 slot access (reader / writer ) :
  Access to instance attributes is through accessor functions. The storage is implemented as slots for
  attributes which are specified in a direct schema and/or class declaration. Other slots are bound as an
  ad-hoc prototypical property list as they appear. These include slots which are defined external to the
  primary vocabulary specification, as their presence is not guaranteed, and thus cannot be an aspect of 
  the nominal typing.

  The accessors use a method combination to wrap standard combination pattern for predicated slots.
  If the instance is new, slot modification effects a switch to modified-new state.
  If the instance is persistent, read access caues the transition hollow -> stored, which reads immediate
  triples associated with the instance as subject, interns and binds literal values, and caches URI
  values for eventual resolution. Support could be added for access attibutes and groups to delay/accellerate
  access to individual slots or slot groups.

 decode-instance (instance slots source) :
  decode the instance from the stream or tuple-set, instance can be a class.
  in order to all hollow/stored loading, the slots can restrict the retrieval/modification to a subset of
  the class' complement. the value, t, indicates all slots.
 encode-instance (instance slots destination) : send the instane's tuples to the destination
  can also be used for list-tupls
 delete-instance (instance) : removes it from the cache ? ends it's temporal limit?
 save-instance (instance) : encodes the object to the repository independent of transactions


 MOP Support
 -----------

 The CLOS MOP class instantiation protocol offers exactly one opportunity in the slot definition process to
 elect the slot definition class and none to generate the initargs. this means that all effective
 slot definitions for a given class must be of the same class and the additional declaration attributes must
 be incorporated in an :after or :around method for the compute-effective-slot-definition function.
 some implementations add operators to compute the initargs, but they are not standard.

 Literal slot values are internalized/externalized according to a data flow implemented in the rdf:internalize-value
 and rdf:externalize-value functions. Reference slot values require further retrieval according to the
 immediate URI and construction of the respective instances to comprise the related attributes. This process
 employes either the declared slot datatype, the type furnished by the repository in relation to the subject,
 of introspective analysis of available classes (see rdf:find-class).


 Persistence Mediation
 ---------------------

 Despite the RDF "open-world" paradigm, which requires a processing mechanism accommodate unforseen data, it
 is imperative that a repository mediator afford an application a stable projection of unpredicatable content.
 If a CLOS application is to rely on class and generic function definitions to behave as intended, they must
 be bound to data as it appears, `de.setf.resource` serves this goal in several ways:
 - it implements instance identity within a given mediation interface according to subject URI
 - it provides for automatic unique instance URI generation within a transactional context
 - it treats symbols, universal names, and URI as equivalent
 - it accepts resources descriptions without nominal type indications, reconciles them to the
 know class structure and admits additional prototypical attributes.

 + instance identity, indexing, and caching
 Each repository mediator adopts the respective repository's interened URI 'nodes' as unifying identifiers to ensure
 a one-to-one relation between identified objects and external resources. The URI serve as keys in an
 hash table which is used in query operations to yield identical instances for equivalent URI.
 The cache is not held weak, as the repository's URI designator-to-node cache is itself static.

 Query operations are expressed either in terms of predicate property lists or query expressions.
 Both cases are translated into a repository's native query interface without any additional internal indexing.
 The mediator only unifies resource designators with its identity cache when instantiating the resulting
 objects.

 + representing resources
 Every vocabulary defines a base uri and a syntax which permits a specific uri reference for each
 resource class defined by the vocabulary. As discussed above (see Identification), these type identifiers
 can either be augmented direct catenation with a UUID to serve as unique, structured instance identifiers,
 or they can be combined with an initial UUID to serve as an opaque identifier.

 Each repository implementation supports its own representation for URI - which would include UUID. 
 URI references can be expressed in absolute terms in all implementations as inflected strings, that is, as
 character sequences adjacent to syntax markers in additon to or instead of the double quote
 characters used in the standard i/o syntax. Wilbur preceeeds a standard string representation with the
 additional '!' marker, while allegrograph encloses he URI namestring with '<' '>' and preceeds tha sequence
 with a '!'. In addition the repository implementations support the  RDF encoding as qualified names ('Q-Names')
 by permitting definition of namespace prefix bindings and interpreting the universal name designated by
 a Q-Name as equivalent to the URI with the same namestring.

 The representation models, one the other hand are not compatible, and, in the case of allegrograph, the
 model is not closed. That is a process cannot expect a URI decoded by wilbur to be recognized as such by
 allegrograph and even allegrograph itself is not consistent. For example, for wilbur,
    ? (eq !\"http://www.w3.org/1999/02/22-rdf-syntax-ns#type\" !rdf:type)
    t
 while for allegrograph suggests a similar equivalence exists here between uri and qualified names,
 but with the restriction that they are not actually eq:
    TRIPLE-STORE-USER(31): (eq !<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> !rdf:type)
    NIL
    TRIPLE-STORE-USER(32): (eq (triple-store:resource \"http://www.w3.org/1999/02/22-rdf-syntax-ns#type\") !rdf:type)
    NIL
    TRIPLE-STORE-USER(33): (part= (triple-store:resource \"http://www.w3.org/1999/02/22-rdf-syntax-ns#type\") !rdf:type)
    t

 The representation for resources also differs markedly between implementations. 
 allegrograph represents _triples_ as an opaque byte sequence with five aspects:
 subject, predicate, object, graph, and triple id - almost as if the object is a displaced
 subsequence of the i/o buffers. Of these, URI, literals, and blank nodes are each reduced
 to a twelve-byte _part identifier_.
 Wilbur, on the other hand represents quads as sructures which comprise subject, predicate, object, and
 graph.

 The mediator implementation reconcile these differences by supporting symbols as the
 universal URI representation and providing standard operatro interfaces in terms of resource-object
 classes, instances, and propeties.
 ---
 [1] : http://github.com/datagraph/spira
 [2] : http://common-lisp.net/pipermail/elephant-devel/2006-June/000446.html
 [3] : http://common-lisp.net/project/elephant/documentation.html
 [4] : http://weitz.de/eclm2006/rucksack-eclm2006.txt
 [5] : http://plob.sourceforge.net/
 [6] : http://www-cgi.cs.cmu.edu/afs/cs/project/ai-repository/ai/lang/lisp/code/ext/wood/0.html

 dc foaf doap skos sioc rdf rdfs owl 'rss 1.0' cc
 
 ---
"))


;;; !!! change it to use two allocations forms
;;;  :resource for archetypal resource slots
;;;  :property for dynamic slots
;;; in this case add a prototype to the slot definitions

(defclass abstract-resource-class (standard-class)
  ((direct-repository
    :initform nil
    :reader class-direct-repository)
   (direct-vocabulary
    :initform nil
    :reader class-direct-vocabulary)
   (datatype
    :reader get-class-datatype :writer setf-class-datatype
    :documentation "Identifies the class in the respective vocabulary. By default, identical with the class name.")
   (compute-uri-function
    :reader get-class-compute-uri-function
    :documentation "A function of two arguments, the class and an instance, which computes a URI for the
     instance on-demand in the context of the class' repository.")
   (property-missing-function
    :reader get-class-property-missing-function
    :documentation "A function of two arguments, the class and the property name, which is invoked should
     an attempt be made to read a property which an instance does not own."))

  (:documentation "An abstract-resource-class embodies the relation between a collection of clos instances and
    a stored rdf graph. The class sets the context in terms of a default repository and a vocabulary,
    and permits the instance to specify an individual repository. Instances comprise five kinds of slots

    - transient : standard clos slots with accessors and no projection into the external repository
    - persistent-pair
      - statement : caches the respective statement
      - literal : numbers, strings, and other atomic values
      - resource : resource-object instances constructed from uri statement object values
    - properties : either literal or resource-object values which corresponde to statement not
      reflected in the class definition

    the persistent status is indicated by the presence of a :datatype in the slot declaration and/or
    a slot name which is a property in a declared rdf vocabulary. This information is recorded in the
    class direct slot definitions and used to generate slot-accessors which manage the uri/resource
    duality and the instance's persistence state.
    The abstract-resource-class serves as a part of the modeling infrastructure, for example, the
    resource-object class, but is never instantiated. Concrete classes use the resource-class metaclass
    instead, in order to cause inheritance among the values specified for repository, datatype, and vocabulary,
    which are initially bound to a 'direct-' slot and resolved after finalization to yied the concrete
    slot values."))


(defclass resource-class (abstract-resource-class)
  ((repository
    :reader get-class-repository
    :documentation "The default repository for persistent instances of this individual class.
     An initialized class may bind value to this slot. If not, it is determined on-demand as the most-specific
     declared repository in the class' precedence list.  The base class resource-object specifies t as the base
     value.")
   (vocabulary
    :reader get-class-vocabulary
    :documentation "Attaches the vocabulary which includes this class, the namespace/package, and the
     related class and datatype definitions. By default the vocabulary which comprises the datatype.")
   (indelible
    :initarg :indelible :initform nil :allocation :class
    :reader rdf:repository-indelible?
    :documentation "Analogous to repository-indelible? for mediators."))

  (:documentation "The resource-class is the concrete metaclass for instantiated resource classes.
    It adds the slots for concrete values for repository, datatype, and vocabulary."))

(setf (find-class '{rdfs}Class) (find-class 'resource-class))
(setf (find-class '{owl}Class) (find-class 'resource-class))

(defmethod c2mop:validate-superclass ((c1 standard-class) (c2 resource-class)) t)
(defmethod c2mop:validate-superclass ((c2 resource-class) (c1 standard-class)) t)
(defmethod c2mop:validate-superclass ((c1 standard-class) (c2 abstract-resource-class)) t)
(defmethod c2mop:validate-superclass ((c2 abstract-resource-class) (c1 standard-class)) t)


(:documentation rdf-relation-definition
  "The essence of RDF is names relations between entities. The relation is asymmetrical, in that
 one entity, the 'subject', must be an identified resource, while the other can be either a resource
 of an anonymous literal. The relation predicate is always a resource. This library represents the
 resources as CLOS instances and designates them in RDF statements with symbols and UUID instances.
 The literals are represted with standard lisp data objects.

 A resource and its related entities, is defined with a CLOS class. The archetypal class' structure
 is captured in direct slot definitions which add several attributes to the standard CLOS complement:
 - predicate
 - datatype
 - element-datatype
 - reader
 - writer
 - encoder
 - decoder
 These slots are distinguished by the presence of datatype and/or predicate arguments in the slot
 declaration. Default values are computed for the other attributes is none is declared.

 The hierarchy distinguishes the archetypal from the prototypal properties, and the literal from the
 resource properties. A mirror slot is provided for the archetypal slot to describe respective slots
 to repository statement instances.

 - rdf-relation-definition
   - rdf-direct-relation-definition
     - rdf:archetypal-property-definition
       ;; - rdf-archetypal-literal-property-definition
       ;; - rdf-archetypal-resource-property-definition
     - rdf:prototypal-property-definition
   - rdf-effective-property-definition
   - rdf-statement-slot-definition

 The terms resource, property, literal, and reference and the respective definitions are taken from
 the various recommendations[1,2,3] The earliest of which makes an explict distinction between resources
 and literals, but the later ones fail to maintain it, and express it only indirectly. For example in the
 rdf schema explanatory figures 1 and 2, the rdfs:Literal _class_ is indicated to be of 'type' rdfs:Resource,
 but there are no examples which explain that an _instance_ the literal class is _not_ of _type_
 rdfs:Resource.

 The names for these property definition classes are chosen consistent with the 1999 terms, as they
 are the most explicit. The most general description is of a `relation`. Of those, the primary
 distinction is between a direct definitions - those which represent the declaration for a
 specific class or instance, and an effective definition - those which combine all definitions
 from precedent classes into the complete set for a concrete class. The access protocol is
 governed by the direct definitions, which comprise the predicate and datatype specification.
 Of the direct definitions, the next distinction is between the archetypal definitions, which describe
 slots which are specified in a class definition to bind data objects, and the prototypal definitions,
 which are added to individual instances to bind data in addition to the respective class definition.
 Finally, of the archetypal property definitions, a distinction is made between literal and resource
 data. In addition a statement slot is associated with each archetypal property to bind the
 applied statement(s).

 The declared slot type distinguishes elementary resources from resource sets. Where the type cons or
 one of its structural subtypes appears, eg. (cons {foaf}Person), the collection relation is modeled
 as a list.

 Internal prototypal properties are managed with a specialized rdf:prototypal-property-definition
 class in order to recognize them when unbinding values.

 The distinction between literal and resource properties can be specified in the declarations in terms
 of encoding and decoding operators. Absent declarations the distinction is made based on the type
 of the declared datatype with recourse to the respective repository, should the type be unknown. The
 determination is delayed until the time of the first actual slot reference in order to permit
 circular type references.

 ---
 [1]: http://www.w3.org/TR/1999/REC-rdf-syntax-19990222/#model
 [2]: http://www.w3.org/TR/2004/REC-rdf-syntax-grammar-20040210/
 [3]: http://www.w3.org/TR/2000/CR-rdf-schema-20000327/#s2.2 ")


(defclass rdf-relation-definition (c2mop:metaobject)
  ((predicate
    :initform (error "predicate required") :initarg :predicate
    :reader slot-definition-predicate
    :type rdf:identifier
    :documentation "The predicate which specifies the relation between the subject instance
     and the slot value object. This is represented internally as a symbol and
     encoded as an URI.")
   (datatype
    :initform t :initarg :datatype
    :reader slot-definition-datatype
    :type rdf:identifier
    :documentation "The RDF datatype of the atomic object value. This is represented internally
     as a symbol and encoded as an URI. It does not distinguish scalar from set values."))
  (:documentation "The rdf-relation-definition class mixes the predicate and datatype
 specification into concrete property definitions."))


(defclass accessor-slot-definition (c2mop:standard-direct-slot-definition)
  ((reader
    :initarg :reader
    :reader slot-definition-reader :writer setf-slot-definition-reader
    :documentation "The primary slot reader")
   (writer
    :initarg :writer
    :reader slot-definition-writer :writer setf-slot-definition-writer
    :documentation "The primary slot writer")
   (encoder
    :initarg :encoder
    :reader get-slot-definition-encoder
    :documentation "A function of three arguments, the instance, the slot definition, and the
     model value. If no value is declared, the value is computed based on the declared datatype.
     If none can be determined, the decoding is determined introspectively")
   (decoder
    :initarg :decoder
    :reader get-slot-definition-decoder
    :documentation "A function of three arguments, the instance, the slot definition, and the
     repository value. If no value is declared, the value is computed based on the declared datatype.
     If none can be determined, the decoding is determined introspectively"))
  (:documentation "The accessor-slot-definition class mixes in slots for a reader and a writer operator
 into abstract standard-slot-definition class.
 These contribute to rdf relation definitions to define the accessors to use when generating accessor
 operators and when cross-referenceing between statement and value slots."))

(defclass rdf-direct-relation-definition (rdf-relation-definition accessor-slot-definition)
  ())

(defclass rdf-statement-slot-definition (accessor-slot-definition)
  ((property-slot
    :initarg :property-slot
    :reader slot-definition-property-slot :writer setf-slot-definition-property-slot))
  (:documentation "The rdf-statement-slot-definition describes the shadow slots to archetypal property
 definitions, which maintain the respective rdf statement instances. For each property a statement slot
 is paired with an archetypal property slot."))

(defclass rdf:archetypal-property-definition (rdf-direct-relation-definition)
  ((statement-slot
    :initarg :statement-slot
    :reader slot-definition-statement-slot :writer setf-slot-definition-statement-slot)
   (list-type-p
    :initform nil :initarg :list-type-p
    :reader slot-definition-list-type-p))
  (:documentation "Describes persistent property slots.  Each is paired with a statement slot which
 caches the statement and must arrange to synchronize that with any instance value."))

#+(or)                                  ; no longer distinguished. allow the object type to decide
(defclass rdf-archetypal-literal-property-definition (rdf:archetypal-property-definition)
  ()
  (:documentation "Describes archetypal property slots which bind literal values."))

#+(or)
(defclass rdf-archetypal-resource-property-definition (rdf:archetypal-property-definition)
  ()
  (:documentation "Describes archetypal property slots which bind resource values."))



(defclass rdf:prototypal-property-definition (rdf-relation-definition)
  ((name
    :initarg :name :reader c2mop:slot-definition-name)
   (type
    :initarg :type :initform nil
    :reader c2mop:slot-definition-type)
   (statement
    :initform nil :initarg :statement
    :accessor slot-definition-statement)
   (value
    :initarg :value
    :accessor slot-definition-value)
   (permission
    :initarg :permission :initform t
    :type (member t :read-only)
    :accessor slot-definition-permission))
  (:documentation "The rdf:prototypal-property-definition class combines the RDF predicate and datatype
 information with standard slot attributes, and augments them with a values facet in order to represent
 prototypal properties in addition to archetypal definitions which follow from the class definition."))

(defclass rdf-internal-property-definition (rdf:prototypal-property-definition)
  ())

(defclass rdf-effective-property-definition (c2mop:standard-effective-slot-definition)
  ()
  (:documentation "The rdf-effective-property-definition class marks the effective slot definitions
 which derive from rdf archetypal properties, but adds no additional information. The effective
 specifications are those of the most specific archetypal definition, which are then incorporated into
 generated accessor operators."))


(def-class-constructor rdf:prototypal-property-definition
  (:method ((class resource-class) &rest initargs)
    "Instantiate a prototypal property for the specific combination of CLASS and INITARGS.
     This permits each class to extend the prototype property access protocols by specialzing the
     property instance, for example, to use the specified predicate to provide default type information
     and arity."
    (declare (dynamic-extent initargs))
    (apply #'prototypal-property-definition
           (apply #'prototypal-property-definition-class class initargs)
           initargs)))

(defgeneric prototypal-property-definition-class (context &rest initargs)
  (:method ((class resource-class) &rest initargs)
    (declare (dynamic-extent initargs) (ignore initargs))
    'rdf:prototypal-property-definition))

(def-class-constructor rdf:archetypal-property-definition )

(defun rdf-internal-property-definition (&rest initargs)
  (declare (dynamic-extent initargs))
  (apply #'make-instance 'rdf-internal-property-definition initargs))

(defmethod print-object ((object rdf:prototypal-property-definition) (stream t))
  (print-unreadable-object (object stream :identity t :type t)
    (format stream "~@[~a~]" (when (slot-boundp object 'name) (c2mop:slot-definition-name object)))))

;;;
;;; manage class bindings which are inherited from super-classes


(defmethod setf-class-repository (repository (class-name symbol))
  (setf-class-repository repository (find-class class-name)))

(defmethod setf-class-vocabulary (vocabulary (class-name symbol))
  (setf-class-vocabulary vocabulary (find-class class-name)))

(defmethod setf-compute-uri-function (function (class-name symbol))
  (setf-compute-uri-function function (find-class class-name)))

(defmethod setf-class-property-missing-function (function (class-name symbol))
  (setf-class-property-missing-function function (find-class class-name)))



;;; resource-class initialization protocol:
;;; - MOP portability proscribes additions to shared-initialize;
;;; - in some implementations finalize-inheritance may be run once only,
;;; - compute-slots is run at least in finalize-inheritance when precedence changes and thereby the slot set,
;;;   perhaps more often, perhaps from shared initialize as well.
;;; the extensions must fit in initialize-instance and reinitialize-instance and prepare any required slots
;;; before shared initialize, but not depend on them until compute-slots. 

(defmethod canonicalize-initarg ((class abstract-resource-class) (arg (eql :repository)) (value cons))
  value)

(defmethod canonicalize-initarg ((class abstract-resource-class) (arg (eql :datatype)) (value symbol))
  value)

(defmethod canonicalize-initarg ((class abstract-resource-class) (arg (eql :vocabulary)) (value vocabulary))
  value)

(defmethod canonicalize-initarg ((class abstract-resource-class) (arg (eql :vocabulary)) (value string))
  value)

(defmethod canonicalize-initarg ((class abstract-resource-class) (arg (eql :vocabulary)) (value symbol))
  (nth-value 0 (uri-vocabulary-components value)))

(defmethod canonicalize-initarg ((class abstract-resource-class) (arg (eql :compute-uri-function)) (value symbol))
  (fdefinition value))

(defmethod canonicalize-initarg ((class abstract-resource-class) (arg (eql :property-missing-function)) (value symbol))
  (fdefinition value))

(defmethod canonicalize-initarg ((class abstract-resource-class) (arg t) (value cons))
  (canonicalize-initarg class arg (first value)))


(defmethod initialize-instance ((class abstract-resource-class) &rest initargs &key direct-slots
                                repository datatype vocabulary
                                compute-uri-function property-missing-function)
  (declare (ignore repository datatype vocabulary compute-uri-function property-missing-function))
  (apply #'call-next-method class
         :direct-slots (compute-extended-slot-specifications class direct-slots)
         initargs)
  ;; must be after initialization in order the (setf (find-class ...))) work: clozure requires the name
  (apply #'initialize-slots class initargs)
  (bind-slot-cross-references class)
  (ensure-resource-accessors class))

(defmethod reinitialize-instance ((class abstract-resource-class) &rest initargs &key direct-slots
                                  repository datatype vocabulary
                                  compute-uri-function property-missing-function)
  (declare (ignore repository datatype vocabulary compute-uri-function property-missing-function))
  (apply #'call-next-method class
         :direct-slots (compute-extended-slot-specifications class direct-slots)
         initargs)
  (apply #'initialize-slots class initargs)
  (bind-slot-cross-references class)
  (ensure-resource-accessors class))


(defmethod initialize-slots ((class abstract-resource-class) &key
                             (name (class-name class)) repository datatype vocabulary
                             compute-uri-function property-missing-function
                             &allow-other-keys)
  (flet ((reinitialize-slot (name value &optional set-p)
           (slot-makunbound class name)
           (when (or value set-p) (setf (slot-value class name) value))))
    (reinitialize-slot 'direct-repository
                       (when repository (canonicalize-initarg class :repository repository))
                       t)
    (reinitialize-slot 'direct-vocabulary
                       (when vocabulary
                         (canonicalize-initarg class :vocabulary vocabulary))
                       t)
    (reinitialize-slot 'datatype
                       (when datatype
                         (setf datatype (canonicalize-initarg class :datatype datatype))))
    (when (and datatype (not (eq datatype name)))
      (let ((old-class (find-class datatype nil)))
        (when (and old-class (not (eq old-class class)))
          (warn "Redefining datatype: ~s: from ~s to ~s"
                datatype old-class class))
        (setf (find-class datatype) class)))
    (reinitialize-slot 'compute-uri-function
                       (when compute-uri-function
                         (canonicalize-initarg class :compute-uri-function compute-uri-function)))
    (reinitialize-slot 'property-missing-function
                       (when property-missing-function
                         (canonicalize-initarg class :property-missing-function property-missing-function)))))


(defgeneric compute-extended-slot-specifications (class direct-slots)
  (:documentation "Given a resource-class metaobject and a slot declaration list, ensure that, for
 each slot which models a predicate relation, add a shadow slot to bind the raw rdf value.")

  (:method ((class abstract-resource-class) direct-slots)
    (let* ((property-slots ()))
      (flet ((complete-property-slot-spec (slot-spec)
               (destructuring-bind (&key name (type t) predicate
                                         (datatype (when predicate (if (eq type t) '{rdfs}Literal type))
                                                   datatype-s)
                                         &allow-other-keys)
                                   slot-spec
                 ;; use the datatype as the indication that it is an rdf slot
                 ;; don't constrain the vocabulary to allow arbitrary cross-schema definitions
                 (when datatype
                   (unless predicate (setf (getf slot-spec :predicate) name))
                   (setf (getf slot-spec :list-type-p) (list-type-p type))
                   (setf (getf slot-spec :statement-slot) (cons-symbol (symbol-package name) name :.statement.))
                   (if datatype-s
                     (assert datatype () "Invalid datatype: ~s." datatype)
                     (setf (getf slot-spec :datatype) datatype))
                   (push slot-spec property-slots)))
               slot-spec)
             (compute-statement-slot-spec (slot-spec)
               (destructuring-bind (&key name statement-slot &allow-other-keys) slot-spec
                 (let* ((reader (cons-symbol nil name :-statement))
                        (writer (cons-symbol nil :setf- name :-statement)))
                   `(:name ,statement-slot :property-slot ,name
                           ;; :type rdf:assertion
                           :readers ,(list reader) :writers ,(list writer))))))
        (append (mapcar #'complete-property-slot-spec direct-slots)
                (mapcar #'compute-statement-slot-spec (nreverse property-slots)))))))


(defmethod bind-slot-cross-references ((class abstract-resource-class))
  "Iterate over the direct slots, bind the uri and resource slot cross-references."
  (let ((direct-slots (c2mop:class-direct-slots class)))
    (flet ((ensure-slot (designator)
             (etypecase designator
               (symbol (or (find designator direct-slots :key #'c2mop:slot-definition-name)
                           (error "Invalid slot cross-reference: ~s." designator)))
               (rdf-relation-definition designator))))
      (dolist (sd direct-slots)
        (typecase sd
          (rdf-statement-slot-definition
           (setf-slot-definition-property-slot (ensure-slot (slot-definition-property-slot sd)) sd))
          (rdf:archetypal-property-definition
           (setf-slot-definition-statement-slot (ensure-slot (slot-definition-statement-slot sd)) sd)))))))


(defmethod c2mop:direct-slot-definition-class ((class resource-class)
                                               &key property-slot statement-slot readers writers type name)
  "If there is a cross-reference to property definition, it's a statement slot. Otherwise if there is a
 predicate or a datatype, then examine the type specification. If that is a resource-object spcialization,
 then 
 package is a registered namespace, or a predicate or datatype is supplied, then the
 slot is persistent. Otherwise it is treated as transient."
  (cond (property-slot
         (find-class 'rdf-statement-slot-definition))
        (statement-slot
         ;; predefine the accessor function in order to set the class
         ;; otherwise sbcl inhibits the change
         (dolist (reader readers)
           #+ccl
           (ensure-generic-function reader
                                    :method-combination (c2mop:find-method-combination #'ensure-resource-accessors
                                                                                       'rdf:persistent-slot-reader
                                                                                       (list :type type :name name)))
           #-ccl
           (ensure-generic-function reader
                                    :method-combination `(persistent-slot-reader :type ,type :name ,name)
                                    :generic-function-class 'rdf-slot-reader))
         (dolist (writer writers)
           ;; (print writer)
           #+ccl
           (ensure-generic-function writer
                                    :method-combination (c2mop:find-method-combination #'ensure-resource-accessors
                                                                                       'rdf:persistent-slot-writer
                                                                                       (list :type type :name name)))
           #-ccl
           (ensure-generic-function writer :method-combination `(persistent-slot-writer :type ,type :name ,name)
                                    :generic-function-class 'rdf-slot-writer))
         (find-class 'rdf:archetypal-property-definition))
        (t
         (call-next-method))))


(defmethod c2mop:effective-slot-definition-class ((class resource-class) &key name)
  "Specify always an rdf slot definition. It serves just as a marker, but adds no behaviour as the direct
 slot definitions incorporate the declarations."
  (if (some #'(lambda (class) (typep (find name (c2mop:class-direct-slots class)
                                           :key #'c2mop:slot-definition-name)
                                     'rdf-relation-definition))
            (c2mop:class-precedence-list class))
    (find-class 'rdf-effective-property-definition)
    ;;(find-class 'c2mop:standard-effective-slot-definition)
    (call-next-method)))


(defmethod c2mop:compute-effective-slot-definition ((class resource-class) (name t) (direct-slot-definitions t))
  "Serves as a hook in case it is necessary to extend the slot definition. Delegate to the default
 constructors for the instance. Should it be necessary, the standard MOP leaves no other opportunity
 for extension as no means is specified to augment the initargs and/or make the class contingent on them."
  (call-next-method))


;;; inherited slot accessors

(:documentation  class-repository class-datatype class-vocabulary
  "Various class attributes are specified alternatively
 - in the class definition proper or via a superclass
 - as literal argument values or as designators to be resolved upon first reference.
 The first variation is handled by accessors which check if the slot is bound. If not they search the inheritance
 chain for an initial value. The second variation is handled by splitting the slot between the active slot and
 the direct slot. The active slot value is computed by resolving whatever value is inherited over direct slots.

 The resolution is delayed until first reference in order to permit values to depend on the existence of a
 mediator operate on the repository. (see class-vocabulary)")


;;; repository and vocabulary slots values are inherited and resolved

(defgeneric class-direct-repository (class)
  (:documentation "Augment the slot reader with alternative methods for designators and for other
    classes in the precedence list.")
  (:method ((class class))
    nil)
  (:method ((class symbol))
    (class-direct-repository (find-class class))))

(defgeneric class-direct-vocabulary (class)
  (:documentation "Augment the slot reader with alternative methods for designators and for other
    classes in the precedence list.")
  (:method ((class class))
    nil)
  (:method ((class symbol))
    (class-direct-vocabulary (find-class class))))


(defgeneric class-repository (class)
  (:method ((class symbol))
    (class-repository (find-class class)))
  (:method ((class class))
    nil)
  (:method ((class abstract-resource-class))
    "An abstract class returns the direct value only, without inheriting"
    (class-direct-repository class))
  (:method ((class resource-class))
    (if (slot-boundp class 'repository)
      (get-class-repository class)
      (setf (slot-value class 'repository)
            (repository-mediator (or (class-direct-repository class)
                                     (some #'class-repository (c2mop:class-direct-superclasses class))
                                     (slot-unbound (class-of class) class 'repository)))))))

(defgeneric class-vocabulary (class)
  (:method ((class symbol))
    (class-vocabulary (find-class class)))
  (:method ((class class))
    nil)
  (:method ((class resource-class))
    (if (slot-boundp class 'vocabulary)
      (get-class-vocabulary class)
      (setf (slot-value class 'vocabulary)
            (ensure-vocabulary (class-repository class)
                               (or (some #'class-direct-vocabulary (c2mop:class-precedence-list class))
                                   (symbol-uri-namestring (class-datatype class))))))))


;;; datatype compute-uri-function and property-missing are simply inherited

(defgeneric class-datatype (class)
  (:method ((class class))
    nil)
  (:method ((class symbol))
    (class-datatype (find-class class)))
  (:method ((class resource-class))
    (if (slot-boundp class 'datatype)
      (get-class-datatype class)
      (setf-class-datatype (or (some #'class-datatype (rest (c2mop:class-precedence-list class)))
                               (class-name class))
                           class))))

(defgeneric class-compute-uri-function (class)
  (:method ((class class))
    nil)
  (:method ((class symbol))
    (class-compute-uri-function (find-class class)))
  (:method ((class abstract-resource-class))
    (if (slot-boundp class 'compute-uri-function)
      (get-class-compute-uri-function class)
      (setf (slot-value class 'compute-uri-function)
            (or (some #'class-compute-uri-function (rest (c2mop:class-precedence-list class)))
                (slot-unbound (class-of class) class 'compute-uri-function))))))

(defgeneric class-property-missing-function (class)
  (:method ((class class))
    nil)
  (:method ((class symbol))
    (class-property-missing-function (find-class class)))
  (:method ((class abstract-resource-class))
    (if (slot-boundp class 'property-missing-function)
      (get-class-property-missing-function class)
      (setf (slot-value class 'property-missing-function)
            (or (some #'class-property-missing-function (rest (c2mop:class-precedence-list class)))
                (slot-unbound (class-of class) class 'property-missing-function))))))


;;; property accessor generation

(defmethod ensure-resource-accessors ((class abstract-resource-class))
  "Given the direct slot definitions, ensure that the accessor functions have the correct method combination,
 define the methods for the class' direct slots to unbind all slots, to map property slots and to map property-slot-values.
 These are in addition to those of superclasses and to the base method which applies to any properties."

  (let ((property-slots ()))
    (dolist (sd (c2mop:class-direct-slots class))
      (typecase sd
        (rdf:archetypal-property-definition
         (push sd property-slots)
         ;; augment the combination for resource slots with access to the statement slot
         ;; (inspect sd) (print sd)
         #+(or)                         ; breaks sbcl - must do this earlier
         (dolist (reader (c2mop:slot-definition-readers sd))
           #+ccl
           (ensure-generic-function reader
                                    :method-combination (c2mop:find-method-combination #'ensure-resource-accessors
                                                                                       'rdf:persistent-slot-reader
                                                                                       (list :slot-definition sd)))
           #-ccl
           (ensure-generic-function reader
                                    :method-combination `(persistent-slot-reader :slot-definition ,sd)
                                    :generic-function-class 'rdf-slot-reader))
         #+(or)
         (dolist (writer (c2mop:slot-definition-writers sd))
           ;; (print writer)
           #+ccl
           (ensure-generic-function writer
                                    :method-combination (c2mop:find-method-combination #'ensure-resource-accessors
                                                                                       'rdf:persistent-slot-writer
                                                                                       (list :slot-definition sd)))
           #-ccl
           (ensure-generic-function writer :method-combination `(persistent-slot-writer :slot-definition ,sd)
                                    :generic-function-class 'rdf-slot-writer)))))
    #+digitool
    (when property-slots
      (let* ((method-class (c2mop:generic-function-method-class #'unbind-property-slots))
             (function (compile nil `(lambda (subject)
                                       ,@(mapcar #'(lambda (sd)
                                                     `(progn
                                                        (slot-makunbound subject ',(c2mop:slot-definition-name sd))
                                                        (slot-makunbound subject ',(c2mop:slot-definition-name
                                                                                    (slot-definition-statement-slot sd)))))
                                                 property-slots))))
             (method (make-instance method-class
                       :function function
                       :qualifiers '(progn)
                       :specializers (list class)
                       :lambda-list '(object)
                       #+ccl :name #+ccl 'unbind-property-slots)))
        (add-method #'unbind-property-slots method))
      (let* ((method-class (c2mop:generic-function-method-class #'rdf:map-property-slots))
             (function (compile nil `(lambda (function subject)
                                       (declare (ignore subject))
                                       ,@(mapcar #'(lambda (sd) `(funcall function ,sd))
                                                 property-slots))))
             (method (make-instance method-class
                       :function function
                       :qualifiers '(progn)
                       :specializers (list (find-class 't) class)
                       :lambda-list '(function object)
                       #+ccl :name #+ccl 'rdf:map-property-slots)))
        (add-method #'rdf:map-property-slots method))
      (let* ((method-class (c2mop:generic-function-method-class #'rdf:map-property-values))
             (function (compile nil `(lambda (function subject)
                                       ,@(mapcar #'(lambda (sd)
                                                     `(when (slot-boundp subject ',(c2mop:slot-definition-name sd))
                                                        (rdf:map-collection function (,(slot-definition-reader sd) subject))))
                                                 property-slots))))
             (method (make-instance method-class
                       :function function
                       :qualifiers '(progn)
                       :specializers (list (find-class 't) class)
                       :lambda-list '(function object)
                       #+ccl :name #+ccl 'rdf:map-property-values)))
        (add-method #'rdf:map-property-values method)))
    #-digitool                          ; should be portable
    (when property-slots
      (c2mop:ensure-method #'unbind-property-slots
                           `(lambda (subject)
                              ,@(mapcar #'(lambda (sd)
                                            `(progn
                                               (slot-makunbound subject ',(c2mop:slot-definition-name sd))
                                               (slot-makunbound subject ',(c2mop:slot-definition-name
                                                                           (slot-definition-statement-slot sd)))))
                                        property-slots))
                           :qualifiers '(progn)
                           :lambda-list '(object)
                           :specializers (list class))
      (c2mop:ensure-method #'rdf:map-property-slots
                           `(lambda (function subject)
                                       (declare (ignore subject))
                                       ,@(mapcar #'(lambda (sd) `(funcall function ,sd))
                                                 property-slots))
                           :qualifiers '(progn)
                           :lambda-list '(function object)
                           :specializers (list (find-class 't) class))
      (c2mop:ensure-method #'rdf:map-property-values
                           `(lambda (function subject)
                                       ,@(mapcar #'(lambda (sd)
                                                     `(when (slot-boundp subject ',(c2mop:slot-definition-name sd))
                                                        (rdf:map-collection function (,(slot-definition-reader sd) subject))))
                                                 property-slots))
                           :qualifiers '(progn)
                           :lambda-list '(function object)
                           :specializers (list (find-class 't) class)))))




(defmethod  compute-object-uri-with-class ((class resource-class) (object t))
  (funcall (class-compute-uri-function class) object))


(defgeneric find-archetypal-property-definition-by-predicate (class predicate)
  (:method ((class symbol) (predicate t))
    (find-archetypal-property-definition-by-predicate (find-class class) predicate))

  (:method ((class resource-class) (predicate t))
    (flet ((find-sd (class)
             (find predicate (c2mop:class-direct-slots class) :key #'slot-definition-predicate)))
      (declare (dynamic-extent #'find-sd))
      (some #'find-sd (c2mop:class-precedence-list class)))))


(defgeneric find-archetypal-property-definition-by-name (class name)
  (:method ((class symbol) (name t))
    (find-archetypal-property-definition-by-name (find-class class) name))

  (:method ((class resource-class) (name t))
    (flet ((find-sd (class)
             (find name (c2mop:class-direct-slots class) :key #'c2mop:slot-definition-name)))
      (declare (dynamic-extent #'find-sd))
      (some #'find-sd (c2mop:class-precedence-list class)))))


(defmethod slot-definition-writable ((definition rdf:prototypal-property-definition))
  (not (eq :read-only (slot-definition-permission definition))))

(defgeneric slot-definition-predicate (definition)
  (:method ((sd c2mop:direct-slot-definition)) nil))

(defgeneric slot-definition-datatype (definition)
  (:method ((sd c2mop:direct-slot-definition)) nil))


(defgeneric slot-definition-statement-slot (definition)
  (:method ((sd c2mop:direct-slot-definition))
    nil))


(defgeneric slot-definition-base-type (definition)
  (:method ((sd c2mop:direct-slot-definition)) nil))


(defgeneric slot-definition-sequence-p (definition)
  (:method ((sd c2mop:direct-slot-definition)) nil))


(defmethod initialize-instance :after ((instance accessor-slot-definition) &key
                                       (reader nil reader-s)
                                       (writer nil writer-s))
  "cache a primary reader and writer."
  (declare (ignore reader writer))
  (unless reader-s
    (setf-slot-definition-reader (first (c2mop:slot-definition-readers instance)) instance))
  (unless writer-s
    (setf-slot-definition-writer (first (c2mop:slot-definition-writers instance)) instance)))

(defmethod initialize-instance ((instance rdf:prototypal-property-definition) &rest initargs
                                &key name (predicate name))
  (apply #'call-next-method instance
         :predicate predicate
         initargs))

(:documentation
  "An archetypal model can be exteneded in two ways: declarative and ad-hoc.
 - Declarative concerns the  association between defined operators and instance properties. In this case, the
 function class prepares the operator to be self-extending to comprehend given classes. The generated
 operators delegate to the property-value opertors to effect the access. This leaves operators which are not
 declared to be extending to signal errors. By default, any slot declared to be a resource slot is associated
 with self-extending accessors.
 - The ad-hoc aspect concerns the property-value operator, which can be used on its own to cause prototype
 delegation and/or extension as well as the base operator for a slot reader/writer.")


(defclass rdf-slot-operator ()
  ((property
    :reader function-property :writer setf-function-property)
   (type
    :initarg :type :initform t
    :reader function-type))
  (:metaclass c2mop:funcallable-standard-class))

(defclass rdf-slot-writer (standard-generic-function rdf-slot-operator)
  ()
  (:metaclass c2mop:funcallable-standard-class))

(defclass rdf-slot-reader (standard-generic-function rdf-slot-operator)
  ()
  (:metaclass c2mop:funcallable-standard-class))

(defmethod initialize-instance ((instance rdf-slot-operator)
                                &key name (property (if (consp name) (second name) name)))
  (call-next-method)
  (setf (function-property instance) property))


(defparameter *property-function-registry* (make-hash-table))

(defun property-function (property)
  (gethash property *property-function-registry*))


(defun (setf property-function) (property function)
  (loop (etypecase function
          (symbol (return))
          (cons (setf function (second function)))
          (generic-function (setf function (c2mop:generic-function-name function)))))
  (assert (typep property '(and symbol (not null))) ()
          "Invalid property value: ~s." property)
  (let ((old (gethash property *property-function-registry*)))
    (cond ((eq old function)
           function)
          (t
           (unless (null old)
             (warn "Redefining operator for ~s from ~s to ~s." property old function))
           (setf (gethash property *property-function-registry*) function)))))


(defmethod (setf function-property) (property (function rdf-slot-operator))
  (setf (property-function function) property)
  (setf-function-property property function)
  property)

(defmethod function-property ((function-name symbol))
  (when (fboundp function-name)
    (function-property (fdefinition function-name))))

(defmethod function-property ((function-name cons))
  (when (fboundp function-name)
    (function-property (fdefinition function-name))))


(:documentation no-applicable-method
  "These are a complement to the defaccessor declarations. If a function is declared, it will
 have base methods for resource object. If it is declared as a generic function, or there is
 no declaration, any attempt operate on a property undeclared for the respective class via an
 accessor will lead to a no-applicable-method error. These methods patch the operators with
 the requisite method.")

;;;!!! these need to deal with the list/atom distinction

(defmethod no-applicable-method ((function rdf-slot-reader) &rest args)
  "If no slot reader applies to an (object) argument list, augment the function with a property-based
 accessor and retry. The generated method delegates to property-value, which will signal property-missing
 if the property is not present."
  (destructuring-bind (object) args
    (let* ((function-name (c2mop:generic-function-name function))
           (property (or (function-property function) function-name))
           (method-class (c2mop:generic-function-method-class function))
           (function-lambda `(lambda (object) (property-value object ',property)))
           (method (make-instance method-class
                     :function (compile nil function-lambda)
                     :qualifiers ()
                     :specializers (list (class-of object))
                     :lambda-list '(object)
                     #+ccl :name #+ccl function-name)))
      (add-method function method)
      (apply function args))))


(defmethod no-applicable-method ((function rdf-slot-writer) &rest args)
  "If no slot writer applies to a (new-value object) argument list, augment the function with a writer for the
 prototypal property and retry. The delegation to (setf prototypal-property-value), which will signal property-missing
 if no property is not present. It does _not_ augment acces to archetypal slots."
  (destructuring-bind (new-value object) args
    (declare (ignore new-value))
    (let* ((function-name (c2mop:generic-function-name function))
           (property (or (function-property function) (second function-name)))
           (method-class (c2mop:generic-function-method-class function))
           (type (function-type function))
           (function-lambda `(lambda (value object) (setf (prototypal-property-value object ',property ',type) value)))
           (method (make-instance method-class
                     :function (compile nil function-lambda)
                     :qualifiers ()
                     :specializers (list (find-class 't) (class-of object))
                     :lambda-list '(new-value object)
                     #+ccl :name #+ccl function-name)))
      (add-method function method)
      (apply function args))))



(defparameter *persistent-slot-reader.verbose* nil)
(defparameter *persistent-slot-writer.verbose* nil)

(define-method-combination persistent-slot-reader (&key (verbose-p *persistent-slot-reader.verbose*)
                                                        ((:slot-definition sd) nil)
                                                        (type (c2mop:slot-definition-type sd))
                                                        (name (c2mop:slot-definition-name sd)))
                           ((hollow (:hollow))
                            (new (:new))
                            (before (:before))
                            (after (:after))
                            (around (:around))
                            (primary () :required t))
  (:arguments instance)

  ;; the core-form comprises the standard method combination mechanism which expects
  ;; an instance to be present in the slot. every persistent slot access can also affect instance
  ;; state and/or content. if the slot binds a resource, also manage the instance/uri dual bindings

  (flet ((call-methods (methods)
           (mapcar #'(lambda (method)
                       `(call-method ,method ()))
                   methods)))
    (let ((form (if (or before after (rest primary))
                  `(multiple-value-prog1
                     (progn ,@(call-methods before)
                            (call-method ,(first primary)
                                         ,(rest primary)))
                     ,@(call-methods (reverse after)))
                  `(call-method ,(first primary) ()))))
      (when around
        (setf form
              `(call-method ,(first around)
                            (,@(rest around)
                             (make-method ,form)))))
      (setf form
            `(progn
               (typecase (object-state ,instance)
                 (new ,@(when new (call-methods new)))
                 (hollow (read-properties ,instance) ,@(call-methods hollow)))
               ;; presume that slot/statement values are in sync
               ;; guard against unbound sequences to return instead nil
               ,(if (list-type-p type)
                  `(when (or (not (slot-exists-p ,instance ',name)) (slot-boundp ,instance ',name))
                    ,form)
                  form)))
      (when verbose-p
        (pprint form *trace-output*))
      form)))

(define-method-combination persistent-slot-writer (&key (verbose-p *persistent-slot-writer.verbose*)
                                                        ((:slot-definition sd) nil)
                                                        (type (c2mop:slot-definition-type sd))
                                                        (name (c2mop:slot-definition-name sd)))
                           ((hollow (:hollow))
                            (new (:new))
                            (before (:before))
                            (after (:after))
                            (around (:around))
                            (primary () :required t))
  (:arguments value instance)
  type

  (flet ((call-methods (methods)
           (mapcar #'(lambda (method)
                       `(call-method ,method ()))
                   methods)))
    (let ((form (if (or before after (rest primary))
                  `(multiple-value-prog1
                     (progn ,@(call-methods before)
                            (call-method ,(first primary)
                                         ,(rest primary)))
                     ,@(call-methods (reverse after)))
                  `(call-method ,(first primary) ()))))
      (when around
        (setf form
              `(call-method ,(first around)
                            (,@(rest around)
                             (make-method ,form)))))
      (setf form
            `(when (or (not (slot-exists-p ,instance ',name))
                       (not (slot-boundp ,instance ',name))
                       (not (equalp ,value (slot-value ,instance ',name))))
               (typecase (object-state ,instance)
                 (rdf:new ,@(when new (call-methods new)))
                 (rdf:hollow (read-properties ,instance) ,@(call-methods hollow))
                 (rdf:deleted))
               (rdf:modify ,instance ',name)
               ,form))
      (when verbose-p
        (pprint form *trace-output*))
      form)))

;;; contingent instantiation and searching 

(defmethod model-value ((class resource-class) identifier)
  (model-value (class-repository class) identifier))

(defmethod rdf:ensure-instance ((class resource-class) identifier)
  (handler-case (rdf:find-instance class identifier)
    (instance-not-found-error ()
      (let* ((type (rdf:type-of (class-repository class) identifier))
             (identified-class (or (rdf:find-class class type) 'resource)))
        (if (typep identified-class 'resource-class)
          ; if the type is known, construct a new instance of the respective class
          (setf (rdf:find-instance class identifier)
                (make-instance identified-class :uri (model-value class identifier)
                               :repository (class-repository class)))
          identified-class)))))


(defmethod rdf:find-instance ((class resource-class) identifier)
  (or (rdf:find-instance (class-repository class) identifier)
      (rdf:instance-not-found class identifier)))


(defmethod (setf rdf:find-instance) (instance (class resource-class) identifier)
  (setf (rdf:find-instance (class-repository class) identifier) instance))


(defmethod rdf:find-class ((class resource-class) (name symbol) &key (error-p t))
  "GIven a concrete class, first delegate to the abstract version to look for sibling classes,
 then try the vocabularies loaded from the repository. If none is found optionally signal an error."

  (or (rdf:find-class (class-of class) name :error-p nil)
      (let ((definition (rdf:find-class (class-vocabulary class) name :error-p nil)))
        (when definition
          (prog1 (eval definition)
            (dolist (superclass (third definition))
              (rdf:find-class class superclass)))))
      (rdf:find-class (class-repository class) name :error-p nil)
      (when error-p
        (rdf:class-not-found class name))))


(defmethod rdf:find-class ((class standard-class) (name symbol) &key (error-p t))
  (or (let ((found (find-class name nil)))
        ;; should either return the class resource-class or one of its instances
        (typecase found
          ((or null c2mop:forward-referenced-class)
           nil)
          (resource-class
           found)
          (t
           (unless (eq (class-name class) 'resource-class)
             (warn "find-class returning a non-resource class: ~s." found))
           found)))
      (when error-p
        (rdf:class-not-found class name))))



(defmethod rdf:find-class ((class resource-class) (identifier t) &rest args)
  (declare (dynamic-extent args))
  (apply #'rdf:find-class class (rdf:model-value (class-repository class) identifier) args))



(defmethod rdf:class-not-found ((class standard-class) type)
  (class-not-found-error :metaclass class :name type))


(defmethod rdf:type-of ((class resource-class) identifier)
  (rdf:type-of (class-repository class) identifier))


(defgeneric rdf:class-property-slots (class)
  (:method ((class resource-class))
    (reduce #'append
            (c2mop:class-precedence-list class)
            :key #'(lambda (class)
                     (remove-if-not #'(lambda (sd) (typep sd 'rdf:archetypal-property-definition))
                                    (c2mop:class-direct-slots class))))))
;;; (class-property-slots (find-class 'adult))


#+(or)
(defun rdf:ensure-class (name)
  (or (find-class name nil)
      (make-instance 'resource-class :name name)))


(defmethod c2mop:finalize-inheritance ((class resource-class))
  (dolist (super (c2mop:class-direct-superclasses class))
    (when (typep super 'c2mop:forward-referenced-class)
      ;; attempt to load the class, but defer to an eventual forward-reference-class error
      (rdf:find-class class (class-name super) :error-p nil)))
  (call-next-method))
