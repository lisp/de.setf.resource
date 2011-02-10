

DE.SETF.RESOURCE: a CLOS projection for RDF repositories
-------

Introduction
------------
 `de.setf.resource` implements transparent projection from RDF repositories into CLOS models.
 It relies on the CLOS-MOP to implement a collection of metaclasses and operators to 
 equate CLOS class/instance/slot[[0]] structure with a domain/resource/property oriented view of RDF graphs.
 It includes interfaces to wilbur[[1]],[[2]],[[3]], , cassandra[[9]], and allegro-graph[[4]] repositories
 which permits both memory-resident and remote repositories.

Architecture
------------
 The core implementation involves several classes

 - resource-metaclass : the metaclass associates URI-designated RDF datatypes with resource-class instances to support
   projection of entire graphs.
 - resource-class : the metaclass associates individual URI with CLOS instances, extends the standard-class
   slot access protocol based on slot declarations to support RDF properties, and adds a prototype-based
   storage model to manage properties absent schemas 
 - resource-object : the abstract class projects the resource URI and predicated properties for a repository
   subject node onto a CLOS instance, its instance slots, and its prototypical properties, each of which
   corresponds to a triple property or a computed graph predicate. In addition to the predicated values and
   any transient slots, each instance also includes state and history slots, which are interpreted in
   combination with instantiation, slot-access, and transaction operators to implement a persistence
   life-cycle[[5]]. In this protocol, the CLOS instance behaves as an heap model for the persistent state of
   nodes in the RDF repository.

 Specialized resource-object classes can be defined in advance, the application can leave them to be
 constructed on-demand based on models accompanying the resource data, or data can be manipulated through
 unclassified prototypes.
 All variations share a common access interface for predicated properties, that of functional accessors.
 Given predefined classes, these are specified in the class declaration.
 Where the class definition is derived from a schema, the accessors are translated from the respective RDF
 vocabulary. Where prototypes are used, they are are defined on-the-fly based on the respective property
 names.
 The accessors implement a protocol which handles internalizing/externalizing RDF literal and resource values,
 lazy construction of related resource-object instances on-demand.
 In combination with the instance state and history, the accessors maintain the correspondence between
 external statements and slot values.


Precedents
----------

There are several precedents for CLOS-MOP based persistence

- CL-SQL
- Elephant
- Pevelance
- PCLOS
- Rucksack
- SWCLOS
- CL-RDFXML[http://www.cs.rpi.edu/~tayloj/CL-RDFXML/] : a direct rdf model interface which transforms an RDF document into a triple stream

They share a basic architecture, which uses the CLOS-MOP to extend the slot access mechanism to encapsulate access to persistent data.
Within that framework each demonstrates a variation in the following aspects:

- mapping granularity - slot v/s instance : are slots/properties/fields projected as a unit for an instance, or individually per slot
- identity : do unique instances correspond to external data or can there be multiple projections
- caching : does the instance act as a cache for projected data or does each access read/write through to external data

The `de.setf.resource` resource-object protocol implements

- instance granularity projection for literals and resource with per-property read granularity as an option for resources.
- URI-based object identity.
- slot-based caching with automatic internalization/externalization based on the relation between declared property 
  Lisp type and RDF datatype.


Repositories
------------

The abstract class resource-mediator embodies the interface to an RDF store.
Each os its concrete specializations implements the requisite access operations based on its respective RDF repository.
Mediators are implemented as

- wilbur-mediator, for wilbur 2.0
- cassandra-mediator, for cassandra 0.6.4
- allegrostore-mediator, for allegrograph

Each provides a cache to unify instances in terms of the repository's representation resource identifiers,
a find-instance operator which accepts URI and locates or creates and interns an instance, a select-statements operator
to constrain projection - in particular to those statements which relate to a node, and a delete-statements
operator to delete or limit validity of statements as a consequence of property modification.


Transactions
------------
Transactions expressed with a `with-transaction` form.

The normal control flow through that form  opens a transaction,
 executes the body and commits the transaction upon completion. When the transaction commits, the complete content of
 new persistent instances and the modifications to existing are projected onto the storage repository. This involves
 creating statements with the respective object as subject combined with slot designators as property and slot value(s)
 as object. Slot modifications require, in addition, that previous statements be deleted.

A non-local exit causes the transaction to rollback. This is accomplished by returning instances to theirs state
prior to any modifications within the transaction. New instances are returned to transient status and prior instances
are reinitialzed to reflect the state of the repository.

    with-transaction ((source) &body body)


Classification
--------------

CLOS relies on a nominal type system. RDF - at least for those documents associated with a schema, also relies on a
nominal type system. A strict open-world view, however, requires that a system comprehend instance constituents
independent of an apriori definition. For CLOS this would entail some form of structural typing.
Perhaps even opportunistically structural.

Anomalies between the defined CLOS model and the actual data can appear in two situations.

- A CLOS class elected by the application as the target model for the projection does not include a property which is present in the
  RDF graph.
- A resource which denotes a constituent in a relation (subject or object) is not associated with a class in the CLOS model.

That is, an exceptional condition arises if either a single class or the application concept model do not correspond to the external model.
In which cases, in general, there are three alternatives: abort the projection, ignore the unknown assertion(s), or extend the CLOS model.

In the case, where the discrepancy is with a class definition, where no slot exists for a property asserted for a given subject object,
there are several specific options.

- signal an unrecoverable error to abort the projection.
- skip the property.
- regard the instance as a prototype and record the property specific to the instance.
- extend the immediate class definition to include a slot for the property.
- find an alternative class which includes a property for the slot, most immediately a specialization of the current class, and change the instance class.
- define a new class - either as an alternative or as a specialization, to include the property, and change the instance class.

To some extent the proper response depends on whether the projection context includes a schema and RDF nodes have types more specific than `rdfs:Resource`.
If a schema is present, as its purpose was to have been to specify the structure of the external model, a strict process must signal an error, while
a robust process should either treat the instance as a prototype or locate/create a specialized subclass - specific to the schema, to accept extensions.
If, on the other hand, no schema was present, then the best response would be to extend the class as required and associate it with the resource domain.


In the second case, when no class is associated with a subject's base uri - whether projected as an autonomous subject,
or when projected as the object of an assertion, the options are similar to those for an isolated slot, depend on additional information.

- signal an unrecoverable error to abort the projection.
- skip the resource, or project it as a literal uri.
- create a prototype to represent resources in that domain as untyped individuals.
- locate a class includes the requisite properties as slot.
- define a new class.

On one hand, if an external schema is present, its definitions may provide either a nominal correspondence, or a structural analogy.
In the first case the class names are compared to a registry. In the second case, the declare properties are compared to
the properties of known classes. If either yields a match, that class should serve as the target for resources in the given domain.
If no external schema is present, then the only alternatve to continue with a typed data model is to opportunistically define the class.
In the latter case, one requires an additional interaction with the repository to obtain schema information. 
Which means the operation cannot occur in-line, as the dynamic state may be in the middle of a continuing statement stream.
It must instead be deferred to the end of the stream, at which point "forward-reference" instances can be classified either nominally or structurally.


## Status

The implementation has reached ["I want to see you"](http://www.loc.gov/exhibits/treasures/trr002.html) status.
The [documentation](./documentation/package_DE.SETF.RESOURCE.xhtml) describes the implemented API.
See [README-status.md](./READMES/README-status.md) for more details.


## Downloading

[github](http://github.com/lisp/de.setf.resource)


## Building

In principle, `de.setf.resource` is built with [`asdf`](http://www.common-lisp.net/projects/asdf).
Please consult the detailed instructions for the respective [runtime](./readmes/README-build.md) for more information.
 
## Licensing

This version is released under version 3 of the GNU Affero license (GAL).[[5]]
The required components are included as per the respective licenses and covered,
in this combined form, under the GAL as well. If you need a different license, get in touch.

- [com.github.ironclad](http://method-combination.net/lisp/ironclad/) MIT-like
  - 2009 [Nathan Froyd](froydnj@gmail.com)
- [net.dardoria.uuid](http://www.dardoria.net/software/uuid.html) LLGPL
  - 2008 [Boian Tzonev](boiantz@gmail.com)
- closer-mop :  MIT-style
  - 2005 - 2010 [Pascal Costanza](http://p-cos.net)
- [cl-ppcre](http://www.weitz.de) : equivalent to MIT
  - 2002-2008, [Dr. Edmund Weitz](http://www.weitz.de)
- [com.b9.puri]() : LLGPL, by which com.b9.puri.puri-ppcre is also covered by the LLGPL
  - 1999-2001 [Franz, Inc](mailto:opensource@franz.com).
  - 2003 [Kevin Rosenberg](mailto:kevin@rosenberg.net)
- [usocket](mailto:usocket-devel@common-lisp.net) : MIT, through 2007. later work unspecified
  - 2003 Erik Enge
  - 2006-2007 Erik Huelsmann 
- [trivial-utf-8](http://common-lisp.net/project/trivial-utf-8/)
- [de.setf.wilbur](http://github.com/lisp/de.setf.wilbur) was [net.sourceforge.wilbur](http://wilbur-rdf.sourceforge.net/) LLGPL
  The fork contains several corrections and extensions to permit use with SBCL and MCL.
  - 2010 [Ora Lassila](ora.lassila@nokia.com)

 ---
- Bobrow, Daniel G., DiMichiel, Linda G., Gabriel, Richard P., Keene, Sonya E., Kiczales, Gregor, and Moon, David A.,
 "Common Lisp Object System specification: 1. Programmer interface concepts." Lisp and Symbolic Computation 1, 3/4 (January 1989), 245-298, 299-394 [[0]]
- Lassila, Ora, "Taking the RDF Model Theory Out For a Spin"[[1]]
- Klaus Ostermann: Nominal and Structural Subtyping in  Component-Based Programming, in Journal of Object Technology, vol. 7, no. 1,
 January–February 2008, pages 121–145, http://www.jot.fm/issues/issues 2008 January-February/  
- Martin Odersky, Vincent Cremet, Christine Röckl, Matthias Zenger, "A Nominal Theory of Objects with  Dependent Types"

 [0]: http://dreamsongs.com/Files/concepts.pdf, http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node260.html
 [1]: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.100.6738&rep=rep1&type=pdf  
 [2]: http://wilbur-rdf.sourceforge.net/  
 [3]: http://lisp.github.com/wilbur  
 [4]: http://www.franz.com/agraph/  
 [5]: http://en.wikipedia.org/wiki/Java_Data_Objects  
 [6]: Papecke, Andreas, "PCLOS: A Critical Review", http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.8.3357&rep=rep1&type=pdf, http://www-db.stanford.edu/~paepcke/shared-documents/pclos-critical.ps  
 [7]: Andreas Paepcke. "PCLOS: Stress Testing CLOS - Experiencing the Metaobject Protocol". In Proceedings of the Conference on Object-Oriented Programming Systems, 1990.  http://www-db.stanford.edu/~paepcke/shared-documents/pclosmeta.ps  
 [8]: Andreas Paepcke. "PCLOS: A Flexible Implementation of CLOS Persistence". In S. Gjessing and K. Nygaard, editors, Proceedings of the European Conference on Object-Oriented Programming (ECOOP). Lecture Notes in Computer Science, Springer Verlag, 1988. http://www-db.stanford.edu/~paepcke/shared-documents/pclos-report.ps  
 [9]: http://cassandra.apache.org/
 [10]: agpl.txt


