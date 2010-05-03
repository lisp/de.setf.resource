

DE.SETF.RESOURCE: a CLOS projection for RDF repositories
-------

Introduction
------------
 `de.setf.resource` implements transparent projection for RDF repositories as CLOS models.
 The implementation relies on the CLOS-MOP support for augmented slot access mechanisms to
 equate CLOS class/instance/slot[0] structure with a domain/resource/property oriented view of RDF graphs. It includes
 interfaces to wilbur[1,2,3] and allegro-graph[4] repositories and permits both memory-resident and remote
 repositories.

Architecture
------------
 The core implementation involves three classes

 - resource-metaclass : the metaclass associates URI realms (base-uri) with resource-class instances to support
   projection of entire graphs.
 - resource-class : the metaclass associates individual URI with CLOS instances and exteneds the standard-class
   slot access protocol based on slot declarations to support RDF assertions
 - resource-object : the abstract class projects the URI and relationships for a repository subject node onto
   an instance and its property slots, each of which corresponds to a triple property or
   a computed graph predicate. In addition to the properties and any transient slots, each instance also includes
   a state slot, which is interpreted in combination
   with instantiation, slot-access, and transaction operators to implement a life-cycle[5] in which 
   the CLOS instance behaves as an heap model for the persistent state of nodes in the RDF repository.

There are several precedents for CLOS-MOP based persistence

- CL-SQL
- Elephant
- Pevelance
- PCLOS
- Rucksack

They share a basic architecture, which uses the CLOS-MOP to extend the slot access mechanism to encapsulate access to persistent data.
Within that framework each demonstrates a variation in the following aspects:

- mapping granularity - slot v/s instance : are slots/properties/fields projected as a unit for an instance, or individually per slot
- identity : do unique instances correspond to external data or can there be multiple projections
- caching : does the instance act as a cache for projected data or does each access read/write through to external data

The`de.setf.resource` resource-object protocol implements

- instance granularity projection for literals and resource with per-property read granularity as an option for resources.
- URI-based object identity
- slot-based caching with

Repositories
------------
Each concrete class binds a mediator between it and a concrete RDF repository. Two mediators are defined
- wilbur-mediator
- allegrostore-mediator
Each provides a weak cache to unify instances in terms of the repository's representation resource identifiers,
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
nominal type system. A strict open-world view, however, requires that a system be comprehend instance constituents
independent of an apriori definition. For CLOS this would entail some form of structural typing.
Perhaps even opportunistically structural.

Anomalies between the defined model and the actual data can appear in two situations.

- A CLOS class elected by the application as the target model for the projection does not include a property which is present in the
  external model.
- A resource which denotes a constituent in a relation (subject or object) is not associated with a class in the CLOS model.

That is, an exceptional condition arises if either a single class model or the application concept model do not correspond to the external model.
In which cases, in general, there are three alternatives: abort the projection, ignore the unknown assertion(s), or extend the CLOS model.

In the case, where the discrepancy is with a class definition, where no slot exists for a property asserted for a given subject object,
there are several specific options.

- signal an unrecoverable error to abort the projection.
- skip the property.
- regard the instance as a prototype and record the property specific to the instance.
- extend the immediate class definition to include a slot for the property.
- find an alternative class which includes a property for the slot, most immediately a specialization of the current class, and change the instance class.
- define a new class - either as an alternative or as a specialization, to include the property, and change the instance class.

To some extent the proper response depends on whether the projection context includes a schema.
If a schema is present, as its purpose was to have been to specify the structure of the external model, a strict process must signal an error, while
a robust process should either treat the instance as a prototype or locate/create a suecpaized subclass - specific to the schema, to accept extensions.
If, on the other hand, no schema was present, then the best response would be to extend the class as required and associate it with the resource domain.


In the second case, when no class is associated with a subject's base uri - whether projected as an autonomous subject,
or when projected as the object of an assertion, the options are similar to those for an isolated slot, depend on additional information.

- signal an  unrecoverable error to abort the projection.
- skip the resource, or project it as a literal uri.
- create a prototype to represent resources in that domain as untyped individuals.
- locate a class includes the requisite properties as slot.
- define a new class.

On one hand, if an external schema is present, its definitions may provide either a nominal correspondence, or a structural analogy.
In the first case the class names are compared to a registry. In the second case, the declare properties are compared to
the properties of known classes. If either yields a match, that class should serve as the target for resources in the given domain.
If no external schem is present, then the only alternatve to continue with a typed data model is to opportunistically define the class.
In the latter case, one requires an additional interaction with the repository to obtain schema information. 
Which means the operation cannot occur in-line, as the dynamic state may be in the middle of a continuing statement stream.
It must instead be deferred to the end of the stream, at which point "forward-reference" instances can be classified either nominally or structurally.

 ---
 [0] : Bobrow, Daniel G., DiMichiel, Linda G., Gabriel, Richard P., Keene, Sonya E., Kiczales, Gregor, and Moon, David A.  
"Common Lisp Object System specification: 1. Programmer interface concepts." Lisp and Symbolic Computation 1, 3/4 (January 1989), 245-298, 299-394
http://dreamsongs.com/Files/concepts.pdf, http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node260.html
 [1] : Lassila, Ora, "Taking the RDF Model Theory Out For a Spin", http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.100.6738&rep=rep1&type=pdf  
 [2] : wilbur-rdf.sourceforge.net/  
 [3] : lisp.github.com/wilbur  
 [4] : http://www.franz.com/agraph/  
 [5] : http://en.wikipedia.org/wiki/Java_Data_Objects  
 [6] :  Papecke, Andreas, "PCLOS: A Critical Review", http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.8.3357&rep=rep1&type=pdf, http://www-db.stanford.edu/~paepcke/shared-documents/pclos-critical.ps  
 [7] : Andreas Paepcke. "PCLOS: Stress Testing CLOS - Experiencing the Metaobject Protocol". In Proceedings of the Conference on Object-Oriented Programming Systems, 1990.  http://www-db.stanford.edu/~paepcke/shared-documents/pclosmeta.ps  
 [8] : Andreas Paepcke. "PCLOS: A Flexible Implementation of CLOS Persistence". In S. Gjessing and K. Nygaard, editors, Proceedings of the European Conference on Object-Oriented Programming (ECOOP). Lecture Notes in Computer Science, Springer Verlag, 1988. http://www-db.stanford.edu/~paepcke/shared-documents/pclos-report.ps  

Status
------

At the moment, github has just these notes.


Downloading
-----------

...

Building
---------

...
 
Licensing
---------

Unknown, except that it depends on P. Costanza's work to provide portable CLOS-MOP operations.

- closer-mop :  MIT-style
  - 2005 - 2010 [Pascal Costanza](http://p-cos.net)

