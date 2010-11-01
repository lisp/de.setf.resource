

DE.SETF.RESOURCE: a CLOS projection for RDF repositories
-------

Status
------

`de.setf.resource` is intended to serve two goals:

- immediately, to provide the persistence service for the setf lab [site](http://www.setf.de/lab.html).
  One goal of that work is to infer regression test plans from source code revision information.
  A RDF-based persistent model is well suited for this.
- longer term, to permit Lisp to continue as a development platform
  in the open-source linked-data ecosystem.

The paramount requirement is that the library provide simple, transparent, implicit persistence for
CLOS-based data models. In order to 'fit-in', it should provide a standard interface which can be ported
to the various available triple-stores.
The proposals for Lisp RDF support were either too primitive[[1]] or too ponderous[[2]] to satisfy these requirements.
They require that, before an application could devote any to application logic, it would either
devote significant attention to manipulating triples and identifiers directly 
or realize its model in terms of OWL paterns.

Alternative Lisp persistence solutions, such as Elephant[[3]], and persitence solutions in
alternative languages[[4]], and in particular RDF persistence solutions in alternative
languages[[5]], suggested an appropriate medium. The Spira API became the basis for the RDF-specific
API and all other operations were designed to follow standard CLOS usage as closely as possible.

Once instance creation and state management was completed on the basis of the wilbur[[6]]
triple-store, the next step was to implement the interface to a persistent triple-store.
The conventional triple-store model - as adapted from RDF.rb and sesame, was  sufficient
to integrate both wilbur and - at least in nominal terms, allegrograph. it served as the target
to implement a triple-store based on cassandra[[7]].
This work progressed for several weeks and reached a proof-of-concept implementation
which supports graph-based revisions.
More immediately siginificant, as work on the cassandra store module progressed, so did discussions with
datagraph.org contributors, and it was proposed the i implement the SPARQL algebra engine in Lisp for a SPARQL/RDF
system, the SPOCQ system.

If one takes that system as an archetype of an application the linked-data ecosystem,
the `resource` library plays a significant role for Lisp applications, but a subordinate
role in the larger [scheme](./spocq.png).
The current focus is to complete the SPARQL algebra engine for the SPOCQ system and
use the BGP matching interface to implement a Lisp binding for the underlying RDFCache triple store.
This will provide a high-performance, scalable storage substrate for the library.


The present `de.setf.resource` version embodies a few hundred hours work for the core resource class and store api implementation,
and close to a hundred more for the cassandra store implementation. As of 2010-06-15, when tested against the wilbur store,
test entry coverage
was a bit over fifty percent and code path coverage a bit under.
Given its context and purpose, unless there are express requests for corrections and or additional features,
it will likely remain stable in this state for the next while, as work proceeds on other elements of the
larger package.

The wilbur and cassandra store mediators both implement the core add/map/delete statement interface.
The most recent cassandra mediator version (2010-08-09) includes the first thoughts on how to use a
triple store to back an STM for CLOS.
The wilbur mediator implements file and http loading.
The development rutimes are mcl (5.2) and sbcl (1.0.36), but network operations are based on usocket[[8]]
and mop operations are based on closer-mop[[9]], so
the porting threshold should be low. additional conformance vetting is performed against clozure (1.4)
The allegro interface is on-hold as the build process exhausts the heap in the free edition.


### Identifier Datatypes

The present implementation supports three representations for resource identifiers: PURI:URI, UUID, and symbols.
The latter facilitate code which integrates
link data schema with function and/or class names,
but it also entails some package housekeeping, as the identifiers are all interned globally.
In order to operate on unbounded data with non UUID identifiers, it will be necessary to distinguish between identifiers
for resources in a known vocabulary schema and those for 'instance' resources and/or ephemeral schema.

### Iteration over Bag/Sequence/SetData

Large data collections will require some form of incremental iteration for sequence slot values.
Concrete stores - whether triple/bgp based or sparql based, support offset/count constraints, but the mechanism is
not exposed though slot accessors.
An additional operator collection will be necessary to expose this control pattern for slot access.

### Monotonicity

An central architectural question is whether a store is monotonic.
The `resource` library is implemented from the perspective than any changes are effected in the contxt of a transaction
and that transactions are realized in combination with a revision/versioning mechanism.
This could be accomplished with graphs/contexts, or with in intrinsically versioned triple-store.
The instance meta-data records sufficient information to implement deletion, but it is not implemented, as it is the wrong thing
to do.

This perspective informed the initial work on the cassandra storage mechanism and is central to the datagraph implementation.
Given that Datagraph's RDFCache is th intended primary storage backend,
there has yet been no reason to change the approach.

### Portability

Each store implementation representes  primitive data in its own way.
This is particulary true of allegrograph, which has a unique iri representation.
The `resource` library intends to insulate the implementation from these difference by mapping between
`model` and `store` data.
While this does engender portability, it introduces latency in the store interface and duplicates
data. In order to eliminate this threshold, one must push a consistent representation in one direction or the other.
The cassandra store model does this by carrying the model's representation all the way to the binary codecs.
If it were to be of concern to improve the performance of a store such as allegrograph, one would need to change
the model to carry allegrograph's native representation in the other direction, into the model.

### Schema Introspection
The `vocabulary` operations import RDF schema into CLOS class models, and export them as Lisp
definitions, but export to RDF has not been implemented.

### Thread Safety

CLOS is not thread-safe. In order to support open-ended RDF -> CLOS projection, numerous side-effecting operations
are performed on classes and instances. The effects of these operations are defined for a single thread only.

 ---
 [1]: [allegrograph](http://www.franz.com/agraph/allegrograph/)
 [2]: [swclos](http://research.nii.ac.jp/TechReports/09-014E.html)
 [3]: [elephant](common-lisp.net/project/elephant/)
 [4]: [ActiveRecord](api.rubyonrails.org/classes/ActiveRecord/Base.html)
 [5]: [Spira](blog.datagraph.org/2010/05/spira)
 [6]: [wilbur](http://lisp.github.com/wilbur)
 [7]: [cassandra](http://cassandra.apache.org/)
 [8]: usocket
 [9]: closer-mop