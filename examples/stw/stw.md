    ;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.resource.implementation; -*-

# Visualizing STW Concepts

 This file uses the resource library to visualize STW concept networks.

 The '[STW Thesaurus for Economics](http://zbw.eu/stw/versions/latest/about.en.html)
 serves the German National Library of Economics ([ZBW](http://www.zbw.eu/))
 as the vocabulary for [thematic access]( http://www.w3.org/2001/sw/sweo/public/UseCases/ZBW/) to their collection.
 The ZBW publishes it as RDF using an extension
 to the [SKOS](http://www.w3.org/TR/skos-reference/) ontology. (See [also](http://en.wikipedia.org/wiki/Simple_Knowledge_Organization_System).)
 As of version 8.06, the documentation suggests about 6,000 headings and 18,000
 entry terms. The data itself indicates 6,524 nodes and 31,425 distinct related labels.

 This example demonstrates how to use the resource library together with an RDF store to render concept
 networks as visual graphs. It starts with the RDF/XML version of the [document](http://zbw.eu/stw/versions/latest/download/about)
 which is available from the ZBW web [site](http://zbw.eu/stw/versions/latest/download/about).


## Getting the data

 The first step is to load the the ntriple version of the STW.
 As the original is fourteen megabytes of RDF/XML, there is some advantage to recoding it as ntriples,
 even just for the convenience of [grepping](http://blog.datagraph.org/2010/03/grepping-ntriples)
 through it to gauge node names, the class mix, or
 the language variations. The [raptor](http://librdf.org/raptor/) tools recode the the document as ntriples with the command

    $ rapper stw.rdf > stw.nt

 Given which, it is a simple operation to load it into the rdf store. Should it be necessary to first
 clear the repository:

    ? (rdf:clear-repository (wilbur-mediator))

 On a vintage G5-2x1.8 with mcl, the process takes about two minutes, yields about 114K nodes and uses
 about 150 megabutes.

    ? (rdf:load-repository (wilbur-mediator) #P"LIBRARY:examples;data;stw.nt")

## Building an API

 The next step is to construct an interface to the data. If the process were driven by application
 requirements, a data model could evolve from process and presentation requirements, to take the
 form of apriori class definitions. In this case, the task is to render as a graph the concept network
 implicit in the relations among the thesaurus' terms.

 In this respect, we observe one of the predicaments of linked data processing: on one hand, one would like
 to avoid a closed model, as that would preclude access to data beyond its purview, but on the other
 the mechanisms to be applied in order to infer a data model, or to adapt or extend a core, are
 frequently ineffective, as the data itself can be incomplete, inconsistent, or even just exhibit sufficient
 variations as to encumber comprehension.

 This task exhibits two such incidental artifacts:

* the correlation between vocabulary names and resources may be such that it is not obvious how to
 derive the schema.

* a schema may simply be incomplete
 

The first issue manifests in the identifers associated with the ZBW schemas. The ontology, per se is identified
 as `http://zbw.eu/stw/`. To wit:

    $ fgrep 'http://www.w3.org/2002/07/owl#Ontology'  stw.nt 
    <http://zbw.eu/stw/> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#Ontology> .
    <http://www.w3.org/2004/02/skos/core> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#Ontology> .

 Yet, in the event, the data indicates that it is defined by another resource

    $ fgrep DefinedBy stw.nt | fgrep zbw
    <http://zbw.eu/namespaces/zbw-extensions/Thsys> <http://www.w3.org/2000/01/rdf-schema#isDefinedBy> <http://zbw.eu/namespaces/zbw-extensions/zbw-extensions.rdf> .
    <http://zbw.eu/namespaces/zbw-extensions/indexedItem> <http://www.w3.org/2000/01/rdf-schema#isDefinedBy> <http://zbw.eu/namespaces/zbw-extensions/zbw-extensions.rdf> .
    <http://zbw.eu/namespaces/zbw-extensions/useInsteadNote> <http://www.w3.org/2000/01/rdf-schema#isDefinedBy> <http://zbw.eu/namespaces/zbw-extensions/zbw-extensions.rdf> .
    <http://zbw.eu/namespaces/zbw-extensions/Descriptor> <http://www.w3.org/2000/01/rdf-schema#isDefinedBy> <http://zbw.eu/namespaces/zbw-extensions/zbw-extensions.rdf> .

 which is of limited use. as neither the terms nor the definition resource are known in advance, this could be addressed
 only by a before-and-after set difference of `isDefinedBy` statements. There must be an ontology vocabulary namespace
 specification somewhere, but it's not ovious. I observe the `{skos}inScheme` assertions, but they are not helpful, as
 they assert that an _instance_ is in a scheme, rather than a class, which seems both contrary to a basic linked data tenet,
 and ambiguous, as it does not entail which of the thing's classes is in the scheme, and, in any case, the designated resource
 yielded no scheme. An attempt to determine it from the actual resources
 is also confounded, as a schema retrieved from the abstract vocabulary namespace resource, `http://zbw.eu/namespaces/zbw-extensions/`,
 indicates yet another incidental
 source -- `http://2007.zbw.eu/namespaces/zbw-extensions/zbw-extensions.rdf`, about which the data revals nothing.
 even `fgrep 'zbw-extensions/>' stw.nt` reveals nothing beyond publication information about some _thing_.
 
 As a consequence, the knowledge gleaned from examining the data as text must be incorporated into the vocabulary
 definition explicitly.

 
    ;;; should it be necessary to delete the vocabulary set its reference to nil
    ;;; (setf (rdf:find-vocabulary (wilbur-mediator) "http://zbw.eu/namespaces/zbw-extensions/") nil)

    ? (rdf:ensure-vocabulary (wilbur-mediator) "http://zbw.eu/namespaces/zbw-extensions/"
                             :resource-uri "http://zbw.eu/namespaces/zbw-extensions/zbw-extensions.rdf")

 For convenience, alias the package

    ? (rename-package "http://zbw.eu/namespaces/zbw-extensions/"
                      "http://zbw.eu/namespaces/zbw-extensions/" '("zbw" "stw"))
    ? (rename-package "http://www.w3.org/2004/02/skos/core#"
                      "http://www.w3.org/2004/02/skos/core#" '("skos"))

 Two functions return information about the schema associated with a vocabulary. The first, `repository-schema-types`,
 returns a list of the classes present in the repository which were defined by a givne resource:

    ? (respository-schema-types (wilbur-mediator) '{zbw}zbw-extensions.rdf)
    (|http://zbw.eu/namespaces/zbw-extensions/|::|Descriptor| |http://zbw.eu/namespaces/zbw-extensions/|::|Thsys|)

 The second, `vocabulary-definitions`, returns a list of the definitions which were computed when the vocabulary
 was extracted:

    ? (vocabulary-definitions (rdf:find-vocabulary (wilbur-mediator) "http://zbw.eu/namespaces/zbw-extensions/"))
    ((rdf:defclass {zbw}Thsys ({skos}Concept)
       ()
       (:datatype {zbw}Thsys))
     (rdf:defclass {zbw}Descriptor ({skos}Concept)
       ()
       (:datatype {zbw}Descriptor)))


 In order to examine the class structure, first we finalize the classes.

    ? (c2mop:finalize-inheritance (rdf:find-class (wilbur-mediator) '{zbw}Descriptor))
    ? (c2mop:finalize-inheritance (rdf:find-class (wilbur-mediator) '{zbw}Thsys))

 after which, we can summarize the class definition,

    ? (mapcar #'c2mop:slot-definition-name (c2mop:class-slots (find-class '{zbw}Descriptor)))
    (URI SOURCE STATE GRAPH HISTORY PROPERTIES {skos}topConceptOf {skos}semanticRelation #:TOPCONCEPTOF #:SEMANTICRELATION)
    ? (mapcar #'class-name (c2mop:class-precedence-list (find-class '{zbw}Descriptor)))
    ({zbw}Descriptor {skos}Concept RESOURCE-OBJECT STANDARD-OBJECT T)

 and are surprised to see that although `{skos}Concept` is included, only two of its properties are reified as slots.
 The problem is, although the SKOS schema defines thirty-two predicates, to wit

   $ rapper http://www.w3.org/2009/08/skos-reference/skos.rdf | fgrep DefinedBy

 only five of them appear to have domain specifications, to wit

   $ rapper http://www.w3.org/2009/08/skos-reference/skos.rdf | fgrep domain

 of those, `{skos}hasTopConcept` and `{skos}semanticRelation`, are recognized and appear as archetypal properties.
 It would be possible to augment the repository schema or compose an explicit vocabulary, but for this example
 the incomplete schema is itself useful, as it demonstrates how resources behave when presented with data beyond
 their definition. One alternative is to use the operator `property-value` to access the properties in a manner
 analogous to explicit slot access. It - and the `setf` complement, provide uniform access to archetypal and
 prototypal slots. A better alternative is to define accessors.

    (rdf:defaccessor concept-narrower (concept) :property {skos}narrower :type list)
    (rdf:defaccessor concept-broader (concept) :property {skos}broader :type list)
    (rdf:defaccessor concept-related (concept) :property {skos}related :type list)
    (rdf:defaccessor concept-label (concept) :property {rdfs}label :type list)
    (rdf:defaccessor pref-label (concept) :property {skos}prefLabel :type list)
    (rdf:defaccessor alt-label (concept) :property {skos}altLabel :type list)

 These define the property complement relevant to the application, provide names more purposeful to the
 application logic, and declare the intended application datatype.


## Constructing a CLOS model for the concept graph

 There are two classes of terms: 'topic headings' (Thsys), and 'descriptions' (Descriptor).
    ? (rdf:query (wilbur-mediator)
               :subject (rdf:subject (first (rdf:query (wilbur-mediator) :predicate '{rdf}type :object '{zbw}Thsys :limit 1)))
               :continuation 'print)
    ---
    #<TRIPLE !"http://zbw.eu/stw/thsys/70897" !rdf:type !"http://zbw.eu/namespaces/zbw-extensions/Thsys" #xE035D46> 
    #<TRIPLE !"http://zbw.eu/stw/thsys/70897" !skos:prefLabel #"W.14.03.02  Kapitalmarkttheorie"@@de #xDFB9A96> 
    #<TRIPLE !"http://zbw.eu/stw/thsys/70897" !skos:prefLabel #"W.14.03.02  Financial Economics"@@en #xDFB9A3E> 
    #<TRIPLE !"http://zbw.eu/stw/thsys/70897" !skos:narrower !"http://zbw.eu/stw/descriptor/18138-2" #xDE44706> 
    #<TRIPLE !"http://zbw.eu/stw/thsys/70897" !skos:narrower !"http://zbw.eu/stw/descriptor/19269-3" #xDE446AE> 
    #<TRIPLE !"http://zbw.eu/stw/thsys/70897" !skos:narrower !"http://zbw.eu/stw/descriptor/19537-4" #xDE44656> 
    #<TRIPLE !"http://zbw.eu/stw/thsys/70897" !skos:narrower !"http://zbw.eu/stw/descriptor/19196-4" #xDE445FE> 
    #<TRIPLE !"http://zbw.eu/stw/thsys/70897" !skos:narrower !"http://zbw.eu/stw/descriptor/19271-2" #xDE445A6> 
    #<TRIPLE !"http://zbw.eu/stw/thsys/70897" !skos:narrower !"http://zbw.eu/stw/descriptor/12204-3" #xDE4454E> 
    #<TRIPLE !"http://zbw.eu/stw/thsys/70897" !skos:narrower !"http://zbw.eu/stw/descriptor/12210-1" #xDE444F6> 
    #<TRIPLE !"http://zbw.eu/stw/thsys/70897" !skos:narrower !"http://zbw.eu/stw/descriptor/18733-2" #xDE4449E> 
    #<TRIPLE !"http://zbw.eu/stw/thsys/70897" !skos:narrower !"http://zbw.eu/stw/descriptor/10210-4" #xDE44446> 
    #<TRIPLE !"http://zbw.eu/stw/thsys/70897" !skos:narrower !"http://zbw.eu/stw/descriptor/18683-5" #xDE443EE> 
    #<TRIPLE !"http://zbw.eu/stw/thsys/70897" !skos:narrower !"http://zbw.eu/stw/descriptor/12212-4" #xDE44396> 
    #<TRIPLE !"http://zbw.eu/stw/thsys/70897" !skos:narrower !"http://zbw.eu/stw/descriptor/18679-3" #xDE4433E> 
    #<TRIPLE !"http://zbw.eu/stw/thsys/70897" !rdfs:label #"Kapitalmarkttheorie"@@de #xDB6387E> 
    #<TRIPLE !"http://zbw.eu/stw/thsys/70897" !rdfs:label #"Financial Economics"@@en #xDB63826> 
    #<TRIPLE !"http://zbw.eu/stw/thsys/70897" !skos:notation #"W.14.03.02"^^<xsd:string> #xD98D9D6> 
    #<TRIPLE !"http://zbw.eu/stw/thsys/70897" !skos:broader !"http://zbw.eu/stw/thsys/70900" #xD526B7E> 
    #<TRIPLE !"http://zbw.eu/stw/thsys/70897" !skos:inScheme !"http://zbw.eu/stw" #xD03DFEE> 
    NIL
    ? (rdf:query (wilbur-mediator)
               :subject (rdf:subject (first (rdf:query (wilbur-mediator) :predicate '{rdf}type :object '{zbw}Descriptor :limit 1)))
               :continuation 'print)
    #<TRIPLE !"http://zbw.eu/stw/descriptor/14453-0" !rdf:type !"http://zbw.eu/namespaces/zbw-extensions/Descriptor" #xE03B7CE> 
    #<TRIPLE !"http://zbw.eu/stw/descriptor/14453-0" !skos:altLabel #"Aerotrain"@@en #xDFB3B56> 
    #<TRIPLE !"http://zbw.eu/stw/descriptor/14453-0" !skos:altLabel #"Hovercraft"@@en #xDFB3AFE> 
    #<TRIPLE !"http://zbw.eu/stw/descriptor/14453-0" !"http://purl.org/ontology/gbv/gvkppn" #"091375819"^^<xsd:string> #xDD7153E> 
    #<TRIPLE !"http://zbw.eu/stw/descriptor/14453-0" !skos:prefLabel #"Luftkissenfahrzeug"@@de #xDABFD16> 
    #<TRIPLE !"http://zbw.eu/stw/descriptor/14453-0" !skos:prefLabel #"Air cushion vehicle"@@en #xDABFCBE> 
    #<TRIPLE !"http://zbw.eu/stw/descriptor/14453-0" !skos:inScheme !"http://zbw.eu/stw" #xD894186> 
    #<TRIPLE !"http://zbw.eu/stw/descriptor/14453-0" !skos:broader !"http://zbw.eu/stw/descriptor/13513-6" #xD7C6E6E> 
    #<TRIPLE !"http://zbw.eu/stw/descriptor/14453-0" !skos:broader !"http://zbw.eu/stw/thsys/70257" #xD7C6E16> 
    #<TRIPLE !"http://zbw.eu/stw/descriptor/14453-0" !skos:related !"http://zbw.eu/stw/descriptor/14463-4" #xD6D0486> 
    NIL
    ? 

 It is possible to project a single instance given its identifier and to verify its properties similarly to the content of the repository

    ? (defparameter *thsys-70897* (rdf:project-graph (wilbur-mediator) (rdf:ensure-instance '{zbw}Thsys '{http://zbw.eu/stw/thsys/}70897)))
    *THSYS-70582*
    ? (rdf:project-graph *thsys-70897* #'print)
    #S(QUAD :SUBJECT {thsys}70897 :PREDICATE {rdf}label :OBJECT "Financial Economics" :CONTEXT NIL) 
    #S(QUAD :SUBJECT {thsys}70897 :PREDICATE {rdf}label :OBJECT "Kapitalmarkttheorie" :CONTEXT NIL) 
    #S(QUAD :SUBJECT {thsys}70897 :PREDICATE {skos}inScheme :OBJECT #<ConceptScheme |http://zbw.eu/|::|stw| #xEA30D96> :CONTEXT NIL) 
    #S(QUAD :SUBJECT {thsys}70897 :PREDICATE {skos}prefLabel :OBJECT "W.14.03.02  Financial Economics" :CONTEXT NIL) 
    #S(QUAD :SUBJECT {thsys}70897 :PREDICATE {skos}prefLabel :OBJECT "W.14.03.02  Kapitalmarkttheorie" :CONTEXT NIL) 
    #S(QUAD :SUBJECT {thsys}70897 :PREDICATE {skos}broader :OBJECT #<Thsys |http://zbw.eu/stw/thsys/|::|70900| #xEE7BA7E> :CONTEXT NIL) 
    #S(QUAD :SUBJECT {thsys}70897 :PREDICATE {skos}notation :OBJECT "W.14.03.02" :CONTEXT NIL) 
    #S(QUAD :SUBJECT {thsys}70897 :PREDICATE {skos}narrower :OBJECT #<Descriptor {descriptor}18679-3 #xEE7B83E> :CONTEXT NIL) 
    #S(QUAD :SUBJECT {thsys}70897 :PREDICATE {skos}narrower :OBJECT #<Descriptor {descriptor}12212-4 #xEE7B72E> :CONTEXT NIL) 
    #S(QUAD :SUBJECT {thsys}70897 :PREDICATE {skos}narrower :OBJECT #<Descriptor {descriptor}18683-5 #xEE7B61E> :CONTEXT NIL) 
    #S(QUAD :SUBJECT {thsys}70897 :PREDICATE {skos}narrower :OBJECT #<Descriptor {descriptor}10210-4 #xEE7B50E> :CONTEXT NIL) 
    #S(QUAD :SUBJECT {thsys}70897 :PREDICATE {skos}narrower :OBJECT #<Descriptor {descriptor}18733-2 #xEE7B3FE> :CONTEXT NIL) 
    #S(QUAD :SUBJECT {thsys}70897 :PREDICATE {skos}narrower :OBJECT #<Descriptor {descriptor}12210-1 #xEE7B2EE> :CONTEXT NIL) 
    #S(QUAD :SUBJECT {thsys}70897 :PREDICATE {skos}narrower :OBJECT #<Descriptor {descriptor}12204-3 #xEE7B1D6> :CONTEXT NIL) 
    #S(QUAD :SUBJECT {thsys}70897 :PREDICATE {skos}narrower :OBJECT #<Descriptor {descriptor}19271-2 #xEE7B0C6> :CONTEXT NIL) 
    #S(QUAD :SUBJECT {thsys}70897 :PREDICATE {skos}narrower :OBJECT #<Descriptor {descriptor}19196-4 #xEE7AFB6> :CONTEXT NIL) 
    #S(QUAD :SUBJECT {thsys}70897 :PREDICATE {skos}narrower :OBJECT #<Descriptor {descriptor}19537-4 #xEE7AEA6> :CONTEXT NIL) 
    #S(QUAD :SUBJECT {thsys}70897 :PREDICATE {skos}narrower :OBJECT #<Descriptor {descriptor}19269-3 #xEE7ABAE> :CONTEXT NIL) 
    #S(QUAD :SUBJECT {thsys}70897 :PREDICATE {skos}narrower :OBJECT #<Descriptor {descriptor}18138-2 #xEE7A8CE> :CONTEXT NIL) 
    #S(QUAD :SUBJECT {thsys}70897 :PREDICATE {rdf}type :OBJECT #<Class |http://zbw.eu/namespaces/zbw-extensions/|::|Thsys| #xEB792D6> :CONTEXT NIL) 

 To model the graph, project the classes from the repository into their CLOS classes

    ? (defparameter *t* (rdf:project-graph (rdf:wilbur-mediator) '{zbw}Thsys))
    *t*
    ? (defparameter *d* (rdf:project-graph (rdf:wilbur-mediator) '{zbw}Descriptor))
    *d*

 For mcl5.2 on a g5-2x1.8 this takes about 40ms per instance.
 Next in prepration for visualization, order the concepts most-general-first, to promote a coherent visual arrangement

    ? (defun concept-is-broader (c1 c2)
          (flet ((test-broader (ci)
                   (concept-is-broader c1 ci)))
            (declare (dynamic-extent #'test-broader))
            (let ((broader (concept-broader c2)))
              (typecase broader
                (null nil)
                (cons (or (find c1 broader) (some #'test-broader broader)))))))
    CONCEPT-IS-BROADER
    ? (defun concept-terms (concept)
      (flet ((as-list (x) (typecase x (list x) (t (list x)))))
        (append (as-list (concept-label concept))
                (as-list (pref-label concept))
                (as-list (alt-label concept)))))
    CONCEPT-TERMS
    ? (defparameter *sorted-concepts* (sort (copy-list (append *t* *d*)) #'concept-is-broader))
    *SORTED-CONCEPTS*
    ? (length *sorted-concepts*)
    6524
    ? (reduce #'+ *sorted-concepts* :key #'(lambda (c) (length (concept-terms c))))
    32011
    ? (let ((terms (make-hash-table :test #'equal)))
        (dolist (concept *sorted-concepts*)
          (dolist (term (concept-terms concept)) (setf (gethash term terms) t)))
        (hash-table-count terms))
    31425


## Rendering concept relations as a graph

 First a few operators to make sense out of the various available labels for a given term, and to make the terms available
 as selection criteria.

    ? (defun stw-label (concept)
      (flet ((maybe (label)
               (unless (search "descriptor" label :test #'char-equal)
                 (return-from stw-label label))))
        (map-collection #'maybe (concept-label concept))
        (map-collection #'maybe (pref-label concept))
        (map-collection #'maybe (alt-label concept))))
    STW-LABEL
    ? (defun topical-label (concept topic)
      (flet ((maybe (label)
                    (when (search topic label :test #'char-equal)
                      (return-from topical-label label))))
        (and topic
             (map-collection #'maybe (concept-label concept))
             (map-collection #'maybe (pref-label concept))
             (map-collection #'maybe (alt-label concept)))))
    TOPICAL-LABEL
    ? (defun graph-stw (concepts pathname
                        &key (size "11,11") (rankdir "LR") (related nil) (topic nil) (depth 1))
        (let ((r-r (make-hash-table :test #'equal))
              (b-n (make-hash-table :test #'equal))
              (node-count 0)
              (edge-count 0)
              (nodes (make-hash-table ))
              (color.normal "#000000")
              (color.topical "#8b0000")
              (color.off-topic "#8b8989")
              (color.narrower "#101010")
              (color.broader "red")
              (color.related "#555555"))
          (labels ((put-node (concept &key (color color.normal) label (fontcolor color.normal))
                     ;; emit just the node itself and register it
                     (setf (gethash concept nodes) t)
                     (dot:put-eol)
                     (dot:put-node concept :label label :color color :fontcolor fontcolor)
                     (incf node-count))
                   (put-edges (concept &key (color.broader color.broader) (color.narrower color.narrower))
                     (dolist (narrower (concept-narrower concept))
                       ;; double-check for duplicate entries
                       (unless (gethash (cons concept narrower) b-n)
                         (dot:put-edge concept narrower :color color.narrower)
                         (incf edge-count)
                         (setf (gethash (cons concept narrower) b-n) t)))
                     ;; double check the broader concepts - they should already have been emitted
                     (dolist (broader (concept-broader concept))
                       (unless (gethash (cons broader concept) b-n)
                         (dot:put-edge broader concept :color color.broader :dir "back")
                         (incf edge-count)))
                     (when related 
                       (dolist (other (concept-related concept))
                         (unless (gethash (cons other concept) r-r)
                           (setf (gethash (cons concept other) r-r) t)))))
                   (put-related-edges (pair ignore)
                     ;; after generating the concept hierarchy, add edges for 'related' concepts
                     ;; and ensure that the respective nodes appear with labels
                     (declare (ignore ignore))
                     (destructuring-bind (n1 . n2) pair
                       (flet ((put-if-necessary (concept)
                                (unless (gethash concept nodes)
                                  (put-node concept :color color.related :label (stw-label concept))
                                  (setf (gethash concept nodes) t))))
                         (put-if-necessary n1)
                         (put-if-necessary n2))
                       (dot:put-edge n1 n2 :color color.related :dir "none")))
                   (graph-concept (concept &key (connected nil) &aux topical-label)
                     ;; emit the graph from a concept node. topicality determined a label and color and
                     ;; also which edges to emit.
                     (unless (gethash concept nodes)
                       (setf (gethash concept nodes) t)
                       (cond ((null topic)
                              (put-node concept :label (stw-label concept))
                              (put-edges concept))
                             ((setf topical-label (topical-label concept topic))
                              (put-node concept :label topical-label :color color.topical)
                              (put-edges concept)
                              (dolist (narrower (concept-narrower concept))
                                (graph-concept narrower :connected depth))
                              (dolist (broader (concept-broader concept))
                                (graph-concept broader :connected depth)))
                             (connected
                              (put-node concept :label (stw-label concept) :color color.off-topic
                                        :fontcolor (if (eql connected depth) color.normal color.off-topic))
                              (when (plusp (decf connected))
                                (put-edges concept :color.narrower "#999999"  :color.broader "#999999")
                                (dolist (narrower (concept-narrower concept))
                                  (graph-concept narrower :connected connected))
                                (dolist (broader (concept-broader concept))
                                  (graph-concept broader :connected connected))))))))
            
            (dot:context-put-graph pathname (pathname-name pathname)
                                   #'(lambda ()
                                       (mapc #'graph-concept concepts)
                                       (maphash #'put-related-edges r-r))
                                   :rankdir rankdir
                                   :size size
                                   ;; :overlap "scale"
                                   :ranksep 4))
          (values node-count edge-count)))
    GRAPH-STW
    ?

 A complete graph yields a nede/edge count of 6524/29720 if the 'related concept' edges are includes, and
 6524/19216 if they are not.    

    ?  (graph-stw (append *t* *d*) #P"LIBRARY:examples;data;stw-0r.dot" :size "100,100" :related t)
    6524
    29720
    ?  (graph-stw (append *t* *d*) #P"LIBRARY:examples;data;stw-0.dot" :size "100,100")
    6524
    19216
    ?

 If related links are included, and 31782 edges if they are not.
 `dot` exhausts memory for that graph and while `twopi` does produce output, neither svg nor pdf viewers could render it.

       $ dot -Tpdf -o stw-dot-0r.pdf -Gcharset=latin1 stw-0r.dot 
       dot(7729) malloc: *** vm_allocate(size=1069056) failed (error code=3)
       dot(7729) malloc: *** error: can't allocate region
       dot(7729) malloc: *** set a breakpoint in szone_error to debug
       out of memory
       Abort trap
       $

 A limited graph, one which restricts the view to terms related to politics, is more successful.
  
    ? (graph-stw *sorted-concepts* #P"LIBRARY:examples;data;stw-2.dot" :size "100,100" :topic "oliti" :depth 2)

 The result renders fairly well on opera and firefox, but exceeds safari's ability to scale. 

<div style='text-align: center'><a href='./stw.svg'><img src='http://github.com/lisp/de.setf.resource/raw/master/examples/stw-3x3.jpg' width='64' height='64'/></a></div>



----
 Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved
 
 This file is part of `de.setf.resource examples`, which is distributed under a
 [Creative Commons Attribution 3.0 Germany License](http://creativecommons.org/licenses/by/3.0/de)
 These examples are is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 See the Creative Commons License for more details.
 A copy of the Creative Commons License is available from the CC
 [site](http://creativecommons.org/about/licenses/).