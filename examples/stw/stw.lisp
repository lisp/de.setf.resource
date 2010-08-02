;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.resource.implementation; -*-

(in-package :de.setf.resource.implementation)


(:documentation "This file uses the resource library to visualize STW concept networks."

  (copyright
   "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
   "`de.setf.resource` is free software: you can redistribute it and/or modify
 it under the terms of version 3 of the GNU Lesser General Public License as published by
 the Free Software Foundation.
 
 This file is part of `de.setf.resource examples`, which is distributed under a
 [Creative Commons Attribution 3.0 Germany License](http://creativecommons.org/licenses/by/3.0/de)

 These examples are is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 See the Creative Commons License for more details.

 A copy of the Creative Commons License is available from the CC
 [site](http://creativecommons.org/about/licenses/).")

  (description
   "The 'STW Thesaurus for Economics'[1][2] serves the German National Library of Economics (ZBW)[3]
 as the vocabulary for thematic access to their collection. The ZBW publishes it as RDF using an extension
 to the SKOS[4][5] ontology. As of version 8.06, the documentation suggests about 6,000 headings and 18,000
 entry terms. The data itself indicates 6,524 nodes and 31,425 distinct related labels.

 This example demonstrates how to use the resource library together with an RDF repository to render concept
 networks as visual graphs. It starts with the RDF/XML version of the document[6] which is available from
 the ZBW web site.

 [1]: http://zbw.eu/stw/versions/latest/about.en.html
 [2]: http://www.w3.org/2001/sw/sweo/public/UseCases/ZBW/
 [3]: http://www.zbw.eu/
 [4]: http://www.w3.org/TR/skos-reference/
 [5]: http://en.wikipedia.org/wiki/Simple_Knowledge_Organization_System
 [6]: http://zbw.eu/stw/versions/latest/download/about"))


(:documentation
  "The first step is to load the the ntriple version of the STW.
 As the original is fourteen megabytes of RDF/XML, there is some advantage to recoding it as ntriples,
 even just for the convenience of grepping[7] through it to guage node names, the class mix, or
 the language variations. The raptor[8] tools recode the the document as ntriples with the command

    rapper stw.rdf > stw.nt

 Given which, it is a simple operation to load it into the rdf repository. Should it be necessary to first
 clear the repository:

    (rdf:repository-clear (wilbur-mediator))

 On a vintage G5-2x1.8 with mcl, the process takes about two minutes, yields about 114K nodes and uses
 about 150 megabutes.

 [7]: http://blog.datagraph.org/2010/03/grepping-ntriples)
 [8]: http://librdf.org/raptor/")


(rdf:load-repository (wilbur-mediator) #P"LIBRARY:examples;data;stw.nt")


(:documentation
 "The next step is to construct an interface to the data. If the process were driven by application
 requirements, a data model could evolve from process and presentation requirements, to take the
 form of apriori class definitions. In this case, the task is to render as a graph the concept network
 implicit in the relations among the thesaurus' terms.

 In this respect, we observe one of the predicaments of linked data processing: on one hand, one would like
 to avoid a closed model, as that would preclude access to data beyond its purview, but on the other
 the mechanisms to be applied in order to infer a data model, or to adapt or extend a core, are
 frequently ineffective, as the data itself can be incomplete, inconsistent, or even just exhibit sufficient
 variations as to encumber comprehension.

 This task exhibits two such incidental artifacts:

 - the correlation between vocabulary names and resources may be such that it is not obvious how to
 derive the schema.
 - a schema may simply be incomplete
 
 The first issue manifests in the identifers associated with the ZBW schemas. The ontology, per se is identified
 as `http://zbw.eu/stw/`. To wir:

    $ fgrep 'http://www.w3.org/2002/07/owl#Ontology'  stw.nt 
    <http://zbw.eu/stw/> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#Ontology> .
    <http://www.w3.org/2004/02/skos/core> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#Ontology> .

 Yet, in the event, the data indicates that it is defined by another resource

    $ fgrep DefinedBy stw.nt | fgrep zbw
    <http://zbw.eu/namespaces/zbw-extensions/Thsys> <http://www.w3.org/2000/01/rdf-schema#isDefinedBy> <http://zbw.eu/namespaces/zbw-extensions/zbw-extensions.rdf> .
    <http://zbw.eu/namespaces/zbw-extensions/indexedItem> <http://www.w3.org/2000/01/rdf-schema#isDefinedBy> <http://zbw.eu/namespaces/zbw-extensions/zbw-extensions.rdf> .
    <http://zbw.eu/namespaces/zbw-extensions/useInsteadNote> <http://www.w3.org/2000/01/rdf-schema#isDefinedBy> <http://zbw.eu/namespaces/zbw-extensions/zbw-extensions.rdf> .
    <http://zbw.eu/namespaces/zbw-extensions/Descriptor> <http://www.w3.org/2000/01/rdf-schema#isDefinedBy> <http://zbw.eu/namespaces/zbw-extensions/zbw-extensions.rdf> .

 which is of limited use. as neither the terms nore the refinition resource are known in advance, this could be addressed
 only by a before-and-after set difference of `isDefinedBy` statements. There must be an ontology vocabulary namespace
 specification somewhere, but it's not ovious. An attempt to determine it from the actual resources
 is also confounded, as a schema retrieved from the abstract vocabulary namespace resource, `http://zbw.eu/namespaces/zbw-extensions/`,
 indicates yet another incidental
 source -- `http://2007.zbw.eu/namespaces/zbw-extensions/zbw-extensions.rdf`, about which the data revals nothing.
 even `fgrep 'zbw-extensions/>' stw.nt` reveals nothing beyond publication information about some _thing_.
 
 As a consequence, the knowledge gleaned from examining the data as text must be incorporated into the vocabulary
 definition explicitly.")
 
;;; purge it by setting it to nil
;;; (setf (rdf:find-vocabulary (wilbur-mediator) "http://zbw.eu/namespaces/zbw-extensions/") nil)

(rdf:ensure-vocabulary (wilbur-mediator) "http://zbw.eu/namespaces/zbw-extensions/"
                       :resource-uri "http://zbw.eu/namespaces/zbw-extensions/zbw-extensions.rdf")

;;; for convenience alias the package

(rename-package "http://zbw.eu/namespaces/zbw-extensions/"
                "http://zbw.eu/namespaces/zbw-extensions/" '("zbw" "stw"))


(rename-package "http://www.w3.org/2004/02/skos/core#"
                "http://www.w3.org/2004/02/skos/core#" '("skos"))

(vocabulary-definitions (rdf:find-vocabulary (wilbur-mediator)
                                             "http://zbw.eu/namespaces/zbw-extensions/"))

(:documentation
  "That is, two classes are associated with the data:

    ((rdf:defclass {zbw}Thsys ({skos}Concept)
       ()
       (:datatype {zbw}Thsys))
     (rdf:defclass {zbw}Descriptor ({skos}Concept)
       ()
       (:datatype {zbw}Descriptor)))

(respository-schema-types (wilbur-mediator) '{zbw}zbw-extensions.rdf)


;; (|http://zbw.eu/namespaces/zbw-extensions/|::|Descriptor| |http://zbw.eu/namespaces/zbw-extensions/|::|Thsys|)

(c2mop:finalize-inheritance (rdf:find-class (wilbur-mediator) '{zbw}Descriptor))
(mapcar #'c2mop:slot-definition-name
        (remove-if-not #'(lambda (sd) (typep sd 'rdf-effective-property-definition))
                       (c2mop:class-slots (find-class '{zbw}Descriptor))))
;;; (|http://www.w3.org/2004/02/skos/core#|::|topConceptOf| |http://www.w3.org/2004/02/skos/core#|::|semanticRelation|)

(mapcar #'class-name (c2mop:class-precedence-list (find-class '{zbw}Descriptor)))
;;; (|http://zbw.eu/namespaces/zbw-extensions/|::|Descriptor| |http://www.w3.org/2004/02/skos/core#|::|Concept| RESOURCE-OBJECT STANDARD-OBJECT T)

(c2mop:finalize-inheritance (rdf:find-class (wilbur-mediator) '{zbw}Thsys))
(mapcar #'c2mop:slot-definition-name
        (remove-if-not #'(lambda (sd) (typep sd 'rdf-effective-property-definition))
                       (c2mop:class-slots (find-class '{zbw}Thsys))))
(mapcar #'class-name (c2mop:class-precedence-list (find-class '{zbw}Thsys)))


;;;
;;; taking an example

(rdf:defaccessor concept-narrower (concept) :property {skos}narrower)
(rdf:defaccessor concept-broader (concept) :property {skos}narrower)
(rdf:defaccessor concept-related (concept) :property {skos}related)

(rdf:defaccessor concept-label (concept) :property {rdfs}label)
(rdf:defaccessor pref-label (concept) :property {skos}prefLabel)
(rdf:defaccessor alt-label (concept) :property {skos}altLabel)

(rdf:query (wilbur-mediator) :subject !"http://zbw.eu/stw/thsys/a")
(rdf:query (wilbur-mediator) :subject '{http://zbw.eu/stw/thsys/}70582 :continuation 'print)
(rdf:query (wilbur-mediator) :subject '{http://zbw.eu/stw/descriptor/}137990 :continuation 'print)

;;; a single instance 
(defparameter *thsys* (rdf:ensure-instance '{zbw}Thsys '{http://zbw.eu/stw/thsys/}70582))
(rdf:project-graph (rdf:project-graph (wilbur-mediator) *thsys*) 'print)
(mapcar #'concept-label (concept-narrower *thsys*))     ; needs to complete the protocol to read on demand

;;; mcl5.2/g5x1.8 == +/- 250/s
(time (defparameter *t* (rdf:project-graph (rdf:wilbur-mediator) '{zbw}Thsys)))
(time (defparameter *d* (rdf:project-graph (rdf:wilbur-mediator) '{zbw}Descriptor)))

(every #'concept-label *t*)
(inspect (find-if-not #'stw-label  *d*))
(inspect (first *d*))


(defun stw-label (concept)
  (flet ((maybe (label)
           (unless (search "descriptor" label :test #'char-equal)
             (return-from stw-label label))))
    (map-collection #'maybe (concept-label concept))
    (map-collection #'maybe (pref-label concept))
    (map-collection #'maybe (alt-label concept))))

(defun concept-terms (concept)
  (flet ((as-list (x) (typecase x (list x) (t (list x)))))
    (append (as-list (concept-label concept))
            (as-list (pref-label concept))
            (as-list (alt-label concept)))))

(defun topical-label (concept topic)
  (flet ((maybe (label)
                (when (search topic label :test #'char-equal)
                  (return-from topical-label label))))
    (and topic
         (map-collection #'maybe (concept-label concept))
         (map-collection #'maybe (pref-label concept))
         (map-collection #'maybe (alt-label concept)))))

(defun concept-is-broader (c1 c2)
  (flet ((test-broader (ci)
           (concept-is-broader c1 ci)))
    (declare (dynamic-extent #'test-broader))
    (let ((broader (concept-broader c2)))
      (typecase broader
        (null nil)
        (cons (or (find c1 broader) (some #'test-broader broader)))
        (t (eq c1 broader))))))

(defparameter *sorted-concepts* (sort (copy-list (append *t* *d*)) #'concept-is-broader))
(length *sorted-concepts*)
(reduce #'+ *sorted-concepts* :key #'(lambda (c) (length (concept-terms c))))
(let ((terms (make-hash-table :test #'equal)))
  (dolist (concept *sorted-concepts*)
    (dolist (term (concept-terms concept)) (setf (gethash term terms) t)))
  (hash-table-count terms))

                

(defun graph-stw (concepts pathname
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

;; the nodes/edges work out to 6524/42286 if related's are included, and 31782 edges if they are not
;; the former does not even appear in many viewers, the latter is still illegible.
  
(graph-stw (append *t* *d*) #P"LIBRARY:examples;data;stw.dot" :size "100,100") ; :related t)

;; limiting the topic improves the result
;; "politik" : 294/2450
(graph-stw *sorted-concepts* #P"LIBRARY:examples;data;stw.dot" :size "100,100" :topic "oliti" :depth 2)

;;;
;;; 


(labels ((is-clean (s)
           (typecase s
             (wilbur:triple (and (is-clean (wilbur:triple-subject s))
                                 (is-clean (wilbur:triple-predicate s))
                                 (is-clean (wilbur:triple-object s))))
             (wilbur:node t)
             (wilbur:literal t))))
  (find-if-not #'is-clean (wilbur:db-triples wilbur::*db*)))