    ;;; -*- package: wilbur; -*-

+ notes about wilbur and the rdf development environment

[1] : wilbur-rdf.sourceforge.net

- node duality as both rdf graph nodes and rdf graph arcs.

+ slot specification can be apath expression, not just a slot name.
? does the path grammar permit queris at the nodes?

if the frame has own-slots why not reify it as a class?

    (setq *db* (make-instance 'db))

    (db-load *db* "http://datagraph.org/jhacker/foaf.rdf")
    (db-load *db* "file:///wilbur/examples/foaf.rdf")

    (wilbur:query !"http://datagraph.org/jhacker/#self" nil nil)
    (wilbur:query nil nil !"mailto:jhacker@example.org")

The core model components are the node, which is an uri, and the tuple, which is the
subject, predicate, object triple. Triples are actually quads in that they also bind a list(?)
of sources.

Literals are also objects which bind the string representation, the data type and language, as
well as an interned value.
A weak eq hash table would be better suited for the annotations.
The literal reader syntax uses #" even though the value is not a fragment.
perhaps the paradigm is "anonymous fragment"

    (wilbur:namespaces )
    (wilbur:dictionary-namespaces *nodes*)
    (wilbur:find-long-name *nodes* "rdf:type")
    (eq !"http://www.w3.org/1999/02/22-rdf-syntax-ns#type" !rdf:type)

    (map nil #'print (db-triples *db*))
    (inspect *db*)
    (inspect (make-instance 'db))
    ;; file url fail due to logical host roblem
    (wilbur:db-load *db* "file:///wilbur/schemata/rdf-schema.rdf")
    ;; remote document succeeds
    (wilbur:db-load *db* "http://www.w3.org/TR/rdf-schema/rdfs-namespace")

    ;; foaf schema
    (wilbur:db-load *db* "http://xmlns.com/foaf/spec/20100101.rdf")

    (mapcar #'rdf:subject (wilbur:query nil !rdf:type nil))
    (wilbur:query !foaf:name !rdfs:range nil)
    (mapcar #'rdf:object (wilbur:query nil !rdfs:range nil))
    (wilbur:query !foaf:age nil  nil)


;;; working with schema sources
;;; schemacache

    (db-load *db* "http://vocab.org/changeset/schema.rdf#type")
    (wilbur:db-load *db* "http://xmlns.com/foaf/0.1/name")
    
    (mapcar #'(lambda (class-statement)
                (let ((subject (triple-subject class-statement)))
                  (list subject
                        (mapcar #'triple-subject (wilbur:query nil !rdfs:domain subject)))))
            (wilbur:query nil !rdf:type !owl:Class))
    (mapcar #'node-uri (mapcar #'triple-subject (wilbur:query nil !rdf:type !owl:Ontology)))
    (db-load *db* "http://web.nickshanks.com/ns/family")

;;; the kennedy clan courtesy of franz.com
    (rdf:load-store *db* #P"LIBRARY:examples;data;kennedy.ntriples")
    (defparameter *k* (rdf:project-graph (rdf:wilbur-mediator) 't))
    (rdf:defaccessor person-first-name :property '|http://www.franz.com/simple#|::|first-name|)
    (rdf:defaccessor person-last-name :property '|http://www.franz.com/simple#|::|last-name|)
    (rdf:defaccessor person-spouse :property '|http://www.franz.com/simple#|::|spouse|)
    (rdf:defaccessor person-sex :property '|http://www.franz.com/simple#|::|sex|)
    (rdf:defaccessor person-children :property '|http://www.franz.com/simple#|::|has-child|
      :type (cons |http://www.franz.com/simple#|::|person|))

    (flet ((filter (o)
             (labels ((first-name-if (o)
                      (typecase o
                        (cons (mapcar #'first-name-if o))
                        (rdf:resource-object (person-first-name o)))))
               (when (typep o '|http://www.franz.com/simple#|::|person|)
                 (list (list (person-first-name o) (person-last-name o))
                       (person-sex o)
                       (first-name-if (person-spouse o))
                       (mapcar #'person-first-name (person-children o)))))))
      (pprint (remove nil (mapcar #'filter *k*))))

(let ((people (remove-if-not #'(lambda (o) (typep o '|http://www.franz.com/simple#|::|person|)) *k*))
      (spouses ()))
  (dot:context-put-graph #P"LIBRARY:examples;data;kennedy.dot" "kennedy"
                         #'(lambda ()
                             (dolist (p people)
                               (let ((name (format nil "~@[~a~] ~@[~a~]"
                                                   (first (person-first-name p)) (first (person-last-name p)))))
                                 (dot:put-node p :label name)
                                 (rdf:do-collection (s (person-spouse p))
                                   (unless (find s spouses)
                                     (push s spouses)
                                     (dot:put-edge p s :label "spouse")))
                                 (rdf:do-collection (c (person-children p))
                                   (dot:put-edge p c :label "child")))))
                         :rankdir "LR"))

(dolist (p (remove-if-not #'(lambda (o) (typep o '|http://www.franz.com/simple#|::|person|)) *k*))
  (format *trace-output* "~%~a ~a" (rdf:uri p) (person-first-name p)))