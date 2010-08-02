;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.resource.implementation; -*-


;;; the kennedy clan courtesy of franz.com
;;; see [http://www.franz.com/agraph/support/learning/The-Kennedy-Family.lhtml]

(defpackage "http://www.franz.com/simple#" (:use) (:nicknames "simple"))

(rdf:defaccessor person-first-name (person) :property '{simple}first-name)
(rdf:defaccessor person-last-name (person) :property '{simple}last-name)
(rdf:defaccessor person-spouse (person) :property '{simple}spouse :type (cons {simple}person))
(rdf:defaccessor person-sex (person) :property '{simple}sex)
(rdf:defaccessor person-children (person) :property '{simple}has-child :type (cons {simple}person))
  
(rdf:load-repository (wilbur-mediator) #P"LIBRARY:examples;data;kennedy.ntriples")

(defparameter *k* (rdf:project-graph (rdf:wilbur-mediator) 't))

(remove-duplicates (mapcar #'type-of *k*))

(let ((people (remove-if-not #'(lambda (o) (typep o '{simple}person)) *k*))
      (spouses ()))
  (flet ((graph-person (person)
           (let ((name (format nil "~@[~a~] ~@[~a~]"
                               (person-first-name person) (person-last-name person))))
             (dot:put-node person :label name)
             (rdf:do-collection (s (person-spouse person))
               (unless (find s spouses)
                 (push s spouses)
                 (dot:put-edge person s :label "spouse")))
             (rdf:do-collection (c (person-children person))
               (dot:put-edge person c :label "child")))))
    (dot:context-put-graph #P"LIBRARY:examples;data;kennedy.dot" "kennedy"
                           #'(lambda () (dolist (p people) (graph-person p)))
                           :rankdir "LR")))

