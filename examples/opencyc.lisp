;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.resource.implementation; -*-


;;; the kennedy clan courtesy of franz.com
;;; see [http://www.franz.com/agraph/support/learning/The-Kennedy-Family.lhtml]

(defpackage "http://www.franz.com/simple#" (:use) (:nicknames "simple"))

(de.setf.rdf:defaccessor person-first-name (person) :property '{simple}first-name)
(de.setf.rdf:defaccessor person-last-name (person) :property '{simple}last-name)
(de.setf.rdf:defaccessor person-spouse (person) :property '{simple}spouse :type (cons {simple}person))
(de.setf.rdf:defaccessor person-sex (person) :property '{simple}sex)
(de.setf.rdf:defaccessor person-children (person) :property '{simple}has-child :type (cons {simple}person))

;;; (de.setf.rdf:repository-clear (wilbur-mediator))
;;; (xmlp:document-parser #P"LIBRARY:examples;data;opencyc-latest.owl")
;;; mcl5.2/g5x1.8 = +/- 30 min, 14,740,383 elements and about 400MB

(de.setf.rdf:load-repository (wilbur-mediator) #P"LIBRARY:examples;data;opencyc-latest.owl")

(defparameter *k* (de.setf.rdf:project-graph (de.setf.rdf:wilbur-mediator) 't))

(remove-duplicates (mapcar #'type-of *k*))

(let ((people (remove-if-not #'(lambda (o) (typep o '{simple}person)) *k*))
      (spouses ()))
  (flet ((graph-person (person)
           (let ((name (format nil "~@[~a~] ~@[~a~]"
                               (person-first-name person) (person-last-name person))))
             (dot:put-node person :label name)
             (de.setf.rdf:do-collection (s (person-spouse person))
               (unless (find s spouses)
                 (push s spouses)
                 (dot:put-edge person s :label "spouse")))
             (de.setf.rdf:do-collection (c (person-children person))
               (dot:put-edge person c :label "child")))))
    (dot:context-put-graph #P"LIBRARY:examples;data;kennedy.dot" "kennedy"
                           #'(lambda () (dolist (p people) (graph-person p)))
                           :rankdir "LR")))

