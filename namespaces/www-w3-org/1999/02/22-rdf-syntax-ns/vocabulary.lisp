;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: common-lisp-user; -*-
;;; 20100513T131613Z00
;;; from #<doc-node http://www.w3.org/1999/02/22-rdf-syntax-ns #x26B2EE16>

(in-package :common-lisp-user)

(defpackage "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  (:use )
  (:nicknames "rdf")
  (:export "Alt" "Bag" "first" "List" "object" "predicate" "Property" "rest"
           "Seq" "Statement" "subject" "type" "value"))

(in-package "http://www.w3.org/1999/02/22-rdf-syntax-ns#")

(rdfs:defvocabulary "rdf"
  :uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  :package "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  :definitions
  ((de.setf.resource.schema:defclass |Alt|
     (|http://www.w3.org/2000/01/rdf-schema#|:|Container|)
     nil)
   
   (de.setf.resource.schema:defclass |Bag|
     (|http://www.w3.org/2000/01/rdf-schema#|:|Container|)
     nil)
   
   (de.setf.resource.schema:defclass |List|
     (|http://www.w3.org/2000/01/rdf-schema#|:|Resource|)
     ((|first| :type
               |http://www.w3.org/2000/01/rdf-schema#|:|Resource|)
      (|rest| :type |List|)))
   
   (de.setf.resource.schema:defclass |Property|
     (|http://www.w3.org/2000/01/rdf-schema#|:|Resource|)
     nil)
   
   (de.setf.resource.schema:defclass |Seq|
     (|http://www.w3.org/2000/01/rdf-schema#|:|Container|)
     nil)
   
   (de.setf.resource.schema:defclass |Statement|
     (|http://www.w3.org/2000/01/rdf-schema#|:|Resource|)
     ((|subject| :type
                 |http://www.w3.org/2000/01/rdf-schema#|:|Resource|)
      (|predicate| :type
                   |http://www.w3.org/2000/01/rdf-schema#|:|Resource|)
      (|object| :type
                |http://www.w3.org/2000/01/rdf-schema#|:|Resource|)))))

;;; (rdfs:require "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
