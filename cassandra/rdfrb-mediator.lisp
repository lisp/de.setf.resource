;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.resource.implementation; -*-

(in-package :de.setf.resource.implementation)

(:documentation
  "This file mediates access to a cassandra repository with an rdf.rd schema the 'de.setf.resource' CLOS
 linked data library."
 
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
  "'de.setf.resource' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.resource' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.resource' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/)."))

(defvar +default-cache-graph+ (compute-spoc-sha1-hex-id ))


(defclass cassandra-rdfrb-index-mediator (cassandra-mediator)
  ((resources-family
    :reader repository-resources-family
    :documentation "a column family for the bas resource storage")
   (index-family
    :reader repository-index-family
    :documentation "a column family for the index")
   (cache-family
    :reader repository-cache-family
    :documentation "a cache column family.")
   (dsc:version-class-map
    :initform '(("2.1.0" . cassandra-rdfrb-index-mediator_2.1.0)
                ("8.3.0" . cassandra-rdfrb-index-mediator_8.3.0))
    :allocation :class))

  (:documentation "A keyspace of the form used by an RDF mediator."))


(defclass cassandra-rdfrb-index-mediator_2.1.0 (cassandra-rdfrb-index-mediator cassandra-mediator_2.1.0)
  ())


(defclass cassandra-rdfrb-index-mediator_8.3.0 (cassandra-rdfrb-index-mediator cassandra-mediator_8.3.0)
  ())

            
(defmethod dsc:keyspace-bind-columns ((instance cassandra-rdfrb-index-mediator)
                                      &key  (resources-family "Resources") (cache-family "Cache") (index-family "Index")
                                      &allow-other-keys)
  (dsc:set-keyspace-column-family instance resources-family 'resources-family :class 'dsc:super-column-family :required t)
  (dsc:set-keyspace-column-family instance index-family 'index-family :class 'dsc:super-column-family :required nil)
  (dsc:set-keyspace-column-family instance cache-family 'cache-family :class 'dsc:standard-column-family :required nil))



;;;
;;; manipulating rdf statements
;;; from repository.rb#insert_statements

(defmethod add-statement* ((mediator cassandra-rdfrb-index-mediator) subject predicate object context)
  (declare (ignore context))

  (assert (and subject predicate object) ()
          "All constituents are required.")
  
  (let* ((s-subject (repository-value mediator subject))
         (s-predicate (repository-value mediator predicate))
         (s-object (repository-value mediator object))
         (index-cf (repository-index-family mediator))
         (resources-cf (repository-resources-family mediator))
         (cache-cf (repository-cache-family mediator)))
    (flet ((index-predicate ()
             (let ((predicate-id (compute-spoc-sha1-hex-id s-predicate)))
               (dsc:set-attributes index-cf (list predicate-id :info) (compute-spoc-sha1-id s-predicate) s-predicate)
               (dsc:set-attributes index-cf (list predicate-id :ps) (compute-spoc-sha1-id s-subject) s-subject)))
           (index-object ()
             (let ((object-id (compute-spoc-sha1-hex-id s-object)))
               (dsc:set-attributes index-cf (list object-id :info) (compute-spoc-sha1-id s-object) s-object)
               (dsc:set-attributes index-cf (list object-id :os) (compute-spoc-sha1-id s-subject) s-subject)
               (dsc:set-attributes index-cf (list object-id :op) (compute-spoc-sha1-id s-predicate) s-predicate))))

      (when cache-cf
        (dsc:set-attribute cache-cf +default-cache-graph+
                           (compute-spoc-sha1-id (utf-8 (format nil "~/n3:format/"(list subject predicate object))) nil nil nil)
                           ""))
      (when index-cf
        (index-predicate)
        (index-object))
      (dsc:set-attribute resources-cf (list s-subject s-predicate) (compute-spoc-sha1-hex-id s-object) s-object))))



(defmethod map-statements* (continuation (mediator cassandra-rdfrb-index-mediator) subject predicate object context)
  ;; indices
  ;; spoc : ( [s.p.o.c] . statement-attribute* )
  ;; spo  : ( [s.p] . ( [o] . (c . spoc-id)* )* )
  ;; sp c : ( [s.p] . ( [c] . (o . spoc-id)* )* )
  (flet ((map-resource-family (op index subject-key predicate-key)
           ;; given both the subject and predicate, oterate over objects
           (if predicate-key
             (loop for column in (dsc:get-columns index (list subject-key predicate-key))
                   do (funcall op
                               (dsc:column-name column)
                               (model-value mediator (dsc:column-value column))))
             ;; given just the subject row key, iterate over all predicates
             (loop for ((nil supercolumn-key) . columns) in (dsc:get-columns index subject-key)
                   for value = (model-value mediator supercolumn-key)
                   do (loop for column in columns
                            do (funcall op
                                        value
                                        (dsc:column-name column)
                                        (model-value mediator (dsc:column-value column))))))))
    
    (handler-case 
      (spoc-case (mediator (s-subject s-predicate s-object s-context) subject predicate object nil)
        :spo-                           ; look for all equivalent statement 
        (let ((column (dsc:get-column (repository-resources-family mediator) (list s-subject s-predicate) (compute-spoc-sha1-hex-id s-object))))
          (when column
            (funcall continuation subject predicate object context (dsc:column-name column))))
        
        :sp--                           ; retrieve objects for subject and predicates
        (flet ((do-constituents (object-sha1 object)
                 (funcall continuation subject predicate object context object-sha1)))
          (declare (dynamic-extent #'do-constituents))
          (map-resource-family #'do-constituents (repository-resources-family mediator) s-subject s-predicate))
        
        :s-o-                           ; interate over subject row's supercolumns/columns filtering for object
        (flet ((do-constituents (predicate object-sha1 test-object)
                 (when (equal object test-object)
                   (funcall continuation subject predicate object context object-sha1))))
          (declare (dynamic-extent #'do-constituents))
          (map-resource-family #'do-constituents (repository-resources-family mediator) s-subject nil))
        
        :s---                           ;interate over subject row's supercolumns/columns
        (flet ((do-constituents (predicate object-sha1 object)
                 (funcall continuation subject predicate object context object-sha1)))
          (declare (dynamic-extent #'do-constituents))
          (map-resource-family #'do-constituents (repository-resources-family mediator) s-subject nil))
        
        :-po-                         ; retrieve p.o across contexts
        (flet ((do-key-slice (key-slice)
                 (let ((subject (model-value mediator (dsc:keyslice-key key-slice))))
                   (loop for cosc = (dsc:keyslice-columns key-slice)
                         for sc = (dsc:columnorsupercolumn-super-column cosc)
                         do (loop for column in (dsc:supercolumn-columns sc)
                                  when (equal object (model-value mediator (dsc:column-value column)))
                                  do (funcall continuation subject predicate object (dsc:column-value column)))))))
          (declare (dynamic-extent #'do-key-slice))
          (dsc:map-range-slices #'do-key-slice mediator :column-family (dsc:column-family-name (repository-resources-family mediator))
                                :start-key "" :finish-key "" :super-column s-predicate))
        
        :-p--                           ; iterate over all rows and filtered for predicate
        (flet ((do-key-slice (key-slice)
                 (let ((subject (model-value mediator (dsc:keyslice-key key-slice))))
                   (loop for cosc = (dsc:keyslice-columns key-slice)
                         for sc = (dsc:columnorsupercolumn-super-column cosc)
                         do (loop for column in (dsc:supercolumn-columns sc)
                                  do (funcall continuation subject predicate
                                              (model-value mediator (dsc:column-value column))
                                              (dsc:column-value column)))))))
          (declare (dynamic-extent #'do-key-slice))
          (dsc:map-range-slices #'do-key-slice mediator :column-family (dsc:column-family-name (repository-resources-family mediator))
                                           :start-key "" :finish-key "" :super-column s-predicate))
        
        :--o-                           ; retrieve all o across all contexts
        (flet ((do-key-slice (key-slice)
                 (let ((subject (model-value mediator (dsc:keyslice-key key-slice))))
                   (loop for cosc = (dsc:keyslice-columns key-slice)
                         for sc = (dsc:columnorsupercolumn-super-column cosc)
                         for predicate = (model-value mediator (dsc:supercolumn-name sc))
                         do (loop for column in (dsc:supercolumn-columns sc)
                                  when (equal object (model-value mediator (dsc:column-value column)))
                                  do (funcall continuation subject predicate object (dsc:column-value column)))))))
          (declare (dynamic-extent #'do-key-slice))
          (dsc:map-range-slices #'do-key-slice mediator :column-family (dsc:column-family-name (repository-resources-family mediator))
                                           :start-key "" :finish-key ""))
        
        :----                         ; retrieve all statements
        (flet ((do-key-slice (key-slice)
                 (let ((subject (model-value mediator (dsc:keyslice-key key-slice))))
                   (loop for cosc = (dsc:keyslice-columns key-slice)
                         for sc = (dsc:columnorsupercolumn-super-column cosc)
                         for predicate = (model-value mediator (dsc:supercolumn-name sc))
                         do (loop for column in (dsc:supercolumn-columns sc)
                                  do (funcall continuation subject predicate
                                              (model-value mediator (dsc:column-value column))
                                              (dsc:column-value column)))))))
          (declare (dynamic-extent #'do-key-slice))
          (dsc:map-range-slices #'do-key-slice mediator :column-family (dsc:column-family-name (repository-resources-family mediator))
                                           :start-key "" :finish-key "")))

      (cassandra_2.1.0:notfoundexception (c) (declare (ignore c)) nil))))


(defmethod delete-statement* ((mediator cassandra-rdfrb-index-mediator) subject predicate object context)
  (declare (ignore context))

  (assert (and subject predicate object) ()
          "All constituents are required.")
  
  (let* ((s-subject (repository-value mediator subject))
         (s-predicate (repository-value mediator predicate))
         (s-object (repository-value mediator object))
         (index-cf (repository-index-family mediator))
         (resources-cf (repository-resources-family mediator))
         (cache-cf (repository-cache-family mediator)))
    (flet ((unindex-predicate ()
             (let ((predicate-id (compute-spoc-sha1-hex-id s-predicate)))
               (dsc:set-attributes index-cf (list predicate-id :info) predicate-id nil)
               (dsc:set-attributes index-cf (list predicate-id :ps) (compute-spoc-sha1-id s-subject) nil)))
           (unindex-object ()
             (let ((object-id (compute-spoc-sha1-hex-id s-object)))
               (dsc:set-attributes index-cf (list object-id :info) (compute-spoc-sha1-id s-object) nil)
               (dsc:set-attributes index-cf (list object-id :os) (compute-spoc-sha1-id s-subject) nil)
               (dsc:set-attributes index-cf (list object-id :op) (compute-spoc-sha1-id s-predicate) nil))))

      (when cache-cf
        (dsc:set-attribute cache-cf +default-cache-graph+
                           (compute-spoc-sha1-id (utf-8 (format nil "~/n3:format/"(list subject predicate object))) nil nil nil)
                           nil))
      (when index-cf
        (unindex-predicate)
        (unindex-object))
      (dsc:set-attribute resources-cf (list s-subject s-predicate) (compute-spoc-sha1-hex-id s-object) nil))))



#|

(defparameter *c-location*
  ;; remote
  ;; #u"thrift://ec2-174-129-66-148.compute-1.amazonaws.com:9160"
  ;; local
  #u"thrift://127.0.0.1:9160"
  "A cassandra service location - either the local one or a remote service 
 - always a 'thrift' uri.")

(defparameter *rdfrb* (client *c-location* :name "RDF" :protocol 'cassandra-rdfrb-index-mediator))

(add-statement.rb *rdfrb* "http://rdf.rubyforge.org/" "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" "http://usefulinc.com/ns/doap#Project" "2010-07-20")
(add-statement.rb *rdfrb* "http://rdf.rubyforge.org/" "http://usefulinc.com/ns/doap#developer" "http://ar.to/#self" "2010-07-30")
(add-statement.rb *rdfrb* "http://rdf.rubyforge.org/" "http://usefulinc.com/ns/doap#developer" "http://bhuga.net/#ben" "2010-07-30")

(add-statement.rb *rdfrb* "http://ar.to/#self" "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" "http://xmlns.com/foaf/0.1/Person" "2010-07-30")
(add-statement.rb *rdfrb* "http://ar.to/#self" "http://xmlns.com/foaf/0.1/name" "Arto Bendiken" "2010-07-30")

(add-statement.rb *rdfrb* "http://bhuga.net/#ben" "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" "http://xmlns.com/foaf/0.1/Person" "2010-07-30")
(add-statement.rb *rdfrb* "http://bhuga.net/#ben" "http://xmlns.com/foaf/0.1/name" "Ben Lavender" "2010-07-30")

(graph-keyspace *rdfrb* "READMES/rdfrb.dot")
|#
