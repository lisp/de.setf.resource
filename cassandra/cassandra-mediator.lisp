;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.resource.implementation; -*-

(in-package :de.setf.resource.implementation)

(:documentation
  "This file mediates access to cassandra RDF repositories for the `de.setf.resource` CLOS linked
 data library."
  
  (copyright
   "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
   "'de.setf.resource' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.resource' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.resource' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/).")
  
  (description "Implement data, identifier, and instance mediation for a cassandar-based rdf repository.

  This implements the repository interface based on a cassadra repository.
  It supports 0.6 and 0.7 of cassandra with various schema.
  Protocol negotiation intends to adjust the class to corresponds with the service version and the schema
  present, but the cassandra spec reads as if this is doomed to fail."))


(defvar *cassandra-location*
  ;; remote
  ;; #u"thrift://ec2-174-129-66-148.compute-1.amazonaws.com:9160"
  ;; local
  #u"thrift://127.0.0.1:9160"
  "A cassandra service location - either the local one or a remote service 
 - always a 'thrift' uri.")


(defvar *cassandra-connection* nil
  "The current connection to a cassandra instance.")

(defun cassandra-conection ()
  (or *cassandra-connection*
      (setf *cassandra-connection*
            (thrift:client *cassandra-location* :protocol 'cassandra-mediator))))


(defclass cassandra-mediator (repository-mediator dsc:keyspace)
  ((repository :initform nil)
   (dsc::name :initform nil)
   (dsc:version-class-map
    :initform '(("2.1.0" ("SPOC" . cassandra-spoc-index-mediator_2.1.0)
                 ("RDF" . cassandra-rdfrb-index-mediator_2.1.0))
                ("8.3.0" ("SPOC" . cassandra-spoc-index-mediator_8.3.0)
                 ("RDF" . cassandra-rdfrb-index-mediator_8.3.0)))
    :allocation :class)
   (indelible
    :initform t
    :documentation "The default behaviour is write-once.")
   (persistent
    :initform t :allocation :class
    :documentation "The cassandra mediator uses a remote service as the repository store.")
   (readable
    :initform t :allocation :class
    :documentation "The cassandra mediator supports read/write operations.")
   (writable
    :initarg :writable :initform t :allocation :class
    :documentation "The cassandar-mediator supports read/write operations.")))


(defclass cassandra-mediator_2.1.0 (cassandra_2.1.0:keyspace cassandra-mediator)
  ())

(defclass cassandra-mediator_8.3.0 (cassandra_8.3.0:keyspace cassandra-mediator)
  ())


;;;
;;; initialization

(defmethod dsc:compute-keyspace-class ((keyspace cassandra-mediator))
  (let* ((service-version (dsc:describe-version keyspace))
         (service-keyspaces (dsc:keyspace-keyspaces keyspace))
         (version-classes (rest (assoc service-version (dsc:keyspace-version-class-map keyspace) :test #'equal)))
         (keyspace-name (dsc:keyspace-name keyspace)))
    (unless version-classes
      (error "Service version not supported: ~s. Expected one of: ~s."
             service-version (mapcar #'first (dsc:keyspace-version-class-map keyspace))))
    (cond ((null keyspace-name)
           (loop for (supported-name . class) in version-classes
                 for service-name = (find supported-name service-keyspaces :test #'string-equal)
                 when service-name
                 do (progn (setf (dsc:keyspace-name keyspace) service-name)
                           (return class))
                 finally (error "No known keyspace no present in repository: ~s; ~s."
                                version-classes
                                service-keyspaces)))
          ((find keyspace-name service-keyspaces :test #'equal) )
          (t
           (error "Specified keyspace no present in repository: ~s; ~s."
                  keyspace-name
                  service-keyspaces)))))


;;;
;;; mediator/repository management

(defmethod repository-close ((mediator cassandra-mediator))
  (close mediator))


(defmethod repository-namespace-bindings ((mediator cassandra-mediator))
  "A cassandra repository manages no bindings internally. All identifiers are fully qualified."
  nil)


(defmethod repository-uri ((mediator cassandra-mediator) (object t))
  (rdf:repository-value mediator object))

(defmethod repository-uri ((mediator cassandra-mediator) (uri-namestring string))
  (with-output-to-vector-stream (stream)
    (thrift:stream-write-struct stream (thrift:list (cons uri uri-namestring)) 'repository-value)))


;;;
;;; transaction support

(defmethod nbfeb-load ((mediator cassandra-mediator) location-id)
  )

(defmethod nbfeb-sac ((mediator cassandra-mediator) location-id value-id)
  )

(defmethod nbfeb-sas ((mediator cassandra-mediator) location-id value-id)
  )

(defmethod nbfeb-tfas ((mediator cassandra-mediator) location-id value-id)
  )


