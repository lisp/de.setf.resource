;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.resource.implementation; -*-


(in-package :de.setf.resource.implementation)

(:documentation
  "This file mediates access to allegrograph RDF stores for the `de.setf.resource` CLOS linked data library."
 
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
  "'de.setf.resource' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.resource' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.resource' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/).")

 (description "Implement data, identifier, and instance mediation for a cassandar-based rdf store.

  This implements the repository interface based on a cassadra store. It supports 0.6 and 0.7 of cassandra
 with various schema. An initializationprotocol adjusts the class to corresponds with the service version
 and the schema present."))


(defmacro spoc-case ((mediator (sub pre obj con) subject predicate object context)
                     &key spoc spo (spo- spo) spc (sp-c spc) sp (sp-- sp)
                     soc (s-oc soc) so (s-o- so) sc (s--c sc) s (s--- s)
                     poc (-poc poc) po (-po- po) pc (-p-c pc) p (-p-- p)
                     oc (--oc oc) o (--o- o) c (---c c) all (---- all))
  `(let ((,sub (repository-value ,mediator ,subject))
         (,pre (repository-value ,mediator ,predicate))
         (,obj (repository-value ,mediator ,object))
         (,con (repository-value ,mediator ,context)))
     (ecase (logior (if ,sub #b1000 0) (if ,pre #b0100 0) (if ,obj #b0010 0) (if ,con #b0001 0))
       (#b1111 ,spoc)
       (#b1110 ,spo-)
       (#b1101 ,sp-c)
       (#b1100 ,sp--)
       (#b1011 ,s-oc)
       (#b1010 ,s-o-)
       (#b1001 ,s--c)
       (#b1000 ,s---)
       (#b0111 ,-poc)
       (#b0110 ,-po-)
       (#b0101 ,-p-c)
       (#b0100 ,-p--)
       (#b0011 ,--oc)
       (#b0010 ,--o-)
       (#b0001 ,---c)
       (#b0000 ,----))))

#+digitool
(setf (ccl:assq 'spoc-case ccl:*fred-special-indent-alist*) 1)


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


(defclass cassandra-mediator (resource-mediator dsc:keyspace)
  ((store :initform nil)
   (dsc::name :initform nil)
   (indelible
    :initarg :indelible :initform t
    :reader repository-indelible? :writer setf-repository-indelible)
   (dsc:version-class-map
    :initform '(("2.1.0" ("SPOC" . cassandra-spoc-index-mediator_2.1.0)
                         ("RDF" . cassandra-rdfrb-index-mediator_2.1.0))
                ("8.3.0" ("SPOC" . cassandra-spoc-index-mediator_8.3.0)
                         ("RDF" . cassandra-rdfrb-index-mediator_8.3.0)))
    :allocation :class)))


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
                 finally (error "No known keyspace no present in store: ~s; ~s."
                                version-classes
                                service-keyspaces)))
          ((find keyspace-name service-keyspaces :test #'equal) )
          (t
           (error "Specified keyspace no present in store: ~s; ~s."
                  keyspace-name
                  service-keyspaces)))))




 
;;;
;;; core access implementation for cassandra-mediator

(defgeneric add-statement* (mediator subject predicate object context) )

(defgeneric delete-statement* (mediator subject predicate object context) )

(defgeneric map-statements* (continuation mediator subject predicate object context) )


(defmethod rdf:delete-statement ((mediator cassandra-mediator) (triple rdf:triple))
  "if the repository is indelible cause an error, but if the
 repository permits revisions, update all occurrence entries to reflect that the statment is no longer valid."

  (if (repository-indelible? mediator)
    (error "Statements are indelible.")
    (delete-statement* mediator (triple-subject triple) (triple-predicate triple) (triple-object triple)
                       (or (rdf:context triple) (repository-default-context mediator)))))


(defmethod rdf:find-instance ((source cassandra-mediator) (subject t))
  "Return the instance which is registered with the MEDIATOR for the SUBJECT value's interned equivalent."
  (gethash (rdf:repository-value source subject) (repository-instance-cache source)))


(defmethod rdf:project-graph ((triple rdf:triple) (mediator cassandra-mediator))
  "Given a QUAD statement and a MEDIATOR, add the denoted triple to the repository store, with optional
 temporally qualified association to a graph. The implementation depends on the store schema."

  (unless (triple-id triple)
    (add-statement* mediator (triple-subject triple) (triple-predicate triple) (triple-object triple)
                    (or (rdf:context triple) (repository-default-context mediator)))))

(defmethod rdf:insert-statement ((triple rdf:triple) (mediator cassandra-mediator))
  (unless (triple-id triple)
    (add-statement* mediator (triple-subject triple) (triple-predicate triple) (triple-object triple)
                    (or (rdf:context triple) (repository-default-context mediator)))))

(defmethod rdf:query ((mediator cassandra-mediator) &key subject predicate object
                      (context (repository-default-context mediator)) continuation offset limit)
  "Given a SUBJECT, PREDICATE, OBJECT, and (optional) GRAPH specification, return the extant statement(s)."

  (let* ((result ()))
    (flet ((component-continuation (subject predicate object context id)
             (when (or (null offset) (minusp (decf offset)))
               (when (or (null limit) (not (minusp (decf limit))))
                 (let ((stmt (make-quad :subject subject :predicate predicate :object object :context context :id id)))
                   (if continuation
                     (funcall continuation stmt)
                     (Push stmt result)))))))
      
      (map-statements* #'component-continuation mediator subject predicate object context)
      
      ;; for the case where the internal continuation collected the result
      (nreverse result))))



(defmethod rdf:repository-persistent? ((mediator cassandra-mediator))
  "The cassandra mediator uses a remote service as the repository store."
  t)

(defmethod rdf:repository-readable? ((mediator cassandra-mediator))
  "The cassandra mediator supports read/write operations."
  t)

(defmethod rdf:repository-transient? ((repository cassandra-mediator))
  "The cassandra mediator uses a remote service as the repository store."
  nil)

(defmethod rdf:repository-writable? ((repository cassandra-mediator))
  "The cassandar-mediator supports read/write operations."
  t)     
                                         

(defmethod store-uri ((source cassandra-mediator) (object t))
  (rdf:repository-value source object))

(defmethod store-uri ((source cassandra-mediator) (uri-namestring string))
  (with-output-to-vector-stream (stream)
    (thrift:stream-write-struct stream (thrift:list (cons uri uri-namestring)) 'repository-value)))


