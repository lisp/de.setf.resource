;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.resource.implementation; -*-

(in-package :de.setf.resource.implementation)

(:documentation
  "This file mediates access to a cassandra repository with an SPOC schema the 'de.setf.resource' CLOS
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


(defclass cassandra-spoc-index-mediator (cassandra-mediator)
  ((c-index
    :reader repository-c-index
    :documentation "a column family index: (context . (spoc-id . context)* )*")
   (o-index
    :reader repository-o-index
    :documentation "a column family index: (object . (spoc-id . context)* )*")
   (p-index
    :reader repository-p-index
    :documentation "a column family index: (predicate . (spoc-id . context)* )*")
   (s-index
    :reader repository-s-index
    :documentation "a column family index: (subject . (spoc-id . context)* )*")
   (cos-index
    :reader repository-cos-index
    :documentation "a super-column family index: ([..object.context] . (subject . (predicate . spoc-id)* )* )*")
   (cpo-index
    :reader repository-cpo-index
    :documentation "a super-column family index: ([.predicate..context] . (object . (subject . spoc-id)* )* )*")
   (cso-index
    :reader repository-cso-index
    :documentation "a super-column family index: ([subject...context] . (object . (predicate . spoc-id)* )* )*")
   (csp-index
    :reader repository-csp-index
    :documentation "a super-column family index: ([subject...context] . (predicate . (object . spoc-id)* )* )*")
   (poc-index
    :reader repository-poc-index
    :documentation "a super-column family index: ([.predicate.object.] . (context . (subject . spoc-id)* )* )*")
   (soc-index
    :reader repository-soc-index
    :documentation "a super-column family index: ([subject..object.] . (context . (predicate . spoc-id)* )* )*")
   (spc-index
    :reader repository-spc-index
    :documentation "a super-column family index: ([subject.predicate..] . (context . (subject . spoc-id)* )* )*")
   (spo-index
    :reader repository-spo-index
    :documentation "a super-column family index: ([subject.predicate..] . (object . (context . spoc-id)* )* )*")
   (spoc-index
    :reader repository-spoc-index
    :documentation "a column family index:
     ([subject.predicate.object.context] . ((:subject . 's') (:predicate . 'p') (:object . 'o') (:context . 'c') ...))
     It enumerates the respective statement's constituents."))

  (:documentation "A keyspace of the form used by an RDF mediator. It uses three kinds of columns.
 a standard cf (spoc-id . column*) enumerates each statement's constituent values
 four standard cfs (s, p, o, c . (spoc-id . context)*), which enumerates each constituent's statements by context
 eight supercolumns which compress two constituents into the row key, use the thrid as the supercolumn key and the
 fourth as the column, with the spoc-id as the value."))


(defclass cassandra-spoc-index-mediator_2.1.0 (cassandra-spoc-index-mediator cassandra-mediator_2.1.0)
  ())


(defclass cassandra-spoc-index-mediator_8.3.0 (cassandra-spoc-index-mediator cassandra-mediator_8.3.0)
  ())

(defmethod dsc:keyspace-bind-columns ((instance cassandra-spoc-index-mediator)
                                      &key (spoc-index "SPOCIndex")
                                      (c-index "CIndex") (o-index "OIndex") (p-index "PIndex") (s-index "SIndex")
                                      (cos-index "COSIndex") (cpo-index "CPOIndex") (cso-index "CSOIndex") (csp-index "CSPIndex")
                                      (poc-index "POCIndex") (soc-index "SPCIndex") (spc-index "SPCIndex") (spo-index "SPOIndex")
                                      &allow-other-keys)
  (dsc:set-keyspace-column-family instance spoc-index 'spoc-index)
  
  (dsc:set-keyspace-column-family instance c-index 'c-index)
  (dsc:set-keyspace-column-family instance o-index 'o-index)
  (dsc:set-keyspace-column-family instance p-index 'p-index)
  (dsc:set-keyspace-column-family instance s-index 's-index)

  (dsc:set-keyspace-column-family instance cos-index 'cos-index :class 'dsc:super-column-family)
  (dsc:set-keyspace-column-family instance cpo-index 'cpo-index :class 'dsc:super-column-family)
  (dsc:set-keyspace-column-family instance cso-index 'cso-index :class 'dsc:super-column-family)
  (dsc:set-keyspace-column-family instance csp-index 'csp-index :class 'dsc:super-column-family)
  (dsc:set-keyspace-column-family instance poc-index 'poc-index :class 'dsc:super-column-family :required nil)
  (dsc:set-keyspace-column-family instance soc-index 'soc-index :class 'dsc:super-column-family :required nil)
  (dsc:set-keyspace-column-family instance spc-index 'spc-index :class 'dsc:super-column-family :required nil)
  (dsc:set-keyspace-column-family instance spo-index 'spo-index :class 'dsc:super-column-family :required nil))



;;;
;;; manipulating rdf statements

(defmethod add-statement* ((mediator cassandra-spoc-index-mediator) subject predicate object context)
  
  (assert (and subject predicate object context) ()
          "All constituents are required.")

  (let* ((s-subject (repository-value mediator subject))
         (s-predicate (repository-value mediator predicate))
         (s-object (repository-value mediator object))
         (s-context (repository-value mediator context))
         (spoc-id (compute-spoc-id s-subject s-predicate s-object s-context)))
    (labels ((store-supercolumn-index (index key sc-key name value)
               (when index (dsc:set-attribute index (list key sc-key) name value)))
             (store-column-index (index key name value)
               (when index (dsc:set-attribute index key name value)))
             (store-statement ()
               (dsc:set-attributes (repository-spoc-index mediator) spoc-id
                                   :subject s-subject :predicate s-predicate :object s-object
                                   :context s-context)

               ;; the single-constituent index maps its key to the set of ids and the respective contexts
               (store-column-index (repository-c-index mediator) s-context spoc-id s-context)        ; redundant. better?
               (store-column-index (repository-o-index mediator) s-object spoc-id s-context)
               (store-column-index (repository-p-index mediator) s-predicate spoc-id s-context)
               (store-column-index (repository-s-index mediator) s-subject spoc-id s-context)
               
               ;; the multi-constituent index maps the 3/4 quad to the fourth constituent and the respective id
               (store-supercolumn-index (repository-cos-index mediator) (compute-spoc-id nil nil s-object s-context) s-subject s-predicate spoc-id)
               (store-supercolumn-index (repository-cpo-index mediator) (compute-spoc-id nil s-predicate nil  s-context) s-object s-subject spoc-id)
               (store-supercolumn-index (repository-cso-index mediator) (compute-spoc-id s-subject nil nil s-context) s-object s-predicate spoc-id)
               (store-supercolumn-index (repository-csp-index mediator) (compute-spoc-id s-subject nil nil s-context) s-predicate s-object spoc-id)
               (store-supercolumn-index (repository-poc-index mediator) (compute-spoc-id nil s-predicate s-object nil) s-context s-subject spoc-id)
               (store-supercolumn-index (repository-soc-index mediator) (compute-spoc-id s-subject nil s-object nil) s-context s-predicate spoc-id)
               (store-supercolumn-index (repository-spc-index mediator) (compute-spoc-id s-subject s-predicate nil nil) s-context s-object spoc-id)
               (store-supercolumn-index (repository-spo-index mediator) (compute-spoc-id s-subject s-predicate nil nil) s-object s-context spoc-id)))

      (handler-case (progn (dsc:get-attribute (repository-spoc-index mediator) spoc-id :context)
                           ;; if this completes, the statement is already present in the graph
                           (duplicate-statement mediator :statement (list subject predicate object context))
                           ;; if the signal is not handled, just return nil
                           nil)
        ;; if there was none, compute the id and add the properties
        ((or cassandra_2.1.0:notfoundexception cassandra_8.3.0:notfoundexception)  (c)
         (declare (ignore c))
         (store-statement)
         ;; return the new id
         spoc-id)))))


(defmethod map-statements* (continuation (mediator cassandra-spoc-index-mediator) subject predicate object context)
  ;; indices
  ;; spoc : ( [s.p.o.c] . statement-attribute* )
  ;; spo  : ( [s.p] . ( [o] . (c . spoc-id)* )* )
  ;; sp c : ( [s.p] . ( [c] . (o . spoc-id)* )* )
  (flet ((map-supercolumn-family (op index row-key &optional super-column-key)
           ;; given the terms for just the super-column row, iterate over all identifier supercolumns
           (if super-column-key
             (loop for column in (dsc:get-columns index (list row-key super-column-key))
                   do (funcall op
                               (model-value mediator (dsc:column-name column))
                               (dsc:column-value column)))
             (loop for ((nil supercolumn-key) . columns) in (dsc:get-columns index row-key)
                   for value = (model-value mediator supercolumn-key)
                   do (loop for column in columns
                            do (funcall op
                                        value
                                        (model-value mediator (dsc:column-name column))
                                        (dsc:column-value column))))))
         (map-spoc-columns (op index key column-names)
           (flet ((cmv (column) (model-value mediator (dsc:column-value column))))
             (declare (dynamic-extent #'cmv))
             (loop for column-id.c in (dsc:get-columns index key)
                   for s-columns = (dsc:get-columns (repository-spoc-index mediator)
                                                    (dsc:column-name column-id.c)
                                                    :column-names column-names)
                   do (apply op 
                             (model-value mediator (dsc:column-value column-id.c))
                             (dsc:column-name column-id.c)
                             (map-into s-columns #'cmv s-columns))))))
    
    (handler-case 
      (spoc-case (mediator (s-subject s-predicate s-object s-context) subject predicate object context)
        :spoc                           ; check for a context-specific statement
        (let ((spoc-id (compute-spoc-id s-subject s-predicate s-object s-context)))
          (when (dsc:get-attributes (repository-spoc-index mediator) spoc-id :column-names '(:context))
            ;; if it completes with an equivalent object
            (funcall continuation subject predicate object context spoc-id)))
        
        :spo-                           ; look for all equivalent statements across contexts
        (flet ((do-constituents (context id)
                 (funcall continuation subject predicate object context id)))
          (declare (dynamic-extent #'do-constituents))
          (map-supercolumn-family #'do-constituents (repository-spo-index mediator) (compute-spoc-id s-subject s-predicate nil nil) s-object))
        
        :sp-c                           ; retrieve for subject and predicate in the context
        (let ((spc-index (repository-spc-index mediator))
              (csp-index (repository-csp-index mediator)))
          (flet ((do-constituents (object id)
                   (funcall continuation subject predicate object context id)))
            (declare (dynamic-extent #'do-constituents))
            (cond (spc-index
                   (map-supercolumn-family #'do-constituents spc-index (compute-spoc-id s-subject s-predicate nil nil) s-context))
                  (csp-index
                   (map-supercolumn-family #'do-constituents csp-index (compute-spoc-id s-subject nil nil s-context) s-predicate))
                  (t
                   (flet ((do-constituents (test-context id test-predicate test-subject)
                            (when (and (equal subject test-subject) (equal predicate test-predicate) (equal context test-context))
                              (funcall continuation subject predicate object context id))))
                     (declare (dynamic-extent #'do-constituents))
                     (map-spoc-columns #'do-constituents (repository-o-index mediator) s-subject '(:predicate :subject)))))))
        
        :sp--                           ; look for subject and predicates across contexts
        (flet ((do-constituents (object context id)
                 (funcall continuation subject predicate object context id)))
          (declare (dynamic-extent #'do-constituents))
          (map-supercolumn-family #'do-constituents (repository-spo-index mediator) (compute-spoc-id s-subject s-predicate nil nil)))
        
        :s-oc                          ; look for s.o in the context
        (let ((soc-index (repository-soc-index mediator))
              (cso-index (repository-cso-index mediator)))
          (flet ((do-constituents (predicate id)
                   (funcall continuation subject predicate object context id)))
            (declare (dynamic-extent #'do-constituents))
            (cond (soc-index
                   (map-supercolumn-family #'do-constituents soc-index (compute-spoc-id s-subject nil s-object nil) s-context))
                  (cso-index
                   (map-supercolumn-family #'do-constituents cso-index (compute-spoc-id s-subject nil nil s-context) s-object))
                  (t
                   (flet ((do-constituents (test-context id test-object test-subject)
                            (when (and (equal subject test-subject) (equal object test-object) (equal context test-context))
                              (funcall continuation subject predicate object context id))))
                     (declare (dynamic-extent #'do-constituents))
                     (map-spoc-columns #'do-constituents (repository-p-index mediator) s-subject '(:object :subject)))))))
        
        :s-o-                           ; retrieve s.o across contexts
        (flet ((do-constituents (context predicate id)
                 (funcall continuation subject predicate object context id)))
          (declare (dynamic-extent #'do-constituents))
          (map-supercolumn-family #'do-constituents (repository-soc-index mediator) (compute-spoc-id s-subject nil s-object nil)))
        
        :s--c                           ; retrieve all s in the context
        (flet ((do-constituents (object predicate id)
                 (funcall continuation subject predicate object context id)))
          (declare (dynamic-extent #'do-constituents))
          (map-supercolumn-family #'do-constituents (repository-cso-index mediator) (compute-spoc-id s-subject nil nil s-context)))
        
        :s---                           ; retrieve all s across all contexts
        (flet ((do-constituents (context id object predicate)
                 (funcall continuation subject predicate object context id)))
          (declare (dynamic-extent #'do-constituents))
          (map-spoc-columns #'do-constituents (repository-s-index mediator) s-subject '(:object :predicate)))
        
        :-poc                          ; look for p.o in the context
        (let ((poc-index (repository-poc-index mediator))
              (cpo-index (repository-cpo-index mediator)))
          (flet ((do-constituents (subject id)
                   (funcall continuation subject predicate object context id)))
            (declare (dynamic-extent #'do-constituents))
            (cond (poc-index
                   (map-supercolumn-family #'do-constituents poc-index (compute-spoc-id nil s-predicate s-object nil) s-context))
                  (cpo-index
                   (map-supercolumn-family #'do-constituents cpo-index (compute-spoc-id nil s-predicate nil s-context) s-object))
                  (t
                   (flet ((do-constituents (test-context id test-object test-predicate)
                            (when (and (equal predicate test-predicate) (equal object test-object) (equal context test-context))
                              (funcall continuation subject predicate object context id))))
                     (declare (dynamic-extent #'do-constituents))
                     (map-spoc-columns #'do-constituents (repository-s-index mediator) s-subject '(:object :predicate)))))))
        
        :-po-                         ; retrieve p.o across contexts
        (flet ((do-constituents (context subject id)
                 (funcall continuation subject predicate object context id)))
          (declare (dynamic-extent #'do-constituents))
          (map-supercolumn-family #'do-constituents (repository-poc-index mediator) (compute-spoc-id nil s-predicate s-object nil)))
        
        :-p-c                         ; retrieve all p in the context
        (flet ((do-constituents (object subject id)
                 (funcall continuation subject predicate object context id)))
          (declare (dynamic-extent #'do-constituents))
          (map-supercolumn-family #'do-constituents (repository-cpo-index mediator) (compute-spoc-id nil s-predicate nil s-context)))
        
        :-p--                           ; retrieve all p across all contexts
        (flet ((do-constituents (context id object subject)
                 (funcall continuation subject predicate object context id)))
          (declare (dynamic-extent #'do-constituents))
          (map-spoc-columns #'do-constituents (repository-p-index mediator) s-predicate '(:object :subject)))
        
        :--oc                         ; retrieve all o in the context
        (flet ((do-constituents (subject predicate id)
                 (funcall continuation subject predicate object context id)))
          (declare (dynamic-extent #'do-constituents))
          (map-supercolumn-family #'do-constituents (repository-cos-index mediator) (compute-spoc-id nil nil s-object s-context)))
        
        :--o-                           ; retrieve all o across all contexts
        (flet ((do-constituents (context id predicate subject)
                 (funcall continuation subject predicate object context id)))
          (declare (dynamic-extent #'do-constituents))
          (map-spoc-columns #'do-constituents (repository-o-index mediator) s-object '(:predicate :subject)))
        
        :---c                         ; retrieve all from a context
        (flet ((do-constituents (context id object predicate subject)
                 (funcall continuation subject predicate object context id)))
          (declare (dynamic-extent #'do-constituents))
          (map-spoc-columns #'do-constituents (repository-c-index mediator) s-context '(:object :predicate :subject)))
        
        :----                         ; retrieve all statements
        (let ((spoc-index (repository-spoc-index mediator)))
          (flet ((do-key-slice (key-slice)
                   (let ((id (dsc:keyslice-key key-slice))
                         (columns (dsc:keyslice-columns key-slice)))
                     (flet ((cmv (cosc)
                              (model-value mediator (dsc:column-value (dsc:columnorsupercolumn-column cosc)))))
                       (case (length columns)
                         (4 (destructuring-bind (c-cosc o-cosc p-cosc s-cosc)
                                                (dsc:keyslice-columns key-slice)
                              (funcall continuation (cmv s-cosc) (cmv p-cosc) (cmv o-cosc) (cmv c-cosc) id)))
                         (0 )
                         (t
                          (warn "Incomplete SPOC entry: ~s . ~s"
                                id columns)))))))
            (declare (dynamic-extent #'do-key-slice))
            (dsc:map-range-slices #'do-key-slice (dsc:column-family-keyspace spoc-index)
                              :column-family (dsc:column-family-name spoc-index)
                              :count (dsc:column-family-slice-size spoc-index)
                              :start-key "" :finish-key ""))))

      (cassandra_2.1.0:notfoundexception (c) (declare (ignore c)) nil))))


(defmethod delete-statement* ((mediator cassandra-spoc-index-mediator) subject predicate object context)

  (assert (and subject predicate object context) ()
          "All constituents are required.")
  
  (let* ((s-subject (repository-value mediator subject))
         (s-predicate (repository-value mediator predicate))
         (s-object (repository-value mediator object))
         (s-context (repository-value mediator context))
         (spoc-id (compute-spoc-id s-subject s-predicate s-object s-context)))
    (flet ((delete-column-index (index key name)
             (when index (dsc:set-attribute index key name nil)))
           (delete-supercolumn-index (index key sc-key name)
             (when index (dsc:set-attribute index (list key sc-key) name nil))))
      
      ;;(dolist (dsc:column-name '(:subject :predicate :object :context))
      ;;  (dsc:set-attribute (repository-spoc-index mediator) spoc-id column-name nil))
      (dsc:remove mediator :key spoc-id :column-family (dsc:column-family-name (repository-spoc-index mediator)))
      
      (delete-column-index (repository-c-index mediator) s-context spoc-id)
      (delete-column-index (repository-o-index mediator) s-object spoc-id)
      (delete-column-index (repository-p-index mediator) s-predicate spoc-id)
      (delete-column-index (repository-s-index mediator) s-subject spoc-id)
      
      (delete-supercolumn-index (repository-cos-index mediator) (compute-spoc-id nil nil s-object s-context) s-subject s-predicate)
      (delete-supercolumn-index (repository-cpo-index mediator) (compute-spoc-id nil s-predicate nil  s-context) s-object s-subject)
      (delete-supercolumn-index (repository-cso-index mediator) (compute-spoc-id s-subject nil nil s-context) s-object s-predicate)
      (delete-supercolumn-index (repository-csp-index mediator) (compute-spoc-id s-subject nil nil s-context) s-predicate s-object)
      (delete-supercolumn-index (repository-poc-index mediator) (compute-spoc-id nil s-predicate s-object nil) s-context s-subject)
      (delete-supercolumn-index (repository-soc-index mediator) (compute-spoc-id s-subject nil s-object nil) s-context s-predicate)
      (delete-supercolumn-index (repository-spc-index mediator) (compute-spoc-id s-subject s-predicate nil nil) s-context s-object)
      (delete-supercolumn-index (repository-spo-index mediator) (compute-spoc-id s-subject s-predicate nil nil) s-object s-context))))



#|

(defun test-map (test)
  (destructuring-bind (pattern &rest expected-results) test
    (let ((results ()))
      (block :map
        (handler-bind ((error (lambda (c) (push c results) (break "error: ~a" c) (return-from :map nil))))
          (apply #'map-statements *spoc* #'(lambda (subject predicate object context id)
                                             (declare (ignore id))
                                             (push (list subject predicate object context) results))
                 pattern)))
      (format *trace-output* "~&~a ~:[ok~;failed: ~:*~s~]"
              pattern
              (set-exclusive-or results expected-results :test #'equalp)))))

(defparameter *c-location*
  ;; remote
  ;; #u"thrift://ec2-174-129-66-148.compute-1.amazonaws.com:9160"
  ;; local
  #u"thrift://127.0.0.1:9160"
  "A cassandra service location - either the local one or a remote service 
 - always a 'thrift' uri.")

(defparameter *spoc* (client *c-location* :name "SPOC" :protocol 'cassandra-spoc-index-mediator))

(add-statement *spoc* "vanille" "scoops" "100" "2010-07-28")
(add-statement *spoc* "vanille" "scoops" "10" "2010-07-27")
(add-statement *spoc* "cheesecake" "slices" "2" "2010-07-28")
(add-statement *spoc* "cheesecake" "cheesecake" "20" "2010-07-29")

(graph-keyspace *spoc* "READMES/rdfspoc2.dot")

(map nil #'test-map
     '(((nil nil nil nil) .                 (("cheesecake" "slices" "20" "2010-07-29") ("vanille" "scoops" "100" "2010-07-28")
                                             ("cheesecake" "slices" "2" "2010-07-28") ("vanille" "scoops" "10" "2010-07-27")))
       (("vanille" nil nil nil) .           (("vanille" "scoops" "100" "2010-07-28") ("vanille" "scoops" "10" "2010-07-27")))
       ((nil "scoops" nil nil) .            (("vanille" "scoops" "100" "2010-07-28") ("vanille" "scoops" "10" "2010-07-27")))
       (("vanille" "scoops" nil nil) .      (("vanille" "scoops" "10" "2010-07-27") ("vanille" "scoops" "100" "2010-07-28")))
       ((nil nil "10" nil) .                (("vanille" "scoops" "10" "2010-07-27")))
       ((nil nil "100" nil).                (("vanille" "scoops" "100" "2010-07-28")))
       (("vanille" nil "10" nil) .          (("vanille" "scoops" "10" "2010-07-27")))
       (("vanille" nil "100" nil) .         (("vanille" "scoops" "100" "2010-07-28")))
       ((nil "scoops" "10" nil) .           (("vanille" "scoops" "10" "2010-07-27")))
       ((nil "scoops" "100" nil) .          (("vanille" "scoops" "100" "2010-07-28")))
       (("vanille" "scoops" "10" nil) .     (("vanille" "scoops" "10" "2010-07-27")))
       (("vanille" "scoops" "100" nil) .    (("vanille" "scoops" "100" "2010-07-28")))
       ((nil nil nil "2010-07-27") .        (("vanille" "scoops" "10" "2010-07-27")))
       ((nil nil nil "2010-07-28") .        (("vanille" "scoops" "100" "2010-07-28") ("cheesecake" "slices" "2" "2010-07-28")))
       (("vanille" nil nil "2010-07-28") .  (("vanille" "scoops" "100" "2010-07-28")))
       (("vanille" "scoops" nil "2010-07-27") . (("vanille" "scoops" "10" "2010-07-27")))
       ((nil nil "10" "2010-07-27") .       (("vanille" "scoops" "10" "2010-07-27")))
       ((nil nil "100" "2010-07-28") .      (("vanille" "scoops" "100" "2010-07-28")))
       (("vanille" nil "10" "2010-07-27") . (("vanille" "scoops" "10" "2010-07-27")))
       (("vanille" nil "100" "2010-07-27"))     ; note scoop count
       ((nil "scoops" "10" "2010-07-27") .  (("vanille" "scoops" "10" "2010-07-27")))
       ((nil "scoops" "100" "2010-07-27"))
       ((nil "scoops" "100" "2010-07-28") . (("vanille" "scoops" "100" "2010-07-28")))
       (("vanille" "scoops" "10" "2010-07-27") . (("vanille" "scoops" "10" "2010-07-27")))
       (("vanille" "scoops" "100" "2010-07-28") . (("vanille" "scoops" "100" "2010-07-28")))))


(defun test-spoc-case (mediator subject predicate object context)
  (spoc-case (mediator (s-subject s-predicate s-object s-context) subject predicate object context)
    :spoc :spoc
    :spo- :spo-
    :sp-c :sp-c
    :sp-- :sp--
    :s-oc :s-oc
    :s-o- :s-o-
    :s--c :s--c
    :s--- :s---
    :-poc :-poc
    :-po- :-po-
    :-p-c :-p-c
    :-p-- :-p--
    :--oc :--oc
    :--o- :--o-
    :---c :---c
    :---- :----))

(test-spoc-case t 1 nil 2 nil)

(defun clear-keyspace (keyspace)
  (dolist (column-family (mapcar #'first (keyspace-description keyspace)))
    (flet ((clear-keyslice (keyslice)
             (let ((key (dsc:keyslice-key keyslice)))
               (cassandra_2.1.0:remove keyspace (keyspace-name keyspace)
                                       key (cassandra_2.1.0:make-columnpath :column-family column-family)
                                       (cassandra_8.3.0:clock-timestamp (keyspace-clock keyspace))
                                       (keyspace-consistency-level keyspace)))))
      (dsc:map-range-slices #'clear-keyslice keyspace
                        :column-family column-family
                        :count nil
                        :start-key #() :finish-key #()))))

;;; (clear-keyspace *spoc*)

;;; rdf.rb example

(add-statement *spoc* "http://rdf.rubyforge.org/" "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" "http://usefulinc.com/ns/doap#Project" "2010-07-30")
(add-statement *spoc* "http://rdf.rubyforge.org/" "http://usefulinc.com/ns/doap#developer" "http://ar.to/#self" "2010-07-30")
(add-statement *spoc* "http://rdf.rubyforge.org/" "http://usefulinc.com/ns/doap#developer" "http://bhuga.net/#ben" "2010-07-30")

(add-statement *spoc* "http://ar.to/#self" "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" "http://xmlns.com/foaf/0.1/Person" "2010-07-30")
(add-statement *spoc* "http://ar.to/#self" "http://xmlns.com/foaf/0.1/name" "Arto Bendiken" "2010-07-30")

(add-statement *spoc* "http://bhuga.net/#ben" "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" "http://xmlns.com/foaf/0.1/Person" "2010-07-30")
(add-statement *spoc* "http://bhuga.net/#ben" "http://xmlns.com/foaf/0.1/name" "Ben Lavender" "2010-07-30")

(graph-keyspace *spoc* "READMES/rdfspoc.dot")
|#
