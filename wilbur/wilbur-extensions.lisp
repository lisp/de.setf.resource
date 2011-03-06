;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: WILBUR; -*-


(in-package "WILBUR")

(:documentation
  "This file extends various wilbur operators."
 
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
  "'de.setf.resource' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.resource' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.resource' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/)."))



(defmethod rdf:load-repository-as ((db wilbur:db) (source pathname) (form mime:application/rdf+xml))
  (wilbur:db-load db source))


(defmethod rdf:load-vocabulary ((db wilbur:db) (source t) &key)
  (let ((source-desc (wilbur:db-load db source)))
    (when source-desc
      (values (wilbur:node-uri (wilbur:source-desc-url source-desc))
              (wilbur:node-uri (wilbur:source-desc-loaded-from source-desc))))))


(defmethod de.setf.resource:map-statements* (continuation (db db) subject predicate object context)
  "The no-index repository just iterates over the statement list."
  (declare (ignore context))
  (dolist (triple (db-triples db))
    (when (and (eq~ (triple-subject triple) subject)
               (eq~ (triple-predicate triple) predicate)
               (eq~ (triple-object triple) object))
      (funcall continuation triple))))


(defmethod de.setf.resource:map-statements* (continuation (db indexed-db) subject predicate object context)
  "The indexed repository tries to focus iteration over the respective index.
 The indices are at three level, so the context constrain requires a test against the
 matched statements' sources."
  (labels ((filter-statements-by-object-by-context (object context statements)
             (dolist (statement statements)
               (when (and (eq (triple-object statement) object)
                          (find context (triple-sources statement)))
                 (funcall continuation statement))))
           (filter-statements-by-object (object statements)
             (dolist (statement statements)
               (when (and (eq (triple-object statement) object))
                 (funcall continuation statement))))
           (filter-statements-by-context (context statements)
             (dolist (statement statements)
               (when (find context (triple-sources statement))
                 (funcall continuation statement))))
           (filter-statements (statements)
             (map nil continuation statements)))
    
    (de.setf.resource:spoc-case (nil (s p o c) subject predicate object context)
      :spoc (filter-statements-by-object-by-context o c (triple-index-get (db-index-sp db) p s))
      :spo (filter-statements-by-object o (triple-index-get (db-index-sp db) p s))
      :spc  (filter-statements-by-context c (triple-index-get (db-index-sp db) p s))
      :sp  (filter-statements (triple-index-get (db-index-sp db) p s))
      :soc  (filter-statements-by-object-by-context o c (triple-index-get (db-index-s db) s))
      :so  (filter-statements-by-object o (triple-index-get (db-index-s db) s))
      :sc  (filter-statements-by-context c (triple-index-get (db-index-s db) s))
      :s   (filter-statements (triple-index-get (db-index-s db) s))
      :poc  (filter-statements-by-context c (triple-index-get (db-index-po db) p o))
      :po  (filter-statements (triple-index-get (db-index-po db) p o))
      :pc   (filter-statements-by-context c (triple-index-get (db-index-p db) p))
      :p   (filter-statements (triple-index-get (db-index-p db) p))
      :oc   (filter-statements-by-context c (triple-index-get (db-index-o db) o))
      :o   (filter-statements (triple-index-get (db-index-o db) o))
      :c (filter-statements-by-context c (db-triples db))
      :all (filter-statements (db-triples db)))))

(defmethod rdf:save-repository ((db wilbur:db) (stream stream))
  (dolist (triple (wilbur:db-triples db))
    (format stream "~&~/n3:format/" triple))
  (terpri stream))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+(or) ;; implmeneted in terms of map-statements*
(defgeneric db-has-statement? (db subject predicate object)
  (:documentation "Searches the repository for a match to the specified s-p-o combination.
 VALUE : boolean : iff the constraint is satisfied.

 Serves as the basis for has- operators for statement, subject, predicate, object.
 Iterates across the internal collection and uses wilbur's equivalence operator to test each statement in 
 turn, without constructing intermediate results.")

  (:method ((db wilbur:db) subject predicate object)
    (flet ((matching-triple-p (triple)
	     (and (wilbur::eq~ (wilbur:triple-subject triple) subject)
		  (wilbur::eq~ (wilbur:triple-predicate triple) predicate)
		  (wilbur::eq~ (wilbur:triple-object triple) object))))
      (declare (dynamic-extent #'matching-triple-p))
      (find-if #'matching-triple-p (wilbur:db-triples db)))))

#+(or)                                  ; handled by rdf:project
(defmethod rdf:load-repository-as ((db wilbur:db) (source stream) (form mime:application/n3))
  (let ((blanks (make-hash-table :test #'equal)))
    (flet ((intern-resource (uri-namestring)
             (wilbur:node uri-namestring))
           (intern-literal (string datatype language)
             (wilbur:literal string :datatype datatype :language language))
           (intern-blank-node (id-string)
             (or (gethash id-string blanks)
                 (setf (gethash id-string blanks)
                       (de.setf.resource:wilbur-blank-node id-string)))))
      (let ((n3::*intern-resource* #'intern-resource)
            (n3::*intern-literal* #'intern-literal)
            (n3::*intern-blank-node* #'intern-blank-node)
            (n3::*construct-statement* #'wilbur:triple))
        (loop (let ((triple (n3:read source nil nil)))
                (unless triple (return))
                (wilbur:db-add-triple db triple)))))))

#+(or) ;; replaced by the map-statements* method which tests contexts
(defmethod rdf:query ((db indexed-db) &key subject predicate object continuation context limit offset)
  "The indexed repository tries to focus iteration over the respective index."
  (declare (ignore context))
  (let* ((collection (list nil))
         (end collection)
         (matched 0)
         (collected 0))
    (labels ((collect (stmt)
               (when (or (null offset) (> (incf matched) offset))
                 (if continuation
                   (funcall continuation stmt)
                   (setf (rest end) (list stmt) end (rest end)))
                 (when (and limit (>= (incf collected) limit))
                   (return-from rdf:query (values (rest collection) matched collected)))))
             (filter-statements-by-object (object statements)
               (dolist (statement statements)
                 (when (eq (triple-object statement) object) (collect statement)))
               (rest collection))
             (collect-statements (statements)
               (cond ((or offset limit)
                      (map nil #'collect statements))
                     (continuation
                      (map nil continuation statements))
                     (t
                      statements))))
      
      (with-spo-case ((s p o) subject predicate object)
        :spo (filter-statements-by-object o (triple-index-get (db-index-sp db) p s))
        :sp  (collect-statements (triple-index-get (db-index-sp db) p s))
        :so  (filter-statements-by-object o (triple-index-get (db-index-s db) s))
        :s   (collect-statements (triple-index-get (db-index-s db) s))
        :po  (collect-statements (triple-index-get (db-index-po db) p o))
        :p   (collect-statements (triple-index-get (db-index-p db) p))
        :o   (collect-statements (triple-index-get (db-index-o db) o))
        :all (collect-statements (db-triples db))))))


;;;
;;; transaction support
;;;
;;; the wilbur:db version is single-thread. multi-thread would require a locked-db-mixin
;;; and conflict resolution.


(defmethod de.setf.resource.implementation::nbfeb-load ((db wilbur:db) location-id)
  "Return the decoded state from the lock statement."
  (flet ((return-nbfeb-load (subject predicate object context id)
           (declare (ignore subject predicate context id))
           (multiple-value-bind (value flag)
                                (de.setf.resource.implementation::decode-nbfeb-state object)
             (return-from de.setf.resource.implementation::nbfeb-load (values value flag)))))
    (declare (dynamic-extent #'return-nbfeb-load))
    (de.setf.resource:map-statements* #'return-nbfeb-load db location-id !NBFEB nil !NBFEB)))


(defmethod de.setf.resource.implementation::nbfeb-sac ((db wilbur:db) location-id value)
  (flet ((return-nbfeb-sac (subject predicate object context id)
           (declare (ignore id))
           (multiple-value-bind (old-value old-flag)
                                (de.setf.resource.implementation::decode-nbfeb-state object)
             (de.setf.resource:delete-statement* db subject predicate object context)
             (let ((s-value (de.setf.resource.implementation::encode-nbfeb-state value nil)))
               (de.setf.resource:add-statement* db subject predicate s-value context))
             (return-from de.setf.resource.implementation::nbfeb-sac (values old-value old-flag)))))
    (declare (dynamic-extent #'return-nbfeb-sac))
    (de.setf.resource:map-statements* #'return-nbfeb-sac db location-id !NBFEB nil !NBFEB)))


(defmethod de.setf.resource.implementation::nbfeb-sas ((db wilbur:db) location-id value)
  (flet ((return-nbfeb-sas (subject predicate object context id)
           (declare (ignore id))
           (multiple-value-bind (old-value old-flag)
                                (de.setf.resource.implementation::decode-nbfeb-state object)
             (de.setf.resource:delete-statement* db subject predicate object context)
             (let ((s-value (de.setf.resource.implementation::encode-nbfeb-state value t)))
               (de.setf.resource:add-statement* db subject predicate s-value context))
             (return-from de.setf.resource.implementation::nbfeb-sas (values old-value old-flag)))))
    (declare (dynamic-extent #'return-nbfeb-sas))
    (de.setf.resource:map-statements* #'return-nbfeb-sas db location-id !NBFEB nil !NBFEB))

  )


(defmethod de.setf.resource.implementation::nbfeb-tfas ((db wilbur:db) location-id value)
  (flet ((return-nbfeb-tfas (subject predicate object context id)
           (declare (ignore id))
           (multiple-value-bind (old-value old-flag)
                                (de.setf.resource.implementation::decode-nbfeb-state object)
             (de.setf.resource:delete-statement* db subject predicate object context)
             (let ((s-value (de.setf.resource.implementation::encode-nbfeb-state value t)))
               (de.setf.resource:add-statement* db subject predicate s-value context))
             (return-from de.setf.resource.implementation::nbfeb-tfas (values old-value old-flag)))))
    (declare (dynamic-extent #'return-nbfeb-tfas))
    (de.setf.resource:map-statements* #'return-nbfeb-tfas db location-id !NBFEB nil !NBFEB)))
