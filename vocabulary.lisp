;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.resource.implementation; -*-


(in-package :de.setf.resource.implementation)

(:documentation
  "This file supports RDF vocabularies for the `de.setf.resource` CLOS linked data library."
 
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
  "'de.setf.resource' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.resource' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.resource' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/).")

 (description "The vocabulary operators map type/class models between CLOS and RDF schemas. The CLOS
 representation comprises the namce. the class instances, and property accessors. A representation of
 a vocabulary can be transliterated from the RDF representation retrieved from a triple store or it
 can be defined in a lisp source file. The vocabulary also includes an optional identifier map to specify the
 relations between Lisp symbols and RDF uri.

 Note, on the subject of 'canonical' identifiers and the vocabulary namespace names:

 The standard XML Schema namespaces are 'http://www.w3.org/2001/XMLSchema' and
 'http://www.w3.org/2001/XMLSchema-datatypes'[1], depending on whether the circumstances involve schema
 aspects inaddition to the datatype terms. In their original useage, these corresponded to the prefixes 'xs'
 and 'xsd', respectively. On the other hand, standard prefix binding which appears in RDF specifications is
 'xsd=http://www.w3.org/2001/XMLSchema#'[2]. That is 'xsd' for the fulls schema namespace and with a
 fragment character.

 The original OWL recommendation even avails itself of both approaches. On one hand, the
 general discussion of namespaces demonstrates the presence of fragment separator characters in the
 vocabulary namespace[3], but on the other, the discussion of extensions[4], suggests that fragements are
 themselves integral entites which _include_ the separator character. While this may be just the result of
 editorial oversight, it demonstrates that the operators can perform neither a strict url merge, not a simple
 string catenation if they are to produce the expected results from the widely varies input arguments.

 The 2009 versions reiterate the practice, to the use different encodings to designate each given
 identifier[5],[6]. whatever. In the exmple of the SKOS RDF schema,[7] the practice explicitly indicateing
 fragments appears again. of Given these precedents, a vocabulary permits to specify explicit
 correspondences between packages and vocabulary namespaces as well as between specific individual
 identifers.

 ---
 [1]: http://www.w3.org/TR/2001/REC-xmlschema-2-20010502/#namespaces
 [2]: http://www.w3.org/TR/2004/REC-rdf-mt-20040210/
 [3]: http://www.w3.org/TR/2004/REC-owl-guide-20040210/#Namespaces
 [4]: http://www.w3.org/TR/2004/REC-owl-guide-20040210/#DefiningSimpleClasses
 [5]: http://www.w3.org/TR/2009/REC-owl2-quick-reference-20091027/#Names.2C_Prefixes.2C_and_Notation
 [6]: http://www.w3.org/TR/2009/CR-xmlschema11-1-20090430/#nss_langids
 [7]: http://www.w3.org/2009/08/skos-reference/skos.rdf
"))


(defclass rdf:vocabulary ()
  ((name
    :reader vocabulary-name :writer setf-vocabulary-name)
   (uri
    :initform (error "A uri is required.")
    :type string
    :reader vocabulary-uri :writer setf-vocabulary-uri
    :documentation "The vocabulary base uri identifies the 'namespace' which comprises its terms and,
     by default, locates the schema declarations.")
   (resource-uri
    :initform (error "A resource-uri is required.")
    :reader vocabulary-resource-uri :writer setf-vocabulary-resource-uri
    :documentation "A vocabulary's resource is the URI location of its schema. If it is null, then the
    vocabulary uri applies. The distinct values allow for cases where a base uri is redirected
    to a concrete rdf document and that schema specifies the document as the 'isDefinedBy resource.
    For example, 'http://zbw.eu/namespaces/zbw-extensions/', which describes the STW vocabulary.")    
   (identifier-map
    :initform ()
    :reader vocabulary-identifier-map
    :documentation "An a-list of maps between model symbols and resource URI namestrings for the external
     schema definition. The default value is derived from a combination of the vocabulary uri, the values of
     the :identifiers and :identifier-map initialization arguments. The former is a list of symbols or
     strings, of which the latter are interned in the respective package.")
   (definitions
     :initform nil :initarg :definitions
     :accessor vocabulary-definitions)
   (documentation
     :initform nil :initarg :documentation
     :accessor vocabulary-documentation))

  (:documentation "A vocabulary represents an RDF schema in Lisp terms and CLOS class definitions.
 It comprises a set of class definitions, which implicitly define the schema's predicates, and
 an optional map between Lisp names and the schema's URI. A vocabulary can be constructed based
 on an RDF representation as mediated by a triple store, or it can be cached as an explicit definition
 in the file system."))


(defmacro rdf:defvocabulary (name &key (uri (error "uri is required."))
                                   definitions (identifiers nil i-s)
                                   (identifier-map nil im-s)
                                   (package nil p-s)
                                   documentation)
  `(make-instance 'vocabulary
     :name ,(string name)
     :uri ,uri
     :definitions ',definitions
     ,@(when i-s `(:identifiers ',identifiers))
     ,@(when im-s `(:identifier-map ',identifier-map))
     ,@(when p-s `(:package ',package))
     ,@(when documentation `(:documentation ,documentation))
     ))


(defmethod print-object ((object vocabulary) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (format stream "~a [~a] x ~s x ~s"
            (vocabulary-name object)
            (vocabulary-uri object)
            (length (vocabulary-identifier-map object))
            (length (vocabulary-definitions object)))))


(defmethod shared-initialize ((instance vocabulary) (slots t)
                              &key identifier-map identifiers package uri (resource-uri uri)
                              (name nil n-s)
                              (separator (compute-extrinsic-separator resource-uri package)))
  "When (re)initializing combine the possible argument for uri, a list of identifiers or a package,
 and/or an identifier-map a-list arguments to construct and set the vocabulary's map to a
 (symbol . uri-namestring) alist.
 NB. the vocabulary URI and the identifers package need not have the same name. "

  (when uri
    (setf-vocabulary-uri uri instance))
  (when resource-uri
    (setf-vocabulary-resource-uri resource-uri instance))
  (call-next-method)
  (unless uri
    (setf uri (vocabulary-uri instance)))
  (unless resource-uri
    (setf resource-uri (vocabulary-resource-uri instance)))
  (setf (uri-extrinsic-separator (or package resource-uri)) separator)
  

  (let* ((vocabulary-uri (vocabulary-uri instance))
         (vocabulary-package (find-package vocabulary-uri)))
    ;; ensure the name is set or update it if given
    (cond (n-s
           (setf (slot-value instance 'name) name))
          ((not (slot-boundp instance 'name))
           (setf (slot-value instance 'name) vocabulary-uri)))
    (when separator (setf separator (string separator)))

    ;; when initializing or reinitializing augment collect identifiers from both any package
    ;; designated by the vocabulary uri itself and any package designated by the identifiers argument.
    ;; use that list to construct (symbol . uri) entries to augment the identifer-map alist.
    (flet ((add-entry (term)
             (setf identifier-map
                   (acons term (concatenate 'string vocabulary-uri separator (string term))
                          identifier-map)))
           (vocabulary-package-symbol (name)
             (assert (packagep vocabulary-package) ()
                     "Term name specified for a vocabulary w/o a package: ~s, ~s."
                     (vocabulary-name instance) name)
             (intern name vocabulary-package)))
      (when vocabulary-package 
        (loop for symbol being the external-symbols of vocabulary-package
              do (add-entry symbol)))
      (dolist (term identifiers)
        (etypecase term
          (symbol (add-entry term))
          (string (add-entry (vocabulary-package-symbol term)))))
      (when package
        (loop for symbol being the external-symbols of package
              do (add-entry symbol))))

    ;; always set the map to the updated value
    (setf (slot-value instance 'identifier-map) identifier-map)))


(defgeneric (setf vocabulary-uri) (uri vocabulary)
  (:method ((uri string) (vocabulary vocabulary))
    (setf-vocabulary-uri uri vocabulary))
  (:method ((uri symbol) (vocabulary vocabulary))
    (assert uri () "URI may not be null.")
    (setf-vocabulary-uri (package-name (symbol-package uri)) vocabulary))
  (:method ((package package) (vocabulary vocabulary))
    (setf-vocabulary-uri (package-name package) vocabulary)))

      
#+digitool
(progn
  (defmethod documentation ((object vocabulary) &optional type)
    (declare (ignore type))
    (vocabulary-documentation object))
  (defmethod (setf documentation) (documentation (object vocabulary) &optional type)
    (declare (ignore type))
    (setf (vocabulary-documentation object) documentation)))


#-digitool
(progn
  (defmethod documentation ((object vocabulary) (type (eql t)))
    (documentation object 'vocabulary))
  (defmethod documentation ((object vocabulary) (type (eql 'vocabulary)))
    (vocabulary-documentation object))
  (defmethod (setf documentation) (documentation (object vocabulary) (type (eql t)))
    (setf (documentation object 'vocabulary) documentation))
  (defmethod (setf documentation) (documentation (object vocabulary) (type (eql 'vocabulary)))
    (setf (vocabulary-documentation object) documentation)))

(defgeneric rdf:repository-namespace-bindings (source)
  (:documentation "Returns an a-list of (prefix . namespace-uri) bindings."))


(defmethod setf-vocabulary-uri ((uri string) (object vocabulary))
  (setf (slot-value object 'uri) uri))


(defmethod vocabulary-package ((vocabulary vocabulary))
  (or (find-package (vocabulary-uri vocabulary))
      (error "Vocabulary without a package: ~s." vocabulary)))


(defmethod rdf:ensure-vocabulary ((context t) (vocabulary vocabulary) &key)
  "Given a vocabulary for an anomolous context, return the vocabulary as-is."
  vocabulary)


(defmethod rdf:find-class ((vocabulary vocabulary) (name symbol) &key (error-p t))
  (or (dolist (definition (vocabulary-definitions vocabulary))
        (when (and (eq (first definition) 'rdf:defclass)
                   (eq (second definition) name))
          (return definition)))
      (when error-p
        (rdf:class-not-found (find-class 'resource-class) name))))


(defmethod (setf rdf:find-class) (definition (vocabulary vocabulary) (name symbol))
  (let ((old (find name (vocabulary-definitions vocabulary) :key #'second)))
    (setf (vocabulary-definitions vocabulary)
          (if definition
            (cons definition (if old
                               (remove old (vocabulary-definitions vocabulary))
                               (vocabulary-definitions vocabulary)))
            (if old
              (remove old (vocabulary-definitions vocabulary))
              (vocabulary-definitions vocabulary))))
    definition))


(defmethod rdf:load-vocabulary ((vocabulary-pathname pathname) (vocabulary-uri string) &key (resource-uri nil ru-s))
  "Given a pathname as context, interpret the file as a vocabulary definition."

  (let ((*package* *package*))
    (with-open-file (stream vocabulary-pathname :direction :input)
      (with-standard-io-syntax 
        (loop (let ((form (read stream)))
                (ecase (first form)
                  (defpackage
                    (eval form))
                  (in-package
                   (let ((package (find-package (second form))))
                     (assert (typep package 'package) () 
                             (error "Invalid package definition: ~s." form))
                     (setq *package* package)))
                  (rdf:require-vocabulary
                   (eval form))
                  (rdf:defvocabulary
                    (let ((vocabulary (eval form)))
                      (assert (typep vocabulary 'vocabulary) ()
                              "Invalid vocabulary: ~s." form)
                      (assert (equal vocabulary-uri (vocabulary-uri vocabulary)) ()
                              "uri mismatch: ~s; ~s."
                              vocabulary-uri
                              vocabulary)
                      (when ru-s
                        (unless (equal (vocabulary-resource-uri vocabulary) resource-uri)
                          (warn "Loaded vocabulary resource uri updated not match given value: ~s != ~s."
                                (vocabulary-resource-uri vocabulary) resource-uri)
                          (setf-vocabulary-resource-uri resource-uri vocabulary)))
                      (return vocabulary))))))))))


(defun vocabulary-pathname (uri)
  "Construct a pathname from the uri path, with the name constrained to 'vocabulary'."

  (uri-pathname uri :name "vocabulary"))


(defmethod rdf:require-vocabulary ((uri-namestring string) &key (pathname (vocabulary-pathname uri-namestring)))
  (or (gethash uri-namestring *vocabularies*)
      (setf (gethash uri-namestring *vocabularies*)
            (load-vocabulary pathname uri-namestring))))

(defmethod rdf:require-vocabulary ((uri symbol) &rest args)
  (declare (dynamic-extent args))
  (apply #'rdf:require-vocabulary (symbol-uri-namestring uri) args))


(defgeneric save-vocabulary (vocabulary destination)
  (:method ((vocabulary vocabulary) (stream stream))
    (let* ((uri (vocabulary-uri vocabulary))
           (name (vocabulary-name vocabulary))
           (package (vocabulary-package vocabulary))
           (definitions (vocabulary-definitions vocabulary))
           (symbols ()))
      (dolist (definition definitions)
        (when (eq (symbol-package (second definition)) package)
          (push (second definition) symbols))
        (dolist (slot (fourth definition))
          (when (eq (symbol-package (first slot)) package)
            (push (first slot) symbols))))
      (format stream ";;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: common-lisp-user; -*-~%;;; ~/date:format-iso-time/~%;;; from ~s~%"
              (get-universal-time)
              uri)
      (format stream "~%(in-package :common-lisp-user)~%")
      (format stream "~%~%(defpackage ~s~%  (:use)~%  (:nicknames~@[ ~s~])~%  (:export~{ \"~a\"~}))~%"
              uri name symbols)
      (format stream "~%(rdfs:defvocabulary ~s :uri ~s :package ~s~% :definitions~% ~:w)"
              name uri (package-name package) definitions)))

  (:method ((vocabulary vocabulary) (destination t))
    (save-vocabulary vocabulary (vocabulary-pathname (vocabulary-uri vocabulary))))
  
  (:method ((vocabulary vocabulary) (pathname pathname))
    (ensure-directories-exist pathname)
    (with-open-file (stream pathname :direction :output :if-exists :supersede :if-does-not-exist :create)
      (save-vocabulary vocabulary stream))))


;;;
;;; load standard vocabularies

(defvar *rdf-vocabulary* (rdf:require-vocabulary "http://www.w3.org/1999/02/22-rdf-syntax-ns#"))
(defvar *rdfs-vocabulary* (rdf:require-vocabulary "http://www.w3.org/2000/01/rdf-schema#"))
(defvar *owl-vocabulary* (rdf:require-vocabulary "http://www.w3.org/2002/07/owl#"))
(defvar *xsd-vocabulary* (rdf:require-vocabulary "http://www.w3.org/2001/XMLSchema-datatypes#"))
(defvar *time-vocabulary* (rdf:require-vocabulary "http://www.w3.org/2006/time#"))

;;; note default separators

(setf (uri-extrinsic-separator "KEYWORD") #\/)
(setf (uri-extrinsic-separator "COMMON-LISP") #\/)
(setf (uri-extrinsic-separator "COMMON-LISP-USER") #\/)
(setf (uri-extrinsic-separator "DE.SETF.RESOURCE") #\/)


;;; (mapcar #'rdf:require-vocabulary *default-vocabulary-names*)

;;; (camel-dash-canonicalizer (make-symbol (camel-dash-canonicalizer "asdfQwer")))
              
;;; examples and sources
;;;  http://vocab.org/
;;;  http://www.schemaweb.info

;;; (xqdm:.// *foaf* "Class")
;;; (map nil #'pprint (rdf-document-class-definitions *foaf*))
;;; (rdf-class-properties *foaf* '|http://xmlns.com/foaf/0.1/|:|Agent|) ; "http://xmlns.com/foaf/0.1/Agent"
;;; (rdf-class-properties *foaf* "http://xmlns.com/foaf/0.1/Agent")

;;; (defparameter *beer* (xmlp:document-parser #p"XML:NAMESPACES;purl-org;net;ontology;beer.rdf"))
;;; (setf (rdf:document-ontology-namespace-name *beer*) nil)
;;; (rdf:document-ontology-namespace-name *beer*)
;;; (rdf:document-ontology-namespace-terms *beer*)
;;; (pprint (rdf:document-definitions *beer*))
;;; (rdf:retrieve-vocabulary *beer*)

;;; (rdf:document-class-definitions "http://vocab.org/bio/0.1/.rdf")


;;; vocabulary sources : schemaweb

;;; importing a vocabulary:
;;; first load it into wilbur
;;; (defparameter *v* (rdf:load-vocabulary (wilbur-mediator) "http://www.w3.org/2006/time#"))
;;; (map nil #'(lambda (d) (print (second d)))  (vocabulary-definitions *v*))
;;;
;;; (rdf:query (wilbur-mediator) :subject '{http://www.w3.org/2006/time#}Instant :context nil)
;;; (vocabulary-pathname  "http://www.w3.org/2006/time#")
;;; (save-vocabulary *v* *trace-output*)
;;; (save-vocabulary *v* t)
