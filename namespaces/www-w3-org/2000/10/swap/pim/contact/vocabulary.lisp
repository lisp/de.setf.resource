;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: xml-query-data-model; -*-

(common-lisp:in-package :common-lisp-user)

(common-lisp:defpackage "http://www.w3.org/2000/10/swap/pim/contact#"
  (:use)
  (:export
   "ContactLocation"
   "Person"
   )
  (:documentation nil))

(common-lisp:in-package "http://www.w3.org/2000/10/swap/pim/contact#")

(rdfs:defvocabulary "contact"
  :uri "http://www.w3.org/2000/10/swap/pim/contact#"
  :definitions
  ((de.setf.resource.schema:defclass |Address| nil nil)
   
   (de.setf.resource.schema:defclass |ContactLocation|
     nil
     ((|address| :type |Address|)
      (|fax| :type |Fax|) (|phone| :type |Phone|)))
   
   (de.setf.resource.schema:defclass |Female| nil nil)
   
   (de.setf.resource.schema:defclass |LanguageCode| nil nil)
   
   (de.setf.resource.schema:defclass |Male| nil nil)
   
   (de.setf.resource.schema:defclass |Phone| nil nil)
   
   (de.setf.resource.schema:defclass |SocialEntity|
     nil
     ((|birthday| :type |Date|)))))

