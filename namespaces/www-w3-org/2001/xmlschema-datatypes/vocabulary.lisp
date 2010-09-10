;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: common-lisp-user; -*-

;;; (rdf:require-vocabulary "http://www.w3.org/2001/XMLSchema-datatypes#")

(in-package :common-lisp-user)


(rdf:require-vocabulary "http://www.w3.org/2001/XMLSchema#")

(defpackage "http://www.w3.org/2001/XMLSchema-datatypes"
  (:use)
  (:documentation "The XML schema datatype identifiers are a subset of those comprised by
 the XML schema namespace. The package is constrcuted such that this subset is imported and
 re-exported in order to a) effect symbol identity, and b) retain the original package
 as the vocabulary uri, as (at least) wilbur depends on that name.")
  (:import-from "http://www.w3.org/2001/XMLSchema"
                "ENTITIES" "ENTITY" "ID" "IDREF" "IDREFS" "NCName"
           "NMTOKEN" "NMTOKENS" "NOTATION" "Name" "QName" "annotation"
           "anyComplexType" "anyListType" "anySimpleType" "anyTreeType"
           "anyType" "anyURI" "attribute" "attributeGroup" "base64Binary"
           "boolean" "byte" "comment" "complexType" "date" "dateTime"
           "decimal" "documentation" "double" "duration" "element" "float"
           "gDay" "gMonth" "gMonthDay" "gYear" "gYearMonth" "hexBinary"
           "int" "integer" "language" "long" "negativeInteger"
           "nonNegativeInteger" "nonPositiveInteger" "normalizedString"
           "pi" "positiveInteger" "restriction" "schema" "short"
           "simpleType" "string" "time" "token" "unsignedByte"
           "unsignedInt" "unsignedLong" "unsignedShort")
  (:export "ENTITIES" "ENTITY" "ID" "IDREF" "IDREFS" "NCName"
           "NMTOKEN" "NMTOKENS" "NOTATION" "Name" "QName" "annotation"
           "anyComplexType" "anyListType" "anySimpleType" "anyTreeType"
           "anyType" "anyURI" "attribute" "attributeGroup" "base64Binary"
           "boolean" "byte" "comment" "complexType" "date" "dateTime"
           "decimal" "documentation" "double" "duration" "element" "float"
           "gDay" "gMonth" "gMonthDay" "gYear" "gYearMonth" "hexBinary"
           "int" "integer" "language" "long" "negativeInteger"
           "nonNegativeInteger" "nonPositiveInteger" "normalizedString"
           "pi" "positiveInteger" "restriction" "schema" "short"
           "simpleType" "string" "time" "token" "unsignedByte"
           "unsignedInt" "unsignedLong" "unsignedShort")
  (:nicknames "xsd"))

(rdf:defvocabulary "xsd"
  :uri "http://www.w3.org/2001/XMLSchema-datatypes#"
  :package "http://www.w3.org/2001/XMLSchema-datatypes"
  :definitions ()

  :documentation "The XML schema datatypes vocabulary comprises the names for basic datatypes
 which may appear in RDF schemas. These are a subset of the terms present in the full XML schema
 vocabulary and are x-exported from there. The uri / package disjunction accounts for the disjunction
 between canonical namespace name[1],[2] and that used in RDF schema[3].
 ---
 [1]: http://www.w3.org/TR/xmlschema-1/#Instance_Document_Constructions
 [2]: http://www.w3.org/TR/xmlschema-1/#normative-schemaSchema
 [3]: http://www.w3.org/TR/swbp-xsch-datatypes/")

