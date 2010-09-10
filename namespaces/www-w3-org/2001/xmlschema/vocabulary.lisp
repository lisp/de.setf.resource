;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: common-lisp-user; -*-

;;; (rdfs:require "http://www.w3.org/2001/XMLSchema#")

(in-package :common-lisp-user)


(defpackage "http://www.w3.org/2001/XMLSchema"
  (:use)
  (:export "all" "any" "anyAttribute" "appinfo" "choice"
           "complexContent" "enumeration" "extension" "field"
           "fractionDigits" "group" "import" "include" "key" "keyref"
           "length" "list" "maxExclusive" "maxInclusive" "maxLength"
           "minExclusive" "minInclusive" "minLength" "notation" "pattern"
           "redefine" "selector" "sequence" "simpleContent" "totalDigits"
           "union" "unique" "whiteSpace"

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
  (:nicknames "xs"))

(rdf:defvocabulary "xs"
  :uri "http://www.w3.org/2001/XMLSchema#"
  :package "http://www.w3.org/2001/XMLSchema"
  :definitions ()

  :documentation "The XML schema datatypes vocabulary comprises the full complement of
 terms which may appear in an XML schema document. A subset of them is x-exported
 into the schema datatypes vocabulary.")
