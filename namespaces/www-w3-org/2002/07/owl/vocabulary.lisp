;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: common-lisp-user; -*-

(in-package :common-lisp-user)

(defpackage "http://www.w3.org/2002/07/owl#"
  (:use )
  (:nicknames "owl")
  (:export "AllDifferent" "AllDisjointClasses"
           "AllDisjointProperties" "allValuesFrom"
           "annotatedProperty" "annotatedSource" "annotatedTarget"
           "Annotation" "AnnotationProperty" "assertionProperty"
           "AsymmetricProperty" "Axiom" "cardinality" "Class"
           "complementOf" "DataRange" "datatypeComplementOf"
           "DatatypeProperty" "DeprecatedClass" "DeprecatedProperty"
           "differentFrom" "disjointUnionOf" "disjointWith"
           "distinctMembers" "equivalentClass" "equivalentProperty"
           "FunctionalProperty" "hasKey" "hasSelf" "hasValue"
           "intersectionOf" "InverseFunctionalProperty" "inverseOf"
           "IrreflexiveProperty" "maxCardinality"
           "maxQualifiedCardinality" "members" "minCardinality"
           "minQualifiedCardinality" "NamedIndividual"
           "NegativePropertyAssertion" "ObjectProperty" "onClass"
           "onDataRange" "onDatatype" "oneOf" "onProperties"
           "onProperty" "Ontology" "OntologyProperty"
           "propertyChainAxiom" "propertyDisjointWith"
           "qualifiedCardinality" "ReflexiveProperty" "Restriction"
           "sameAs" "someValuesFrom" "sourceIndividual"
           "SymmetricProperty" "targetIndividual" "targetValue"
           "Thing"
           "TransitiveProperty" "unionOf" "withRestrictions"))

(rdfs:defvocabulary "owl"
  :uri "http://www.w3.org/2002/07/owl#"
  :package "http://www.w3.org/2002/07/owl#"
  :definitions ())


;;; (rdfs:require "http://www.w3.org/2002/07/owl#")
