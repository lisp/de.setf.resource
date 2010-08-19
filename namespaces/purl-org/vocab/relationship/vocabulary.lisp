;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: common-lisp-user; -*-
;;; from #<doc-node http://vocab.org/relationship/.rdf #x30C6BA0E>

(common-lisp:in-package :common-lisp-user)

(common-lisp:defpackage "http://purl.org/vocab/relationship/"
  (:use)
  (:nicknames "rel")
  (:export
   "acquaintanceOf"
   "ambivalentOf"
   "ancestorOf"
   "antagonistOf"
   "apprenticeTo"
   "childOf"
   "closeFriendOf"
   "collaboratesWith"
   "colleagueOf"
   "descendantOf"
   "employedBy"
   "employerOf"
   "enemyOf"
   "engagedTo"
   "friendOf"
   "grandchildOf"
   "grandparentOf"
   "hasMet"
   "influencedBy"
   "knowsByReputation"
   "knowsInPassing"
   "knowsOf"
   "lifePartnerOf"
   "livesWith"
   "lostContactWith"
   "mentorOf"
   "neighborOf"
   "parentOf"
   "participant"
   "participantIn"
   "Relationship"
   "siblingOf"
   "spouseOf"
   "worksWith"
   "wouldLikeToKnow")
  (:documentation nil))

(common-lisp:in-package "http://purl.org/vocab/relationship/")

(rdfs:defvocabulary "rel"
  :uri "http://purl.org/vocab/relationship/"
  :definitions
  ((de.setf.resource.schema::extclass |http://xmlns.com/foaf/0.1/|:|Person| nil
                                     ((|parentOf| :type |http://xmlns.com/foaf/0.1/|:|Person|)
                                      (|childOf| :type |http://xmlns.com/foaf/0.1/|:|Person|)
                                      (|grandchildOf| :type |http://xmlns.com/foaf/0.1/|:|Person|)
                                      (|enemyOf| :type |http://xmlns.com/foaf/0.1/|:|Person|)
                                      (|antagonistOf| :type |http://xmlns.com/foaf/0.1/|:|Person|)
                                      (|ambivalentOf| :type |http://xmlns.com/foaf/0.1/|:|Person|)
                                      (|knowsOf| :type |http://xmlns.com/foaf/0.1/|:|Person|)
                                      (|wouldLikeToKnow| :type |http://xmlns.com/foaf/0.1/|:|Person|)
                                      (|knowsInPassing| :type |http://xmlns.com/foaf/0.1/|:|Person|)
                                      (|knowsByReputation| :type |http://xmlns.com/foaf/0.1/|:|Person|)
                                      (|employerOf| :type |http://xmlns.com/foaf/0.1/|:|Person|)
                                      (|employedBy| :type |http://xmlns.com/foaf/0.1/|:|Person|)
                                      (|mentorOf| :type |http://xmlns.com/foaf/0.1/|:|Person|)
                                      (|apprenticeTo| :type |http://xmlns.com/foaf/0.1/|:|Person|)
                                      (|grandparentOf| :type |http://xmlns.com/foaf/0.1/|:|Person|)
                                      (|participantIn| :type |Relationship|)
                                      (|influencedBy| :type |http://xmlns.com/foaf/0.1/|:|Person|)))

  (de.setf.resource.schema:defclass |Relationship|
    nil
    ((|participant| :type
                    |http://xmlns.com/foaf/0.1/|:|Person|)))))



