;;; -*- -*-

(common-lisp:defpackage "http://xmlns.com/foaf/0.1/"
  (:use )
  (:nicknames "foaf")
  (:export
   "account"
   "accountName"
   "accountServiceHomepage"
   "age"
   "Agent"
   "aimChatID"
   "based_near"
   "birthday"
   "currentProject"
   "depiction"
   "depicts"
   "dnaChecksum"
   "Document"
   "family_name"
   "familyName"
   "firstName"
   "fundedBy"
   "geekcode"
   "gender"
   "givenname"
   "givenName"
   "Group"
   "holdsAccount"
   "homepage"
   "icqChatID"
   "Image"
   "img"
   "interest"
   "isPrimaryTopicOf"
   "jabberID"
   "knows"
   "LabelProperty"
   "lastName"
   "logo"
   "made"
   "maker"
   "mbox"
   "mbox_sha1sum"
   "member"
   "membershipClass"
   "msnChatID"
   "myersBriggs"
   "name"
   "nick"
   "OnlineAccount"
   "OnlineChatAccount"
   "OnlineEcommerceAccount"
   "OnlineGamingAccount"
   "openid"
   "Organization"
   "page"
   "pastProject"
   "Person"
   "PersonalProfileDocument"
   "phone"
   "plan"
   "primaryTopic"
   "Project"
   "publications"
   "schoolHomepage"
   "sha1"
   "skypeID"
   "status"
   "surname"
   "theme"
   "thumbnail"
   "tipjar"
   "title"
   "topic"
   "topic_interest"
   "weblog"
   "workInfoHomepage"
   "workplaceHomepage"
   "yahooChatID"))

(common-lisp:in-package "http://xmlns.com/foaf/0.1/")

(de.setf.resource.schema:require-vocabulary "http://www.w3.org/2000/10/swap/pim/contact#")
(de.setf.resource.schema:require-vocabulary "http://www.w3.org/2003/01/geo/wgs84_pos#")

(de.setf.resource.schema:defvocabulary "foaf"
  :uri "http://xmlns.com/foaf/0.1/"
  :definitions
  ((de.setf.resource.schema:defclass |Agent|
     nil
     ((|mbox| :type
              |http://www.w3.org/2002/07/owl#|:|Thing|
              :documentation
              "A personal mailbox, ie. an Internet mailbox associated with exactly one owner, the first owner of this mailbox. This is a 'static inverse functional property', in that there is (across time and change) at most one individual that ever has any particular value for foaf:mbox.")
      (|mbox_sha1sum| :type
                      |http://www.w3.org/2000/01/rdf-schema#|:|Literal|
                      :documentation
                      "The sha1sum of the URI of an Internet mailbox associated with exactly one owner, the first owner of the mailbox.")
      (|gender| :type
                |http://www.w3.org/2000/01/rdf-schema#|:|Literal|
                :documentation
                "The gender of this Agent (typically but not necessarily 'male' or 'female').")
      (|jabberID| :type
                  |http://www.w3.org/2000/01/rdf-schema#|:|Literal|
                  :documentation
                  "A jabber ID for something.")
      (|aimChatID| :type
                   |http://www.w3.org/2000/01/rdf-schema#|:|Literal|
                   :documentation "An AIM chat ID")
      (|skypeID| :type
                 |http://www.w3.org/2000/01/rdf-schema#|:|Literal|
                 :documentation "A Skype ID")
      (|icqChatID| :type
                   |http://www.w3.org/2000/01/rdf-schema#|:|Literal|
                   :documentation "An ICQ chat ID")
      (|yahooChatID| :type
                     |http://www.w3.org/2000/01/rdf-schema#|:|Literal|
                     :documentation "A Yahoo chat ID")
      (|msnChatID| :type
                   |http://www.w3.org/2000/01/rdf-schema#|:|Literal|
                   :documentation "An MSN chat ID")
      (|weblog| :type |Document| :documentation
                "A weblog of some thing (whether person, group, company etc.).")
      (|openid| :type |Document| :documentation
                "An OpenID for an Agent.")
      (|tipjar| :type |Document| :documentation
                "A tipjar document for this agent, describing means for payment and reward.")
      (|made| :type
              |http://www.w3.org/2002/07/owl#|:|Thing|
              :documentation
              "Something that was made by this agent.")
      (|account| :type |OnlineAccount|
                 :documentation
                 "Indicates an account held by this agent.")
      (|holdsAccount| :type |OnlineAccount|
                      :documentation
                      "Indicates an account held by this agent.")
      (|birthday| :type
                  |http://www.w3.org/2000/01/rdf-schema#|:|Literal|
                  :documentation
                  "The birthday of this Agent, represented in mm-dd string form, eg. '12-31'.")
      (|age| :type
             |http://www.w3.org/2000/01/rdf-schema#|:|Literal|
             :documentation
             "The age in years of some agent.")
      (|status| :type
                |http://www.w3.org/2000/01/rdf-schema#|:|Literal|
                :documentation
                "A string expressing what the user is happy for the general public (normally) to know about their current activity."))
     (:documentation
       "An agent (eg. person, group, software or physical artifact)."))
   
   (de.setf.resource.schema:defclass |Document|
     nil
     ((|sha1| :type
              |http://www.w3.org/2000/01/rdf-schema#|:|Literal|
              :documentation "A sha1sum hash, in hex.")
      (|topic| :type
               |http://www.w3.org/2002/07/owl#|:|Thing|
               :documentation
               "A topic of some page or document.")
      (|primaryTopic| :type
                      |http://www.w3.org/2002/07/owl#|:|Thing|
                      :documentation
                      "The primary topic of some page or document."))
     (:documentation "A document."))
   
   (de.setf.resource.schema:defclass |Group|
     (|Agent|)
     ((|member| :type |Agent| :documentation
                "Indicates a member of a Group"))
     (:documentation "A class of Agents."))
   
   (de.setf.resource.schema:defclass |http://www.w3.org/2000/01/rdf-schema#Class|
     nil
     nil)
   
   (de.setf.resource.schema:defclass |Image|
     nil
     ((|depicts| :type
                 |http://www.w3.org/2002/07/owl#|:|Thing|
                 :documentation
                 "A thing depicted in this representation.")
      (|thumbnail| :type |Image| :documentation
                   "A derived thumbnail image."))
     (:documentation "An image."))
   
   (de.setf.resource.schema:defclass |LabelProperty| nil nil)
   
   (de.setf.resource.schema:defclass |OnlineAccount|
     (|http://www.w3.org/2002/07/owl#|:|Thing|)
     ((|accountServiceHomepage| :type |Document|
                                :documentation
                                "Indicates a homepage of the service provide for this online account.")
      (|accountName| :type
                     |http://www.w3.org/2000/01/rdf-schema#|:|Literal|
                     :documentation
                     "Indicates the name (identifier) associated with this online account."))
     (:documentation "An online account."))
   
   (de.setf.resource.schema:defclass |OnlineChatAccount|
     (|OnlineAccount|)
     nil
     (:documentation "An online chat account."))
   
   (de.setf.resource.schema:defclass |OnlineEcommerceAccount|
     (|OnlineAccount|)
     nil
     (:documentation
       "An online e-commerce account."))
   
   (de.setf.resource.schema:defclass |OnlineGamingAccount|
     (|OnlineAccount|)
     nil
     (:documentation "An online gaming account."))
   
   (de.setf.resource.schema:defclass |Organization|
     (|Agent|)
     nil
     (:documentation "An organization."))
   
   (de.setf.resource.schema:defclass |Person|
     (|Agent|
      |http://www.w3.org/2000/10/swap/pim/contact#|:|Person|
      |http://www.w3.org/2003/01/geo/wgs84_pos#|:|SpatialThing|)
     ((|geekcode| :type
                  |http://www.w3.org/2000/01/rdf-schema#|:|Literal|
                  :documentation
                  "A textual geekcode for this person, see http://www.geekcode.com/geek.html")
      (|firstName| :type
                   |http://www.w3.org/2000/01/rdf-schema#|:|Literal|
                   :documentation
                   "The first name of a person.")
      (|surname| :type
                 |http://www.w3.org/2000/01/rdf-schema#|:|Literal|
                 :documentation
                 "The surname of some person.")
      (|family_name| :type
                     |http://www.w3.org/2000/01/rdf-schema#|:|Literal|
                     :documentation
                     "The family name of some person.")
      (|familyName| :type
                    |http://www.w3.org/2000/01/rdf-schema#|:|Literal|
                    :documentation
                    "The family name of some person.")
      (|plan| :type
              |http://www.w3.org/2000/01/rdf-schema#|:|Literal|
              :documentation
              "A .plan comment, in the tradition of finger and '.plan' files.")
      (|img| :type |Image| :documentation
             "An image that can be used to represent some thing (ie. those depictions which are particularly representative of something, eg. one's photo on a homepage).")
      (|myersBriggs| :type
                     |http://www.w3.org/2000/01/rdf-schema#|:|Literal|
                     :documentation
                     "A Myers Briggs (MBTI) personality classification.")
      (|workplaceHomepage| :type |Document|
                           :documentation
                           "A workplace homepage of some person; the homepage of an organization they work for.")
      (|workInfoHomepage| :type |Document|
                          :documentation
                          "A work info homepage of some person; a page about their work for some organization.")
      (|schoolHomepage| :type |Document|
                        :documentation
                        "A homepage of a school attended by the person.")
      (|knows| :type |Person| :documentation
               "A person known by this person (indicating some level of reciprocated interaction between the parties).")
      (|interest| :type |Document| :documentation
                  "A page about a topic of interest to this person.")
      (|topic_interest| :type
                        |http://www.w3.org/2002/07/owl#|:|Thing|
                        :documentation
                        "A thing of interest to this person.")
      (|publications| :type |Document|
                      :documentation
                      "A link to the publications of this person.")
      (|currentProject| :type
                        |http://www.w3.org/2002/07/owl#|:|Thing|
                        :documentation
                        "A current project this person works on.")
      (|pastProject| :type
                     |http://www.w3.org/2002/07/owl#|:|Thing|
                     :documentation
                     "A project this person has previously worked on."))
     (:documentation "A person."))
   
   (de.setf.resource.schema:defclass |PersonalProfileDocument|
     (|Document|)
     nil
     (:documentation
       "A personal profile RDF document."))
   
   (de.setf.resource.schema:defclass |Project|
     nil
     nil
     (:documentation
       "A project (a collective endeavour of some kind)."))
   ))
