    ;;; -*- Package: de.setf.resource.implementation -*-

    * (asdf:load-system :de.setf.resource.cassandra)

    * (defparameter *rdf-store* (cassandra-conection))

    *RDF-STORE*
    * (dsc:keyspace-version *rdf-store*)

    "0.6.4"
    * (dsc:keyspace-name *rdf-store*)

    "SPOC"
    * (rdf:insert-statement (rdf:triple  #u"http://rdf.rubyforge.org/"
                                         #u"http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
                                         #u"http://usefulinc.com/ns/doap#Project")
                            *rdf-store*)

    #(124 223 237 229 151 26 142 67 232 240 204 53 157 79 63 13)
    * (rdf:insert-statement (rdf:triple  #u"http://rdf.rubyforge.org/"
                                         #u"http://usefulinc.com/ns/doap#developer"
                                         #u"http://ar.to/#self")
                            *rdf-store*)

    #(170 58 20 14 174 54 252 49 220 96 78 2 124 166 67 181)
    * (rdf:insert-statement (rdf:triple  #u"http://rdf.rubyforge.org/"
                                       #u"http://usefulinc.com/ns/doap#developer"
                                       #u"http://bhuga.net/#ben")
                          *rdf-store*)

    #(246 254 234 244 70 16 107 221 106 205 91 130 95 75 183 123)
    * (rdf:insert-statement (rdf:triple  #u"http://ar.to/#self"
                                       #u"http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
                                       #u"http://xmlns.com/foaf/0.1/Person")
                          *rdf-store*)

    #(88 140 93 122 115 55 80 43 129 94 247 220 93 68 23 129)
    * (rdf:insert-statement (rdf:triple  #u"http://ar.to/#self"
                                       #u"http://xmlns.com/foaf/0.1/name"
                                       "Arto Bendiken")
                          *rdf-store*)

    #(141 127 67 86 163 254 8 48 193 76 169 243 104 224 254 154)
    * (rdf:insert-statement (rdf:triple  #u"http://bhuga.net/#ben"
                                       #u"http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
                                       #u"http://xmlns.com/foaf/0.1/Person")
                          *rdf-store*)

    #(25 38 18 63 141 116 62 161 22 207 130 26 144 169 177 144)
    * (rdf:insert-statement (rdf:triple  #u"http://ar.to/#self"
                                       #u"http://xmlns.com/foaf/0.1/name"
                                       "Ben Lavender")
                          *rdf-store*)

    #(67 37 142 6 53 237 28 207 148 118 35 74 139 58 255 48)
    * (rdf:query *rdf-store* :subject #u"http://rdf.rubyforge.org/")

    (#s(QUAD #<PURI:URI http://rdf.rubyforge.org/>
             #<PURI:URI http://usefulinc.com/ns/doap#developer>
             #<PURI:URI http://ar.to/#self>
             "urn:sha1:da39a3ee5e6b4b0d3255bfef95601890afd80709"
             #(170 58 20 14 174 54 252 49 220 96 78 2 124 166 67 181))
     #s(QUAD #<PURI:URI http://rdf.rubyforge.org/>
             #<PURI:URI http://usefulinc.com/ns/doap#developer>
             #<PURI:URI http://bhuga.net/#ben>
             "urn:sha1:da39a3ee5e6b4b0d3255bfef95601890afd80709"
             #(246 254 234 244 70 16 107 221 106 205 91 130 95 75 183 123))
     #s(QUAD #<PURI:URI http://rdf.rubyforge.org/>
             #<PURI:URI http://www.w3.org/1999/02/22-rdf-syntax-ns#type>
             #<PURI:URI http://usefulinc.com/ns/doap#Project>
             "urn:sha1:da39a3ee5e6b4b0d3255bfef95601890afd80709"
             #(124 223 237 229 151 26 142 67 232 240 204 53 157 79 63 13)))
    *