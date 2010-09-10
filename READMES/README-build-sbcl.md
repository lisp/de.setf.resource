
DE.SETF.RESOURCE: how to build it with Steel Bank Common Lisp
----------------

In order to use asdf with sbcl, 

The system can be built and saved from the command line. This example illustrates a build which
incorporates the cassandra interface.

    # see the file in READMES
    $ sbcl --userinit /development/source/library/build-init.lisp \
      --eval "(asdf:operate 'asdf:load-op :de.setf.resource.cassandra)" \
      --eval '(sb-ext:save-lisp-and-die "sbcl-rdf.core")'

In principle, that suffices. In practice, it turned out that wilbur's non-ccl implementation relied on portable
allegroserve. ok. of that, the most recent version located by the cliki was that of sourceforge, which failed on
several counts:

 - Turns out to depend on three systems: aserve and two compatibility modules. As if they were independent.
 - portableaserve/acl-compat/sbcl/acl-mp.lisp :
    Lock on package SB-VM violated when interning THREAD-PID-SLOT.
    Symbol "CURRENT-THREAD-ID" not found in the SB-THREAD package.

 - portableaserve/acl-compat/sbcl/acl-excl.lisp:
    Lock on package SB-EXT violated when setting the symbol-function of WITHOUT-PACKAGE-LOCKS.
    Symbol "UNIX-FILE-KIND" not found in the SB-UNIX package.

Given which, one wonders if the port remained at all compatible with SBCL evolution.
All just to perform an HTTP get. Like, open a socket, write, read, close.
It was less work to port the clozure/mcl code to use usocket. (see :de.setf.wilbur)


Start it with the core

    * (test:execute-test :resource.**)

    ;Loading RDF: "file:///ebs/source/library/net/sourceforge/wilbur/schemata/true-rdf-schema.rdf"...done.

    ;Loading RDF: "http://xmlns.com/foaf/0.1/"...
    done.
    ; #<HTTP-URL "http://xmlns.com/foaf/0.1/">: 200

    :PASSED
    90
    0
    15
    0
    * (defparameter *rdf-store* (cassandra-conection))
    
    *RDF-STORE*
    * (dsc:keyspace-version *rdf-store*)

    "0.6.4"
    * 
