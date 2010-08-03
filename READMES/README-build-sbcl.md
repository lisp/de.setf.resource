
DE.SETF.RESOURCE: how to build it with Steel Bank Common Lisp
----------------

In order to use asdf with sbcl, 

The system can be built and saved from the command line. This example illustrates a build which
incorporates the cassandra interface.

    # see the file in READMES
    $ sbcl --userinit /development/source/library/build-init.lisp \ 
      --eval "(asdf:operate 'asdf:load-op :de.setf.resource.cassandra)" \
      --eval '(sb-ext:save-lisp-and-die "sbcl-rdf.core")'

Start it with the core

    $ sbcl --core sbcl-rdf.core
    This is SBCL 1.0.35, an implementation of ANSI Common Lisp.
    More information about SBCL is available at <http://www.sbcl.org/>.
    
    SBCL is free software, provided as is, with absolutely no warranty.
    It is mostly in the public domain; some portions are provided under
    BSD-style licenses.  See the CREDITS and COPYING files in the
    distribution for more information.
    * (defparameter *c* (make-instance 'amqp:connection :uri "amqp://guest:guest@localhost/"))
    
    *C*
    * (defparameter *rdf-store* (cassandra-conection))
    
    *RDF-STORE*
    * (dsc:keyspace-version *rdf-store*)

    "0.6.4"
    * 
