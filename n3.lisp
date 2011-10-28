;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.resource.implementation; -*-


(in-package :de.setf.resource.implementation)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (intersection '(:digitool :clozure :allegro :sbcl :lispworks) *features*)
    (cerror "Compile it anyway." "This file lacks conditionalization for ~a."
            (lisp-implementation-type))))

(:documentation
  "This file defines an minimal n3 codec for the `de.setf.resource` CLOS linked data library."
 
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
  "'de.setf.resource' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.resource' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.resource' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/).")

 (description "See http://www.w3.org/DesignIssues/Notation3.html."))


(defpackage :n3
  (:use )
  (:export :format :format-property :print-property :with-context
           :*readtable* :read-resource :read :*read-resource*
           :open :with-open-stream :call-with-stream))

(defvar n3:*readtable* (copy-readtable))

(defparameter n3::*intern-literal* 'n3::decode-literal)

(defparameter n3::*intern-resource* 'n3::intern-resource-as-identifier)

(defparameter n3::*intern-blank-node* 'n3::intern-blank-node)

(defparameter n3::*construct-statement* 'list)

#+(or sbcl lispworks)
(defparameter *rapper-binary-pathname* "/usr/local/bin/rapper")
#+digitool
(defparameter *rapper-binary-pathname* "/usr/local/bin/rapper")

(defparameter *blank-node-prefix* "_:")

(defparameter *blank-nodes* (make-hash-table :test #'equal)
  "The global blnak node registry")

(defgeneric n3:format (object stream &optional colon at var)
  (:documentation "Given an N3 tuple, write it to the output stream.
 Enclude type information for any non-string encoded as a literal.")
  
  (:method ((stream stream) (object t) &optional colon at var)
    ;; flip the order
    (n3:format object stream colon at var))
  
  (:method ((object list) stream &optional colon at var)
    (declare (ignore colon var))
    (destructuring-bind (subject predicate object) object
      (write-char #\space stream)
      (n3:print-property subject stream)
      (write-char #\space stream)
      (n3:print-property predicate stream)
      (write-char #\space stream)
      (n3:print-property object stream)
      (unless at
        (write-string " ." stream)))
    object))


(defun n3:format-property (stream object &optional colon at var)
  (declare (ignore colon at var))
  (n3:print-property object stream))


(defgeneric n3:print-property (object stream)
  (:method ((object null) stream) (write-string "_:NIL" stream))
  (:method ((object string) stream)
    ;;!!! this need to properly escape
    (write object :stream stream))
  (:method ((object number) stream) (format stream "\"~d\"^^~/n3:format-property/" object (rdf-datatype-uri object)))

  (:method ((object symbol) stream)
    (let ((package (symbol-package object))
          (fragment (symbol-name object)))
      (if package
        (let ((base-uri (package-name package)))
          (format stream "<~a~@[~a~]~a>"
                  base-uri (uri-extrinsic-separator base-uri) fragment))
        (format stream "~a~a" *blank-node-prefix* fragment))))

  (:method ((object uuid:uuid) stream)
    (write-char #\< stream)
    (uuid::format-as-urn stream object)
    (write-char #\> stream))

  (:method ((object class) stream)
    (n3:print-property (class-name object) stream))

  #+de.setf.xml
  (:method ((object xqdm:abstract-name) stream)
    (let* ((nn (xqdm:namespace-name object))
           (fc (unless (find (char nn (1- (length n))) "#/") #\#)))
      (format stream "<~a~@[~c~]~a>"
              nn fc (xqdm:local-part object))))
  #+de.setf.xml
  (:method ((object xqdm:uri) stream)
    (format stream "<~a>" (uri-namestring object)))
  
  )


(defgeneric rdf-datatype-uri (object)
  (:method ((object integer)) '{xsd}integer)
  (:method ((object double-float)) '{xsd}double)
  (:method ((object float)) '{xsd}float)
  (:method ((object real)) '{xsd}decimal))


(defun n3::read-token (stream)
   (let ((buffer (make-array 10 :adjustable t :fill-pointer 0 :element-type 'character))
         (char #\null))
     (loop (case (setf char (read-char stream))
             ((#\space #\linefeed #\tab #\return)
              (return (subseq buffer 0 (length buffer))))
             (t
              (vector-push-extend char buffer))))))

     
(defun read-resource-as-string (stream start)
  (let ((buffer (make-array 10 :adjustable t :fill-pointer 0 :element-type 'character))
        (char #\null))
    (flet ((read-unicode-char (stream length)
             (let ((buffer (make-array length :element-type 'character)))
               (dotimes (i length)
                 (setf (char buffer i) (read-char stream)))
               (code-char (parse-integer buffer :radix 16)))))
      (loop (setf char (read-char stream))
            (case char
              (#\>
               (if (eql start #\<)
                 (return (subseq buffer 0 (length buffer)))
                 (vector-push-extend char buffer)))
              (#\"
               (ecase start
                 (#\" (return (subseq buffer 0 (length buffer))))
                 (:|"""|
                  (cond ((and (eql (setf char (read-char stream)) #\")
                              (eql (peek-char nil stream) #\"))
                         (read-char stream)
                         (return (subseq buffer 0 (length buffer))))
                        (t
                         (vector-push-extend #\" buffer)
                         (vector-push-extend char buffer))))
                 (#\<
                  (vector-push-extend #\" buffer))))
              ((#\tab #\linefeed #\return #\space)
               (unless (eql start #\<)
                 (vector-push-extend char buffer)))
              (#\\ (ecase (read-char stream)
                     (#\newline )
                     (#\\ (vector-push-extend #\\ buffer))
                     (#\' (vector-push-extend #\' buffer))
                     (#\" (vector-push-extend #\" buffer))
                     (#\b (vector-push-extend #\backspace buffer))
                     (#\n (vector-push-extend #\linefeed buffer))
                     (#\r (vector-push-extend #\return buffer))
                     (#\t (vector-push-extend #\tab buffer))
                     (#\u (vector-push-extend (read-unicode-char stream 4) buffer))
                     (#\U (vector-push-extend (read-unicode-char stream 8) buffer))))
              (t
               (vector-push-extend char buffer)))))))

(defun n3::intern-resource-as-identifier (string)
  (uri-namestring-identifier string))

(defun n3::intern-blank-node (string)
  (or (gethash string *blank-nodes*)
      (setf (gethash string *blank-nodes*)
            (make-symbol string))))

(defun n3::read-blank-node (stream char)
  (assert (eql (setf char (read-char stream)) #\:) ()
          "Invalid blank node syntax: ~s." char)
  (funcall n3::*intern-blank-node* (n3::read-token stream)))
  

(defun n3::decode-literal (string datatype language)
  (declare (ignore language))
  (if datatype
    (decode-literal-value string datatype)
    string))


(defun n3:read-resource (stream char)
  (funcall n3::*intern-resource* (read-resource-as-string stream char)))


(defun n3::read-literal (stream char)
  (declare (ignore char))
  (let ((literal nil)
        (datatype nil)
        (language nil))
    (case (peek-char nil stream)
      (#\"
       (read-char stream)
       (setf literal (cond ((eql (peek-char nil stream) #\")
                            (read-char stream)
                            (read-resource-as-string stream :|"""|))
                           (t
                            (setf literal "")))))
      (t
       (setf literal (read-resource-as-string stream #\"))))
    (loop (case (peek-char nil stream nil nil)
            (#\^
             ;; skip the ^ and the next, and save the read data type.
             ;; to the literal
             (read-char stream)
             (assert (eql (read-char stream) #\^) ()
                     "Invalid N3 encoding: ~s; missing datatype" stream)
             (setf datatype (read stream t nil t))
             (assert (identifier-p datatype) ()
                     "Invalid N3 encoding: ~s; datatype ~s." stream datatype))
            (#\@
             ;; skip the @, save the language token
             (read-char stream)
             (setf language (n3::read-token stream)))
            (t
             (return))))
    ;; pass through the literal string and whatever data type and language indicators
    ;; the read context may have collected to the active constructor.
    (funcall n3::*intern-literal* literal datatype language)))
            
    
      

(defun n3:read (stream &optional (eof-error-p t) (eof-value nil))
  (flet ((read-node (eofp)
           (read stream eofp stream)))
    (let* ((*readtable* n3:*readtable*)
           (subject (read-node eof-error-p)))
      (if (eq subject stream)
        eof-value
        (let ((predicate (read-node t))
              (object (read-node t)))
          (case (peek-char t stream)
            (#\.
             (read-char stream)
             (funcall n3::*construct-statement* subject predicate object))
            (t
             (error "Invalid N3 encoding: ~s; missing punctuation" stream))))))))

(defun n3::read-quad (stream &optional (eof-error-p t) (eof-value nil))
  (flet ((read-node (eofp)
           (read stream eofp stream)))
    (let* ((*readtable* n3:*readtable*)
           (subject (read-node eof-error-p)))
      (if (eq subject stream)
        eof-value
        (let ((predicate (read-node t))
              (object (read-node t)))
          (case (peek-char t stream)
            (#\.
             (read-char stream)
             (funcall n3::*construct-statement* subject predicate object nil))
            (t
             (let ((context (read-node t)))
               (case (peek-char t stream)
                 (#\.
                  (read-char stream)
                  (funcall n3::*construct-statement* subject predicate object context))
                 (t
                  (error "Invalid NQ encoding: ~s; missing punctuation" stream)))))))))))


(defgeneric decode-literal-value (string type) )

#+wilbur
(progn
  (let ((literal nil))
    (defun temp-literal ()
      (or literal (setf literal (wilbur:literal "")))))
  
  (defun wilbur-datatype (type)
    (declare (special *model-to-repository-datatype-map*))
    (or (gethash type *model-to-repository-datatype-map*)
        (setf (gethash type *model-to-repository-datatype-map*)
              (wilbur:node (symbol-uri-namestring type)))))
  
  (defmethod decode-literal-value (string (type symbol))
    (wilbur::compute-literal-value (temp-literal) (wilbur-datatype type) string))
  
  (defmethod decode-literal-value (string (type null))
    (wilbur::compute-literal-value (temp-literal) nil string))
  )


#-wilbur
(progn
  (defmethod decode-literal-value (string (type symbol))
    (cons type string))
  (defmethod decode-literal-value (string (type null))
    string)
  )

  

;;; set up after the definition. otherwise allegro fails with an undefined function error

(set-macro-character #\< 'n3:read-resource t n3:*readtable*)
(set-macro-character #\" 'n3::read-literal t n3:*readtable*)
(set-macro-character #\_ 'n3::read-blank-node t n3:*readtable*)


(:documentation "interface methods" n3:open n3:with-open-stream rdf:project-graph

 "The library provides i/o access to RDF streams encoded as notation3. The operator n3:read reads
 statements from the stream and n3:format writes statements to a stream. While notation3 is the only
 directly implemented encoding, others are available by wrapping a stream of the alternative encoding
 in a respective filter. In the case of file streams, this is accomplished by piping the data through
 raptor[1]. http locations are NYI.

 The control structure presumes that the encoding is to be infered from the location.
 Whice is customary for pathnames, but can be problematic for http sources. In that case, the decision
 must combine the URI with the content type from a response header in order to determine the value.

 ---
 [1]: http://librdf.org/raptor/rapper.html")


(defmacro n3:with-open-stream ((var location &rest args) &body body &aux (body-op (gensym)))
  `(flet ((,body-op (,var) ,@body))
    (declare (dynamic-extent #',body-op))
    (n3:call-with-stream #',body-op ,location ,@args)))


(defgeneric n3:call-with-stream (function location &rest args)
  (:method (function (location t) &rest args)
    "The method for pathnames acts like with-open-file, but uses n3:open to observe the mime-type"
    (let ((done nil)
          (stream nil))
      (unwind-protect
        (multiple-value-prog1
          (funcall function (setf stream (apply #'n3:open location args)))
          (setf done t))
        (when stream (close stream :abort (null done)))))))


(defun n3:open (location &rest args &key (mime-type (location-mime-type location) mt-s)
                         &allow-other-keys)
  (declare (dynamic-extent args))
  (when mt-s (remf args :mime-type))
  (apply #'n3::open-with-codec location mime-type args))


(defgeneric n3::open-with-codec (location encoding &rest args)
  
  (:method ((location pathname) (mime-type mime:application/n3) &rest args)
    "If the type is already notation3, then resurn the stream unadorned."
    (apply #'open location args))
  
  (:method ((location t) (mime-type mime:text/plain) &rest args)
    "The 'standard' notation3 mime type is, in fact, text/plain, so presume it's ok."
    (declare (dynamic-extent args))
    (apply #'n3::open-with-codec location mime:application/n3 args))
  
  #+digitool
  (:method ((location pathname) (mime-type mime:application/rdf+xml)
            &key (direction (error "direction is required."))
            &allow-other-keys)
    "Given an RDF source, pipe the input/output through a raptor process"
    (ecase direction
      (:input  (make-instance 'bsd:pipe-input-stream
                 :command `(,*rapper-binary-pathname* "-q" "-o ntriples" ,location)))
      (:output (make-instance 'bsd:pipe-output-stream
                 :command `(,*rapper-binary-pathname* "-q" "-i ntriples" "-o rdfxml" ,location)))))

  #+digitool
  (:method ((location pathname) (mime-type mime:text/turtle)
            &key (direction (error "direction is required."))
            &allow-other-keys)
    "Given a turtle source, pipe the input/output through a raptor process"
    (ecase direction
      (:input  (make-instance 'bsd:pipe-input-stream
                 :command `(,*rapper-binary-pathname* "-q" "-o ntriples" "-i turtle" ,location)))
      (:output (make-instance 'bsd:pipe-output-stream
                 :command `(,*rapper-binary-pathname* "-q" "-i ntriples" "-o turtle" ,location)))))

  #+clozure
  (:method ((location pathname) (mime-type mime:application/rdf+xml)
            &key (direction (error "direction is required."))
            &allow-other-keys)
    "Given an RDF source, pipe the input/output through a raptor process"
    (ecase direction
      (:input
       (ccl:run-program *rapper-binary-pathname*
                        `("-q" "-o" "ntriples" ,location)
                        :output :stream))
      (:output
       (ccl:run-program *rapper-binary-pathname*
                        `("-q" "-i" "ntriples" "-o" "rdfxml",location)
                        :input :stream))))
  
  #+allegro
  (:method ((location pathname) (mime-type mime:application/rdf+xml)
            &key (direction (error "direction is required."))
            &allow-other-keys)
    "Given an RDF source, pipe the input/output through a raptor process"
    (ecase direction
      (:input
       (excl:run-shell-command (format nil "~a -q -o ntriples ~a"
                                       *rapper-binary-pathname* location)
                               :output :stream))
      (:output
       (excl:run-shell-command (format nil "~a -q -i ntriples -o rdf/xml ~a"
                                       *rapper-binary-pathname* location)
                               :input :stream))))
  
  #+lispworks
  (:method ((location pathname) (mime-type mime:application/rdf+xml)
            &key (direction (error "direction is required."))
            &allow-other-keys)
    "Given an RDF source, pipe the input/output through a raptor process"
    (ecase direction
      (:input (system:run-shell-command (format nil "~a -q -o ntriples ~a" *rapper-binary-pathname* location)
                                        :output :stream))
      (:output (system:run-shell-command (format nil "~a -q -i ntriples -o rdf/xml ~a" *rapper-binary-pathname* location)
                                         :input :stream))))

  #+lispworks
  (:method ((location pathname) (mime-type mime:text/turtle)
            &key (direction (error "direction is required."))
            &allow-other-keys)
    "Given a turtle source, pipe the input/output through a raptor process"
    (ecase direction
      (:input (system:run-shell-command (format nil "~a -q -o ntriples -i turtle ~a" *rapper-binary-pathname* location)
                                        :output :stream))
      (:output (system:run-shell-command (format nil "~a -q -i ntriples -o turtle ~a" *rapper-binary-pathname* location)
                                         :input :stream))))

  #+sbcl
  (:method ((location pathname) (mime-type mime:application/rdf+xml)
            &key (direction (error "direction is required."))
            &allow-other-keys)
    "Given an RDF source, pipe the input/output through a raptor process"
    (ecase direction
      (:input (sb-ext:process-output (sb-ext:run-program *rapper-binary-pathname*
                                                         `("-q" "-o" "ntriples" ,(namestring (translate-logical-pathname location)))
                                                         :output :stream)))
      (:output (sb-ext:process-input (sb-ext:run-program *rapper-binary-pathname*
                                                          `("-q" "-i" "ntriples" "-o" "rdfxml" ,(namestring (translate-logical-pathname location)))
                                                          :input :stream)))))

  #+sbcl
  (:method ((location pathname) (mime-type mime:text/turtle)
            &key (direction (error "direction is required."))
            &allow-other-keys)
    "Given a turtle source, pipe the input/output through a raptor process"
    (ecase direction
      (:input (sb-ext:process-output (sb-ext:run-program *rapper-binary-pathname*
                                                         `("-q" "-o" "ntriples" "-i" "turtle" ,(namestring (translate-logical-pathname location)))
                                                         :output :stream)))
      (:output (sb-ext:process-input (sb-ext:run-program *rapper-binary-pathname*
                                                          `("-q" "-i" "ntriples" "-o" "turtle" ,(namestring (translate-logical-pathname location)))
                                                          :input :stream)))))

  )


(defmethod rdf:project-graph ((source pathname) (destination t))
  "Given a pathname, open it as an ntriple input stream, wrap it  with a continuation-based iterator
 and project that onto the destination."
  (n3:with-open-stream (n3-input-stream source :direction :input)
    (let ((count 0)
          (*blank-nodes* (make-hash-table :test #'equal)))
      (flet ((nt-reader (consumer)
               (let ((statement ()))
                 (loop (unless (setf statement (n3:read n3-input-stream nil nil))
                         (return))
                       (funcall consumer statement)
                       (incf count)))))
        ;; no dynamic extent, in case one tries to re-use it, the stream will have been closed,
        ;; but it still shouldn't crash
        (rdf:project-graph #'nt-reader destination))
      (values count source))))


(defmethod rdf:project-graph ((source t) (destination pathname))
  (n3:with-open-stream (n3-output-stream source :direction :output
                                         :if-exists :supersede :if-does-not-exist :create)
    (let ((count 0))
      (flet ((nt-writer (statement)
               (n3:format statement n3-output-stream)
               (terpri n3-output-stream)
               (incf count)))
        ;; no dynamic extent, in case one tries to re-use it, the stream will have been closed,
        ;; but it still shouldn't crash
        (rdf:project-graph source #'nt-writer))
      (values count destination))))

(:documentation
  "Count the statements in the 'Standard Thesaurus fuer Wirtschaft' (stw)
 [data](http://zbw.eu/stw/versions/latest/download/about)."
  (let ((i 0))
    (rdf:project-graph #P"LIBRARY:examples;data;stw.nt"
                       #'(lambda (statement)
                           (incf i)
                           (when (zerop (mod i 10000))
                             (format *trace-output* "~&~6,'0d ~s" i statement)
                             (finish-output *trace-output*))))
    i))

(:documentation
  "Return the class names referenced in the stw data." 
  (let ((classes ())
        (count 0))
    (rdf:project-graph #P"LIBRARY:examples;data;stw.nt"
                       #'(lambda (statement)
                           (destructuring-bind (s p o) statement
                             (when (and (eq p '{rdf}type) (member o '({rdfs}Class {owl}Class)))
                               (incf count)
                               (setf (getf classes s) 0)))))
    (rdf:project-graph #P"LIBRARY:examples;data;stw.nt"
                       #'(lambda (statement)
                           (destructuring-bind (s p o) statement
                             (declare (ignore s))
                             (when (and (eq p '{rdf}type) (member o classes))
                               (incf (getf classes o))))))
    (values count
            classes)))

(:documentation
  "Return a list of all types used in the stw data."
  (let ((types ()))
    (rdf:project-graph #P"LIBRARY:examples;data;stw.nt"
                       #'(lambda (statement)
                           (destructuring-bind (s p o) statement
                             (declare (ignore s))
                             (when (eq p '{rdf}type)
                               (pushnew o types)))))
    (sort types #'string-lessp)))


(:documentation
  "print the staements in a file encoded as RDF."
  (dolist (pathname (directory #P"LIBRARY:examples;data;dublincore;example*.rdf"))
    (format t "~&~%;;; ~s" pathname)
    (rdf:project-graph pathname #'print)))