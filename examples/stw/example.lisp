;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.resource.implementation; -*-


(in-package :de.setf.resource.implementation)

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
        (de.setf.rdf:project-graph #'nt-reader destination))
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
        (de.setf.rdf:project-graph source #'nt-writer))
      (values count destination))))

(:documentation
  "Count the statements in the 'Standard Thesaurus fuer Wirtschaft' (stw)
 [data](http://zbw.eu/stw/versions/latest/download/about)."
  (let ((i 0))
    (de.setf.rdf:project-graph #P"LIBRARY:examples;data;stw.nt"
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
    (de.setf.rdf:project-graph #P"LIBRARY:examples;data;stw.nt"
                       #'(lambda (statement)
                           (destructuring-bind (s p o) statement
                             (when (and (eq p '{rdf}type) (member o '({rdfs}Class {owl}Class)))
                               (incf count)
                               (setf (getf classes s) 0)))))
    (de.setf.rdf:project-graph #P"LIBRARY:examples;data;stw.nt"
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
    (de.setf.rdf:project-graph #P"LIBRARY:examples;data;stw.nt"
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
    (de.setf.rdf:project-graph pathname #'print)))