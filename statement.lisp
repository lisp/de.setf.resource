;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.resource.implementation; -*-

(in-package :de.setf.resource.implementation)

(defstruct de.setf.rdf:triple
  subject predicate object id)

(defstruct (de.setf.rdf:quad (:include de.setf.rdf:triple))
  context)

(defun print-triple (triple stream level)
  (unless (and *print-level* (>= level *print-level*))
    (format stream "#s(~s ~s ~s ~s"
            (type-of triple)
            (triple-subject triple)
            (triple-predicate triple)
            (triple-object triple))
    (when (quad-p triple)
      (format stream " ~s" (quad-context triple)))
    (format stream "~@[ ~s~]" (triple-id triple))
    (write-char #\) stream))
  triple)


(defun de.setf.rdf:triple (subject predicate object &optional id)
  (make-triple :subject subject :predicate predicate :object object :id id))

(defun de.setf.rdf:quad (subject predicate object context &optional id)
  (make-quad :subject subject :predicate predicate :object object :context context :id id))

(defmethod print-object ((object de.setf.rdf:triple) stream)
  (print-triple object stream 1))

(defgeneric de.setf.rdf:valid? (statement)
  (:method ((statement triple))
    (when (and (triple-subject statement)
               (triple-predicate statement)
               (triple-object statement))
      t)))


(defgeneric de.setf.rdf:namestring (statement)
  (:method ((statement triple))
    (with-output-to-string (stream)
      (format stream "~@/n3:format/" statement))))

(defmethod n3:format ((object de.setf.rdf:triple) stream  &optional colon at var)
  (declare (ignore colon var))
  (unless at (write-char #\space stream))
  (n3:print-property (triple-subject object) stream)
  (write-char #\space stream)
  (n3:print-property (triple-predicate object) stream)
  (write-char #\space stream)
  (n3:print-property (triple-object object) stream)
  (unless at (write-string " ." stream))
  object)


(defmethod de.setf.rdf:subject ((triple de.setf.rdf:triple)) (triple-subject triple))
(defmethod de.setf.rdf:subject-value ((source t) (triple de.setf.rdf:triple)) (triple-subject triple))
(defmethod de.setf.rdf:predicate ((triple de.setf.rdf:triple)) (triple-predicate triple))
(defmethod de.setf.rdf:predicate-value ((source t) (triple de.setf.rdf:triple)) (triple-predicate triple))
(defmethod de.setf.rdf:object ((triple de.setf.rdf:triple)) (triple-object triple))
(defmethod de.setf.rdf:object-value ((source t) (triple de.setf.rdf:triple)) (triple-object triple))
(defmethod de.setf.rdf:context ((triple de.setf.rdf:triple)) nil)
(defmethod de.setf.rdf:context ((quad de.setf.rdf:quad)) (quad-context quad))
(defmethod de.setf.rdf:id ((triple de.setf.rdf:triple)) (triple-id triple))


(deftype de.setf.rdf:statement ()
  "An abstract type which distinguishes the various rdf assertion classes which may be present.
 These include the library's own triple type, and the statement types from any concrete reopsitory"
  '(satisfies de.setf.rdf:statement-p))

(defgeneric de.setf.rdf:statement-p (object)
  (:method ((object t)) nil)
  (:method ((object de.setf.rdf:triple)) t))

(defmethod de.setf.rdf:equal ((s1 de.setf.rdf:triple) (s2 de.setf.rdf:triple))
  (or (equalp s1 s2)
      (equal (triple-id s1) (triple-id s2))
      (and (de.setf.rdf:equal (triple-subject s1) (triple-subject s2))
           (de.setf.rdf:equal (triple-predicate s1) (triple-predicate s2))
           (de.setf.rdf:equal (triple-object s1) (triple-object s2))
           )))

(defmethod de.setf.rdf:equal ((s1 de.setf.rdf:quad) (s2 de.setf.rdf:quad))
  (or (equalp s1 s2)
      (equal (triple-id s1) (triple-id s2))
      (and (de.setf.rdf:equal (triple-subject s1) (triple-subject s2))
           (de.setf.rdf:equal (triple-predicate s1) (triple-predicate s2))
           (de.setf.rdf:equal (triple-object s1) (triple-object s2))
           (de.setf.rdf:equal (quad-context s1) (quad-context s2)))))

(defgeneric copy-statement (statement)
  (:method ((statement de.setf.rdf:triple))
    (copy-triple statement))
  (:method ((statement de.setf.rdf:quad))
    (copy-quad statement)))