;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.resource.implementation; -*-

(in-package :de.setf.resource.implementation)

(defstruct rdf:triple
  subject predicate object id)

(defstruct (rdf:quad (:include rdf:triple))
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


(defun rdf:triple (subject predicate object &optional id)
  (make-triple :subject subject :predicate predicate :object object :id id))

(defun rdf:quad (subject predicate object context &optional id)
  (make-quad :subject subject :predicate predicate :object object :context context :id id))

(defmethod print-object ((object rdf:triple) stream)
  (print-triple object stream 1))

(defgeneric rdf:valid? (statement)
  (:method ((statement triple))
    (when (and (triple-subject statement)
               (triple-predicate statement)
               (triple-object statement))
      t)))


(defgeneric rdf:namestring (statement)
  (:method ((statement triple))
    (with-output-to-string (stream)
      (format stream "~@/n3:format/" statement))))

(defmethod n3:format ((object rdf:triple) stream  &optional colon at var)
  (declare (ignore colon var))
  (unless at (write-char #\space stream))
  (n3:print-property (triple-subject object) stream)
  (write-char #\space stream)
  (n3:print-property (triple-predicate object) stream)
  (write-char #\space stream)
  (n3:print-property (triple-object object) stream)
  (unless at (write-string " ." stream))
  object)


(defmethod rdf:subject ((triple rdf:triple)) (triple-subject triple))
(defmethod rdf:subject-value ((source t) (triple rdf:triple)) (triple-subject triple))
(defmethod rdf:predicate ((triple rdf:triple)) (triple-predicate triple))
(defmethod rdf:predicate-value ((source t) (triple rdf:triple)) (triple-predicate triple))
(defmethod rdf:object ((triple rdf:triple)) (triple-object triple))
(defmethod rdf:object-value ((source t) (triple rdf:triple)) (triple-object triple))
(defmethod rdf:context ((triple rdf:triple)) nil)
(defmethod rdf:context ((quad rdf:quad)) (quad-context quad))
(defmethod rdf:id ((triple rdf:triple)) (triple-id triple))


(deftype rdf:statement ()
  "An abstract type which distinguishes the various rdf assertion classes which may be present.
 These include the library's own triple type, and the statement types from any concrete reopsitory"
  '(satisfies rdf:statement-p))

(defgeneric rdf:statement-p (object)
  (:method ((object t)) nil)
  (:method ((object rdf:triple)) t))

(defmethod rdf:equal ((s1 rdf:triple) (s2 rdf:triple))
  (or (equalp s1 s2)
      (equal (triple-id s1) (triple-id s2))
      (and (rdf:equal (triple-subject s1) (triple-subject s2))
           (rdf:equal (triple-predicate s1) (triple-predicate s2))
           (rdf:equal (triple-object s1) (triple-object s2))
           )))

(defmethod rdf:equal ((s1 rdf:quad) (s2 rdf:quad))
  (or (equalp s1 s2)
      (equal (triple-id s1) (triple-id s2))
      (and (rdf:equal (triple-subject s1) (triple-subject s2))
           (rdf:equal (triple-predicate s1) (triple-predicate s2))
           (rdf:equal (triple-object s1) (triple-object s2))
           (rdf:equal (quad-context s1) (quad-context s2)))))

(defgeneric copy-statement (statement)
  (:method ((statement rdf:triple))
    (copy-triple statement))
  (:method ((statement rdf:quad))
    (copy-quad statement)))