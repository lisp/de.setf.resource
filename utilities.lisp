;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.resource.implementation; -*-


(in-package :de.setf.resource.implementation)

(:documentation
  "This file defines the utilities for the `de.setf.resource` CLOS linked data library."
 
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
  "'de.setf.resource' is free software: you can redistribute it and/or modify
 it under the terms of version 3 of the GNU Lesser General Public License as published by
 the Free Software Foundation.

 'de.setf.resource' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 See the GNU Lesser General Public License for more details.

 A copy of the GNU Lesser General Public License should be included with 'de.setf.resource', as `lgpl.txt`.
 If not, see the GNU [site](http://www.gnu.org/licenses/)."))


;;; component conditional macro


(defmacro spoc-case ((mediator (sub pre obj con) subject predicate object context)
                     &key spoc spo (spo- spo) spc (sp-c spc) sp (sp-- sp)
                     soc (s-oc soc) so (s-o- so) sc (s--c sc) s (s--- s)
                     poc (-poc poc) po (-po- po) pc (-p-c pc) p (-p-- p)
                     oc (--oc oc) o (--o- o) c (---c c) all (---- all))
  "Consists of a sequence of forms, each identifier by a combination of statement components.
 The arguments are a mediator, s sequence of constituent variable and a matching series of
 constituent forms. If the mediator is not the constant nil, the variables are bound to the respective
 repository value. If nil, to the forms direct value. Then that clause is evaluated which indicates the
 non-null constituents. if no constituent is present, control passes to the :---- clause."

  `(let ((,sub ,(if mediator `(repository-value ,mediator ,subject) subject))
         (,pre ,(if mediator `(repository-value ,mediator ,predicate) predicate))
         (,obj ,(if mediator `(repository-value ,mediator ,object) object))
         (,con ,(if mediator `(repository-value ,mediator ,context) context)))
     (ecase (logior (if ,sub #b1000 0) (if ,pre #b0100 0) (if ,obj #b0010 0) (if ,con #b0001 0))
       (#b1111 ,spoc)
       (#b1110 ,spo-)
       (#b1101 ,sp-c)
       (#b1100 ,sp--)
       (#b1011 ,s-oc)
       (#b1010 ,s-o-)
       (#b1001 ,s--c)
       (#b1000 ,s---)
       (#b0111 ,-poc)
       (#b0110 ,-po-)
       (#b0101 ,-p-c)
       (#b0100 ,-p--)
       (#b0011 ,--oc)
       (#b0010 ,--o-)
       (#b0001 ,---c)
       (#b0000 ,----))))

#+digitool
(setf (ccl:assq 'spoc-case ccl:*fred-special-indent-alist*) 1)


;;; life-cycle states

(defclass rdf-state () ())
(defclass rdf:deleted (rdf-state) ())
(defclass rdf:new (rdf-state) ())
(defclass rdf:clean (rdf-state) ())
(defclass rdf:clean-persistent (rdf:clean rdf:persistent) ())
(defclass rdf:deleted-new (rdf:deleted rdf:new) ())
(defclass rdf:deleted-persistent (rdf:deleted rdf:persistent) ())
(defclass rdf:hollow (rdf:persistent) ())
(defclass rdf:modified (rdf-state) ())
(defclass rdf:modified-persistent (rdf:modified rdf:persistent) ())
(defclass rdf:new-persistent (rdf:new rdf:persistent) ())
(defclass rdf:modified-transient (rdf:modified rdf:transient) ())
(defclass rdf:persistent (rdf-state) ())
(defclass rdf:stored (rdf:persistent) ())
(defclass rdf:transient (rdf-state) ())


(defclass rdf:non-transactional (rdf-state) ())
(defclass transactional (rdf-state) ())
(defclass transaction-open (transactional)
  ((id
   :initform (uuid:make-v1-uuid)
   :reader transaction-id
   :documentation
   "A unique uuid identifies each transaction in the repositiory.")
   (start
    :initform (get-universal-time)
    :reader transaction-start)
   (end
    :initform nil
    :accessor transaction-end)))
(defclass transaction-abort (transactional) ())
(defclass transaction-commit (transactional) ())

(defvar rdf:clean-persistent (make-instance 'rdf:clean-persistent))
(defvar rdf:deleted-persistent (make-instance 'rdf:deleted-persistent))
(defvar rdf:hollow (make-instance 'rdf:hollow))
(defvar rdf:modified-persistent (make-instance 'rdf:modified-persistent))
(defvar rdf:new-persistent (make-instance 'rdf:new-persistent))
(defvar rdf:transient (make-instance 'rdf:transient))


(defvar rdf:non-transactional (make-instance 'rdf:non-transactional))
(defvar transaction-open (make-instance 'transaction-open))
(defvar transaction-commit (make-instance 'transaction-commit))
(defvar transaction-abort (make-instance 'transaction-abort))

#+(or)
(progn
  ;; while this is a possible implementation, it's easier to just use (cons <x>)
  ;; as its primary purpose is to distinctuis accessor mechanisms rather than to
  ;; test list constituency
  (deftype rdf:set (&optional type)
    "A set of objects of a homogeneous type"
    (if type
      `(satisfies ,(set-type-predicate type))
      'list))
  
  (defun set-type-predicate (type)
    (or (get type 'set-type-predicate)
        (let ((name (cons-symbol nil :setf-of- type :-p)))
          (setf (fdefinition name) #'(lambda (object) (when (listp object)
                                                        (dolist (e object t) (unless (typep e type) (return nil))))))
          (setf (get type 'set-type-predicate) name)
          name))))

(defgeneric resource-subtypep (type)
  (:method ((type null)) nil)
  (:method ((type symbol)) (subtypep type 'resource-object))
  (:method ((type cons))
    (case (first type)
      (or (some #'resource-subtypep (rest type)))
      (cons (resource-subtypep (second type)))
      (t nil))))

(defgeneric base-type (type)
  (:method ((type null)) nil)
  (:method ((type symbol)) type)
  (:method ((type cons))
    (case (first type)
      (or (some #'base-type (rest type)))
      (cons (second type))
      (t nil))))

(defgeneric list-type-p (type)
  (:documentation "Given a type specifier, indicate whether it specifies a list type
 TYPE : type-specifier
 VALUES : boolean : true iff the type is an explicit cons type
          boolean : false iff the first value cannot be determined")
  (:method ((type symbol))
    (case type
      ((nil) (values nil nil))
      ((cons list) (values t t))
      (t (values nil t))))
  (:method ((type class))
    (list-type-p (class-name type)))
  (:method ((type cons))
    (case (first type)
      (or (when (rest type)
            (dolist (type (rest type) (values nil t))
              (when (list-type-p type) (return (values t t))))))
      (cons (values t t))
      (t (values nil t)))))

(defun uri-unreserved-char-p (c) (or (alphanumericp c) (find c "-_.!~*'()")))

(defgeneric cl-user::format-url-encoded (stream datum colon at)
  (:method ((stream stream) (string string) colon at)
    (declare (ignore colon at))
    (if (every #'uri-unreserved-char-p string)
      (write-string string stream)
      (loop for c across string
            if (uri-unreserved-char-p c)
            do (write-char c stream)
            else do (format stream "~:@(%~2,'0X~)" (char-code c)))))
  (:method ((stream stream) (object t) colon at)
    (cl-user::format-url-encoded stream (princ-to-string object) colon at)))                          


(defgeneric rdf:uri-match-p (uri-instance uri-pattern)
  (:method ((uri string) (uri-pattern string))
    (let ((length (length uri-pattern)))
      (and (>= (length uri) length)
           (string= uri uri-pattern :end1 length)))))

;;;
;;; conditions

(define-condition rdf-error (simple-error) ())

(define-condition repository-error (rdf-error) ())

(define-condition rdf:invalid-state-error (rdf-error)
  ((object :initarg :object :reader condition-object)
   (start-state :initarg :start-state :reader condition-start-state)
   (end-state :initarg :end-state :reader condition-end-state))
  (:report (lambda (condition stream)
             (format stream "Object state transition not permitted: ~s (~s -> ~s)."
                     (condition-object condition)
                     (type-of (condition-start-state condition))
                     (type-of (condition-end-state condition))))))

(defun rdf:invalid-state-error (&rest args)
  (apply #'error 'rdf:invalid-state-error args))

(define-condition rdf:property-missing-error (rdf-error)
  ((object :initarg :object :reader condition-object)
   (value :initarg :value :reader condition-value)
   (predicate :initarg :predicate :reader condition-predicate)
   (operation :initarg :operation :reader condition-operation))
  (:report (lambda (condition stream)
             (format stream "predicate does not apply to object: ~s, ~s~@[ = ~s~]."
                     (condition-object condition)
                     (condition-predicate condition)
                     (condition-value condition)))))

(defun rdf:property-missing-error (&rest args)
  (apply #'error 'rdf:property-missing-error args))


(define-condition rdf:resource-not-found-error (rdf-error)
  ((uri :initarg :uri :reader condition-uri))
  (:report (lambda (condition stream)
             (format stream "No resource found for URI: ~s."
                     (condition-uri condition)))))

(defun rdf:resource-not-found-error (&rest args)
  (apply #'error 'rdf:resource-not-found-error args))


(define-condition rdf:schema-not-found-error (rdf-error)
  ((uri :initarg :uri :reader condition-uri))
  (:report (lambda (condition stream)
             (format stream "No resource found for URI: ~s."
                     (condition-uri condition)))))

(defun rdf:schema-not-found-error (&rest args)
  (apply #'error 'rdf:schema-not-found-error args))


(define-condition rdf:class-not-found-error (rdf-error)
  ((metaclass :initarg :metaclass :reader condition-metaclass)
   (name :initarg :name :reader condition-name))
  (:report (lambda (condition stream)
             (format stream "No class instance of metaclass found: ~s, ~s."
                     (condition-metaclass condition)
                     (condition-name condition)))))

(defun rdf:class-not-found-error (&key metaclass name)
  (restart-case (error 'rdf:class-not-found-error :metaclass metaclass :name name)
    (use-value (value)
               :report "Specify a class."
               (setf (find-class name) value))
    (create (&key (metaclass (or metaclass (error "METACLASS is required.")))
                  ;; nb. full namespace name as no prefixes are defined at compile-time
                  (direct-superclasses '({http://www.w3.org/2000/01/rdf-schema#}Resource)))
            (c2mop:ensure-class name :metaclass (find-class metaclass)
                                :direct-superclasses direct-superclasses))))


(define-condition rdf:property-read-only-error (rdf-error)
  ((object :initarg :object :reader condition-object)
   (predicate :initarg :predicate :reader condition-predicate)
   (operation :initarg :operation :reader condition-operation)
   (value :initarg :value :reader condition-value))
  (:report (lambda (condition stream)
             (format stream "Property is read-only: ~s, ~s."
                     (condition-object condition)
                     (condition-predicate condition)))))

(defun rdf:property-read-only-error (&rest args)
  (apply #'error 'rdf:property-read-only-error args))


(define-condition rdf:unbound-source-error (rdf-error)
  ((class :initarg :class :reader condition-class)
   (operation :initarg :operation :initform nil :reader condition-operation))
  (:report (lambda (condition stream)
             (format stream "No source mediator is bound for class and operation: ~s, ~s."
                     (condition-class condition)
                     (condition-operation condition)))))

(defun rdf:unbound-source-error (&rest args)
  (apply #'error 'rdf:unbound-source-error args))


(define-condition rdf:instance-not-found-error (rdf-error)
  ((class :initarg :class :reader condition-class)
   (uri :initarg :uri :reader condition-uri))
  (:report (lambda (condition stream)
             (format stream "No instance found for class and identifier: ~s, ~s."
                     (condition-class condition)
                     (condition-uri condition)))))

(defun rdf:instance-not-found-error (&rest args)
  (apply #'error 'rdf:instance-not-found-error args))

(define-condition rdf:feb-timeout-error (repository-error)
  ((repository :initarg :repository :reader condition-repository)
   (operation :initarg :operation :reader condition-operation))
  (:documentation "The error is signaled if repeated attempts to perform an feb operation
 encounter conflicts.")
  (:report (lambda (condition stream)
             (format stream "FEB operation timed out: ~s, ~s."
                     (condition-repository condition)
                     (condition-operation condition)))))

(defun rdf:feb-timeout-error (&rest args)
  (apply #'error 'rdf:feb-timeout-error args))


;;;
;;; literal utilities

(deftype rdf::literal () '(satisfies rdf::literal-p))

(defgeneric rdf::literal-p (object)
  (:method ((object t)) nil))

;;;
;;; uri/symbol utilities

(defgeneric rdf:identifier-p (object)
  (:documentation "Return true when the object is an identifier. This includes URI as represented
 by a repository, symbols, and UUID.")
  (:method ((identifier symbol)) t)
  (:method ((identifier uuid:uuid)) t)
  (:method ((object null)) nil)
  (:method ((object t)) nil))

(deftype rdf:identifier () '(satisfies rdf:identifier-p))

(defun uri-intrinsic-separator (uri-namestring)
  (let ((length (length uri-namestring)))
    (and (> length 0)
         (find (char uri-namestring (1- length)) '(#\/ #\#)))))

(defgeneric uri-extrinsic-separator (uri-namestring)
  (:method ((uri-namestring string))
    (unless (uri-intrinsic-separator uri-namestring)
      (multiple-value-bind (separator present)
                           (gethash uri-namestring *uri-separators*)
        (cond (present separator)
              (t
               #+(or ) (warn "Presuming default separator (~c) for uri: ~s"
                     *default-uri-separator* uri-namestring)
               *default-uri-separator*)))))
  (:method ((package package))
    (uri-extrinsic-separator (package-name package))))

(defun uri-extrinsic-separator-string (uri)
  (let ((sep (uri-extrinsic-separator uri)))
    (when sep (string sep))))

(defgeneric (setf uri-extrinsic-separator) (char uri-namestring)
  (:method ((delete null) (uri-namestring string))
    (remhash delete *uri-separators*))
  (:method ((char character) (uri-namestring string))
    (let ((old-value (gethash uri-namestring *uri-separators*)))
      (when (and old-value (not (eql old-value char)))
        (warn "Redefining url separator: ~s; ~c" uri-namestring char))
      (setf (gethash uri-namestring *uri-separators*) char)))
  (:method (object (package package))
    (setf (uri-extrinsic-separator (package-name package)) object)))


(defgeneric compute-extrinsic-separator (uri identifiers)
  (:documentation "if the uri has a separator, if identifiers is a package or a package name, cache
 the difference between the two as the separator.")

  (:method ((uri string) (identifiers string))
    (unless (equal uri identifiers)
      (if (uri-intrinsic-separator uri)
        (let ((i-length (length identifiers))
              (u-length (length uri)))
          (cond ((and (= u-length (1+ i-length))
                      (string= uri identifiers :end1 i-length))
                 (char uri i-length))
                (t
                 (warn "uri and package name do not match: ~s; ~s." uri identifiers)
                 nil)))
        (unless (uri-intrinsic-separator identifiers)
          (warn "Presuming separator: ~s; ~c" identifiers *default-uri-separator*)
          *default-uri-separator*))))

  (:method  ((uri string) (package package))
    (compute-extrinsic-separator uri (package-name package)))

  (:method  ((uri t) (identifiers null))
    (unless (uri-intrinsic-separator uri)
      (warn "Presuming separator: ~s; ~c" identifiers *default-uri-separator*)
      *default-uri-separator*))

  (:method  ((uri t) (identifiers t))
    (warn "Cannot compute extrinsic separator: ~s; ~s." uri identifiers)
    nil))


(defun fragment-has-separator-p (fragment-string)
  (and (> (length fragment-string) 0)
       (char-equal #\# (char fragment-string 0))))

(defun make-vocabulary-uri-namestring (base-uri fragment)
  (concatenate 'string base-uri
               (uri-extrinsic-separator-string base-uri)
               (if (and (uri-intrinsic-separator base-uri) (fragment-has-separator-p fragment))
                 (subseq fragment 1)
                 fragment)))

(defun symbol-uri-namestring (symbol &optional (canonicalize #'symbol-name))
  (declare (dynamic-extent canonicalize))
  (let* ((package (symbol-package symbol))
         (vocabulary (if package (package-name package) "")))
    (make-vocabulary-uri-namestring vocabulary
                                    (funcall canonicalize symbol))))

(defun uri-namestring-identifier (namestring &optional (canonicalize #'string) (create t))
  (declare (dynamic-extent canonicalize))
  (cond ((and (> (length namestring) 9) (string-equal namestring "urn:uuid:" :end1 9))
         (uuid:make-uuid-from-string (subseq namestring 9)))
        ((and (> (length namestring) 2) (string-equal namestring "_:" :end1 2))
         (intern (subseq namestring 2) "_"))
        (t
         (multiple-value-bind (package fragment)
                              (uri-vocabulary-components namestring)
           (let ((c-fragment (if fragment (funcall canonicalize fragment) "")))
             (if create
               (intern c-fragment package)
               (find-symbol c-fragment package)))))))
        
(defgeneric uri-vocabulary-components (uri)
  (:documentation "Given a URI, return its two source components, a package and the original symbol name.
 URI : string : the absolute uri reference string
 VALUES : package
          (or string null) : the term fragment or null if no term is present")

  (:method ((uri string))
    (let ((pos nil)
          (length (length uri))
          (fragment nil))
      ;; treat '/' and '#' the same
      (cond ((setf pos (position #\# uri :from-end t))
             (unless (= pos (1- length))
               (setf fragment (subseq uri (1+ pos)))
               (setf uri (subseq uri 0 (1+ pos)))))
            ((setf pos (position #\/ uri :from-end t))
             (unless (= pos (1- length))
               (setf fragment (subseq uri (1+ pos)))
               (setf uri (subseq uri 0 (1+ pos)))))
            ((and (string-equal "urn:" uri :end2 (min (length uri) 4))
                  (setf pos (position #\: uri :start 4)))
             (setf fragment (subseq uri (1+ pos))
                   uri (subseq uri 0 pos))))
             
      (values (or (find-package uri)
                  (when (uri-intrinsic-separator uri)
                    (find-package (subseq uri 0 (1- (length uri)))))
                  (make-package uri :use ()))
              fragment)))

  (:method ((identifier null))
    (values))

  (:method ((identifier symbol))
    (values (package-name (symbol-package identifier)) (symbol-name identifier)))

  (:method ((object t))
    (uri-vocabulary-components (uri-namestring object))))


(defun rdf:ensure-uri-package (uri)
  "Given the URI for a vocabulary term, extract the vocabulary uri and ensure that
 the respective package exists. If it is not found, make it anew.

 TERM-URI : string : an absolute URI reference which identifies a term. If it includes a fragment,
  that is truncated. Otherwise the last element in the path is removed."

  (let ((pos nil)
        (length (length uri)))
    (or (cond ((setf pos (position #\# uri :from-end t))
               (unless (= pos (1- length)) (setf uri (subseq uri 0 (1+ pos))))
               (find-package uri))
              ((setf pos (position #\/ uri :from-end t))
               (ensure-package (setf uri (subseq uri 0 pos))))
              (t
               (error "Invalid uri reference: ~s." uri)))
        (make-package uri :use ()))))


(defgeneric compute-object-uuid (object)
  )

(defun uuid-namestring-p (namestring)
  (and (> (length namestring) 9) (string-equal namestring "urn:uuid:" :end1 9)))

(defmethod uri-namestring ((uri uuid:uuid))
  (with-output-to-string (stream) (uuid::format-as-urn stream uri)))

(defmethod uri-namestring ((uri symbol))
  (symbol-uri-namestring uri))


(defgeneric uri-pathname (uri &key name type)
  (:documentation "Construct a pathname from a uri.
 URI : (or string symbol package uuid)

 Canonicalized the namestring and parse on '/' separatos to yield the path. In order to be consistent,
 the pathname requires a name component, since in some cases the associated schema has no object component in
 the uri path.")

  (:method ((designator package) &rest args)
    (declare (dynamic-extent args))
    (apply #'uri-pathname (package-name designator) args))

  (:method ((designator symbol) &rest args)
    (declare (dynamic-extent args))
    (apply #'uri-pathname (package-name (symbol-package designator)) args))

  (:method ((designator uuid:uuid) &rest args)
    (declare (dynamic-extent args))
    (apply #'uri-pathname (uri-namestring designator) args))

  (:method ((namestring string) &key (name (error "name is required.")) (type "lisp"))
    "Strip a fragement separator and construct a logical pathname with the '/'-separated components,
 replacing any non-word characters with '-', and adding a standard name and file type."
    (let ((hash (position #\# namestring :from-end t)))
      (when hash (setf namestring (subseq namestring 0 hash))))
    (let* ((path (remove nil (split-string namestring "/"))))
      (cond ((string-equal (first path) "http:")
             (pop path))
            ((uuid-namestring-p (first path))
             (setf (first path) (subseq (first path) 9))))
      (flet ((word (s)
               (substitute-if #\- #'(lambda (c) (not (or (alphanumericp c) (eql c  #\-)))) s)))
        (make-pathname :host (pathname-host *uri-pathname-root*)
                       :directory (append (pathname-directory *uri-pathname-root*) (mapcar #'word path))
                       :name name :type type)))))

;;; (probe-file (uri-pathname "http://xmlns.com/foaf/0.1/" :name "vocabulary"))
;;; (probe-file (uri-pathname (uuid:make-v1-uuid) :name "vocabulary"))


(defun rdf:map-collection (function collection)
  "Iterate over a degenerate list to allow post-facto change form atom to sequence"
  (loop (typecase collection
          (cons (funcall function (pop collection)))
          (null (return))
          (t (funcall function collection)
             (return)))))

(defun map-rdf-collection (function collection)
  (break "use rdf:map-collection")
  (rdf:map-collection function collection))


(defmacro rdf:do-collection ((variable collection) &body body)
  (let ((op (gensym)))
    `(block nil
       (flet ((,op (,variable) ,@body))
         (declare (dynamic-extent #',op))
         (rdf:map-collection #',op ,collection)))))
     


(defun hash-table-parent (hash-table)
  (gethash :hash-table-parent hash-table))

(defun (setf hash-table-parent) (parent hash-table)
  (setf (gethash :hash-table-parent hash-table) parent))


;;;
;;; pathname map -> mime type 

(defparameter *pathname-type-mime-type*
  '(("nt" . mime:application/n3)
    ("n3" . mime:application/n3)
    ("ntriples" . mime:application/n3)
    ("owl" . mime:application/rdf+xml)
    ("rdf" .  mime:application/rdf+xml)
    ("ttl" . mime:text/turtle)))


(defgeneric location-mime-type (type)
  (:documentation "Return the expected or declared mime type for s given LOCATION.
 LOCATION : (or pathname string url stream)

 URL support depends on methods to be inrtoduced for the respective external module.")

  (:method ((location pathname))
    "Given a pathname, use its file type"
    (location-mime-type (pathname-type location)))


  (:method ((type string))
    (let ((type-length (length type)))
      (flet ((test-key (key)
               (let ((key-length (length key)))
                 (and (>= type-length key-length)
                      (string-equal type key :start1 (- type-length key-length)))))
             (resolve-type (mt)
               (etypecase mt
                 (symbol (symbol-value mt))
                 (mime:mime-type mt))))
        (declare (dynamic-extent #'test-key))
        (or (resolve-type (rest (assoc-if #'test-key *pathname-type-mime-type*)))
            (error "No mime type for pathname type: ~s." type))))))


;;;
;;; mop utilities

(defmacro rdf:defclass (name supers slots &rest options)
  "Define a class to mirror a linked data resource.
 - ensure that the metaclass is set.
 - ensure that resource-object is in the precedence list.
 - ensure that each slot has an accessor.
   perfer the declared accessor for the respective property as the default."

  (unless (assoc :metaclass options)
    (push '(:metaclass rdf:resource-class) options))
  (unless (find 'rdf:resource-object supers)
    (setf supers (append supers '(rdf:resource-object))))
  `(defclass ,name ,supers
     ,(mapcar #'(lambda (slot)
                  (destructuring-bind (slot-name &rest slot-args &key accessor (predicate slot-name p-s) &allow-other-keys) slot
                    `(,slot-name
                      ,@(unless accessor `(:accessor ,(or (property-function predicate)
                                                         (cons-symbol (symbol-package name) name "-" slot-name))))
                      ,@(unless p-s `(:predicate ,predicate))
                      ,@slot-args)))
              slots)
     ,@options))

(set-pprint-dispatch '(cons (eql rdf:defclass)) (pprint-dispatch '(cons (eql defclass))))


(defmacro rdf:defaccessor (function-name (parameter) &key property (name property) (type t))
  "Define the base definitions for prototypal proerty accessors for the given predicate."
  (let ((list-type-p (list-type-p type)))
    (when property
      (unless (and (consp property) (eq (first property) 'quote))
        (setf property `(quote ,property)))
      (unless (and (consp type) (eq (first type) 'quote))
        (setf type `(quote ,type))))
    `(progn
       (defgeneric ,function-name (,parameter)
         ,(if list-type-p
            `(:method ((object resource-object))
               (let ((value (rdf:prototypal-property-value object ,property)))
                 (if (listp value) value (list value))))
            `(:method ((object resource-object))
               (rdf:prototypal-property-value object ,property)))
         (:generic-function-class rdf:rdf-slot-reader)
         (:method-combination persistent-slot-reader
                              ,@(when name `(:name ,name))
                              ,@(when type `(:type ,type))))
       ,@(when property `((setf (slot-value #',function-name 'property) ,property)))
       ,@(when type `((setf (slot-value #',function-name 'type) ,type)))
       (defgeneric (setf ,function-name) (value ,parameter)
         ,@(if list-type-p
             `((:method ((value list) (object resource-object))
                 (setf (rdf:prototypal-property-value object ,property) value))
               (:method ((value t) (object resource-object))
                 (setf (rdf:prototypal-property-value object ,property) (list value))))
             `((:method (value (object resource-object))
                 (setf (rdf:prototypal-property-value object ,property) value))))
         (:generic-function-class rdf:rdf-slot-writer)
         (:method-combination persistent-slot-writer
                              ,@(when name `(:name ,name))
                              ,@(when type `(:type ,type))))
       ,@(when property `((setf (slot-value #'(setf ,function-name) 'property) ,property)))
       ,@(when type `((setf (slot-value #'(setf ,function-name) 'type) ,type)))
       ',function-name)))
     
#|


;;; this does not work to unify structure classes, at least for ccl
;;; as reinitialize instance does not update the precedence list.

(defclass protocol-class (class) ()
  (:metaclass standard-class))
(defmethod reinitialize-instance :after ((class protocol-class) &key direct-slots)
  (declare (ignore direct-slots)))

(reinitialize-instance (find-class 'protocol-class) :precedence-list (list (find-class 'class)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod c2mop:validate-superclass ((c1 class) (c2 protocol-class)) t)
  (defmethod c2mop:validate-superclass ((c2 protocol-class) (c1 class)) t))

(defgeneric interpose-superclass (additional-class class)
  (:method ((additional-class symbol) (class t))
    (interpose-superclass (find-class additional-class) class))

  (:method ((additional-class t) (class symbol))
    (interpose-superclass additional-class (find-class class)))

  (:method ((additional-class class) (class class))
    (let ((existing-supers (c2mop:class-direct-superclasses class)))
      (unless (find additional-class existing-supers)
        (let* ((right (member-if #'(lambda (c) (find c (c2mop:class-direct-superclasses additional-class)))
                                 existing-supers))
               (left (ldiff existing-supers right)))
        (reinitialize-instance class
                               :direct-superclasses (append left (list additional-class) right)))))
    class))

;;; this does not work,  compute-applicable methods is not included in effective method
;;; computations

(defclass satisfies-specializer (c2mop:specializer)
  ((predicate :initform 'identity
              :reader specializer-predicate)))

(trace compute-applicable-methods)
(defclass sfg (standard-generic-function)
  ()
  (:metaclass c2mop:funcallable-standard-class))

(defgeneric f1 (a1 a2)
  (:generic-function-class sfg))

(defmethod compute-applicable-methods ((function sfg) (arguments t))
  (print function)
  (call-next-method))

(defmethod f1 ((a string) (b number)) (list a b))
(defmethod f1 ((a number) (b number)) (list a b))
(f1 "a" 1)

|#
