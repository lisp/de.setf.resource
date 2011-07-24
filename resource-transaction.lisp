;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.resource.implementation; -*-


(in-package :de.setf.resource.implementation)

(:documentation
  "This file implements resource transactions for the `de.setf.resource` CLOS linked data library."
 
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
  "'de.setf.resource' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.resource' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.resource' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/)."))


;;; life-cycle suport


(defmethod abort-transaction (mediator)
  (transaction-step mediator (mediator-state mediator) transaction-abort))

(defmethod commit-transaction (mediator)
  (transaction-step mediator (mediator-state mediator) transaction-commit))

(defmethod open-transaction (mediator)
  (transaction-step mediator (mediator-state mediator) 
                    (make-instance 'transaction-open)))

(defgeneric transaction-evict (mediator object)
  (:documentation "Remove the object from the transactional cache.")

  (:method ((mediator repository-mediator) object)
    (remhash object (mediator-transaction-cache mediator))))

(defgeneric transaction-register (mediator object)
  (:documentation "Register the object and state in the transaction cache.")

  (:method ((mediator repository-mediator) object)
    (setf (gethash object (mediator-transaction-cache mediator)) (object-state object))))



(defgeneric transaction-step (mediator start-state end-state)
  (:method ((mediator repository-mediator) (start t) (end t))
    (invalid-state-error :object mediator :start-state start :end-state end))

  (:method ((mediator repository-mediator) (start non-transactional) (end transaction-commit))
    ;; ignore it
    end)

  (:method ((mediator repository-mediator) (start transaction-open) (end transaction-commit))
    "To commit an open transaction required three passes over the transaction cache.
 - First, ensure that no object has been modified by attempting to assert a new version in the store for each
 modified object. Should that fail, roll back any changed store versions, abort the transaction, and in the
 process clear the cache.
 - Second, write all modified object properties in the context of the respective new version graph. Should that
 fail, perform the same rollback and abort as for the first pass.
 - Once the write has succeeded, update the object states to either hollow or non-transactional and clear the
 cache."

    (setf (transaction-end start) (get-universal-time))
    (setf-mediator-state end mediator)
    ;; pass one, attempt to revise all modified objects' state
    (loop for state being each hash-key of (mediator-transaction-cache mediator)
          using (hash-value object)
          unless (lock-version-in-state object state)
          do (return-from transaction-step (abort-transaction mediator)))
                    
    ;; push any changes (or deletion) to the repository
    (loop with new-version-id = (transaction-id start)
          for state being each hash-key of (mediator-transaction-cache mediator)
          using (hash-value object)
          do (progn (setf (object-graph object) new-version-id)
                    (write-properties-in-state object state)))

    (repository-write-transaction-metadata mediator start)

    ;; then mark comitted
    (maphash #'commit-in-state (mediator-transaction-cache mediator))
    (clrhash (mediator-transaction-cache mediator))
    (setf-mediator-state non-transactional mediator))

  (:method ((mediator repository-mediator) (start non-transactional) (end transaction-open))
    (setf-mediator-state transaction-open mediator))

  (:method ((mediator repository-mediator) (start non-transactional) (end transaction-abort))
    ;; ignore it
    end)

  (:method ((mediator repository-mediator) (start transaction-open) (end transaction-abort))
    (setf-mediator-state end mediator)
    (maphash #'rollback-in-state (mediator-transaction-cache mediator))
    (clrhash (mediator-transaction-cache mediator))
    (setf-mediator-state non-transactional mediator)))

(defmacro rdf:with-transaction ((mediator) &body body)
  (let ((op (cons-symbol nil :transaction))
        (repository-var (cons-symbol nil :mediator)))
    `(flet ((,op () ,@body))
       (let ((,repository-var ,mediator))
         (typecase (mediator-state ,repository-var)
           (transactional (,op))
           (t (unwind-protect (progn (open-transaction ,repository-var)
                                     (multiple-value-prog1 (,op)
                                       (commit-transaction ,repository-var)))
                (typecase (mediator-state ,repository-var)
                  (transactional (abort-transaction ,mediator))))))))))




#|

on transactions.

both allegrostore and jena include operators which imply transactional support.
the allegrostore documentation lead one to believe that everything is transactional - there is no begin, and the
only option is to commit or roll back.

Cassandra has no transactions. its only guarantee is the a single row write is atomic.
there is not even any guarantee for the extent of a 'batch-mutate'.
In this environment - and in keeping with the "open world" goal, one approach is to treat the repository as
logically write-once.

- Each update is written as  relation to a single context.
- Each new context is either a successor or an altrenative to the contexts from which the initial content of
  its constituents originated. A successor supplants the original, an alternative proceeds in parallel and
  can eventually be integrated to the original.
- data is read into a transaction in one of two ways
  - as autonomous statements with respect to an explicitly specified (possibly compound) context
  - in relation to subject (and an optional temporal/version position), in which case the effective contexts
    respective the projection position must be determined. This starts from a subject root set and is extended
    incrementally as the predicated relation graph extends wit successive slot retrivals

this requires:

- (context . ( (assertion-time . ts) (validity-start . ts) (validity-end . ts) ) )

 which would be used to determine the temporal location of a context when combining or extending it with others.

- (context .
    :alternatives . ( (alternative-context . state)* )
-   :successor ( (successor-context . state) )

- (subject . ( (spoc . context)* ) )
or
- (subject . ( (context . spoc)* ) )

the latter would permit v1-uuid context names to serve to sort the entries

the alternative/successor state would indicate open or closed. given which, the successor row would act as a
flagged storage cell required by the NB-FEB primitives to implement synchronization primitives on the basis of
a cassandra store.

Ha, et.al[2] describe the NB-FEB synchronization primitive in terms of four sub-primtives:

- TFAS (function (place object) (values object flag))
  returns the cell's original value (object flag); if the flag was :false, sets the cell to the new value (object :true)

- LOAD (function (place) (values object flag))
  returns the cell's value (object flag)

- SAC (function (place object) (values object flag)
  returns the cell's original value (object flag); sets the cell to the new value (object :false)

- SAS (function (place object) (values object flag)
  returns the cell's original value (object flag); sets the cell to the new value (object :true)


The abstract structure is just

  Store ::= (Name x Cell)
  Cell ::=  (Object x Flag)
  Flag :: = True + False

The only requirement is that of Figure 1 of [1], that apparently 'simultaneous' operations are serialized, which the
effect that all parties observe the same outcome. In particular the only one TFAS invocation receives back the :false flag.

In terms of cassandra operators, the literal implementation for tfas would be

    (defun tfas (variable value)
      "PLACE designates an initial context, OBJECT designates a successor"
    
      (destructuring-bind (old-value old-flag) (cf-ref-values *variables* variable :value :flag)
        (when (eql :false old-flag)
          (cf-set-values *variables* variable :value object :flag :true))
        (values old-value old-flag)))

Which reads nicely, and requires just one read and one write.
However, as cassandra provides no atomic update operation, the implementation fails: interleaved execution by more
than one process would lead each to believe it had succeeded.
On the other hand, multiple column writes are atomic, so long as they are in the same row.
With which, rather than represent the flag with a literal value, one can represent it as the
presence/absence of values. If, for example, a row with content

    ( variable1 . ( ) )

indicates '(value . :false)', while

    ( variable1 . ( (holder1 . value2) ) )

indicates  '(value2 . :true)' and

    ( variable1 . ( (holder1 . ?) (holder2 . ?) ) )

indicates an intermediate state to be combined as per[2]. On the basis of which the operators are

    (defun tfas (variable value)
      "PLACE designates an initial context, OBJECT designates a successor"

      (cf-set-values *variables* variable *uuid* :TFAS)
      (let ((values (cf-ref-values *variables* variable)))
        (cond ((cddr values)
               ;; if there is a conflict, remove the successor
               (cf-set-values *variables* variable *uuid* nil)
               (values (first values) :true))
              (t
               (cf-set-values *variables* :value value)
               (values (first values) :false)))))

    (defun load (variable)
      (let ((values (cf-ref-values *variables* variable)))
        (values (first values) (if (rest values) :true :false))))

    (defun sac (variable value) ;no
      (let ((values (cf-ref-values *variables* variable)))
        (cf-set-values *variables* variable *uuid* nil)
        (values (first values) (if (rest values) :true :false))))

    (defun sas (variable value) ;no
      (let ((values (cf-ref-values *variables* variable)))
        (cf-set-values *variables* variable *uuid* value)
        (values (first values) (if (rest values) :true :false))))

if two (or more) clients attempt simultaneous operations, the elementary operations will be interleaved and
a similary situation arises as in the original exposition for operation combinations at an intermediate switch.
As, in this case, the read/write operation sequence is not atomic, one must treat the local state variations apparent to
each process analogous to the switch variations. The intermediate result seen by each need not be the same, but the
final result seen by each must converge, and the effective operation on the store state must be that of the correct
combined operation.

For two given operations the read can contain one process marker, or the other, or both,
and the reflection seen by the two processes need not be the same. If the operations were to be performed with
a low consistency level, given the possible result state in an immediate node: m.p1, m.p2, m.p1+m.p2, under the
constraint, that a process could never see the result of the other operation without seeing the result of its own,
the projection would be one of

      process       p1           p2
      projection  m.p1          m.p2
                  m.p1, m.p2    m.p2
                  m.p1          m.p1, m.p2
                  m.p1, m.p2    m.p1, m.p2

Given a higher consistency level, the first combination (p1: m.p1, p2:m.p2) cannot arise. The following tables describe how to
resolve the combinations in the remaining cases:

-> see notes in notebook:
 if the batch mutations for a single key, it is possible to implement a nb-feb with three operations
 - write operation
 - read variable value/flag state + all pending operations
 - sort operations by timestamp, then by id
 - combine all prior to and including the own to produce the local result
 - combine that with the rest to create the global result.
 - write the result back with the timestamp of the latest operation, deleting all operations.
 - the store retains only the rsult with the latest timestamp.

according to ellis/black, isolation is not guaranteed between mutation operations on a single row key.
the means that one thread may not see at once all the columns written by another thread. if that is true, then
it is not possible to use the row as an operation cache as each thread my see a different - and contradictory
projection of its content.

storageProxy / storageService is too sparsely documented to tell what it is intended to do, but it reads as if
each column is modified separately in the memory cache, but a given cache is flushed as a single entity.
which should yield row isolation, but somehow does not. ? because of reference before it is flushed?

if one has just single-column isolation,
- the presence of more than one operation column is a conflict. each process removes it's column and tries again.
- once it is the only operation, it performs the op, updating the value.
-- tfas : 

}

is it possible to implement an feb with just one column?
- the cell requires a value when it is empty

---
[1] : http://www.franz.com/agraph/support/documentation/v4/lisp-reference.html 




(thrift:def-struct "location"
  "The variable cell combines a value with a full/empty bit."
  (("feb" nil :type bool :id 1)
   ("value" nil :type i64 :id 2)))

(thrift:def-struct "operation"
  "An operation combines an operation code with an argument."
  (("opCode" nil :type i08 :id 1)
   ("value" nil :type i64 :id 2)))

(thrift:def-struct "operationColumn"
  "A column specialized to accept nfeb operations."
  (("name" nil :id 1 :type binary)
   ("value" nil :id 2 :type operation)
   ("timestamp" nil :id 3 :type i64)))

(defmethod dsc:column-name ((column operation-column))
  (operation-column-name column))

(defmethod dsc::column-timestamp ((column operation-column))
  (operation-column-timestamp column))

(defmethod dsc:column-value ((column operation-column))
  (operation-column-value column))


(defparameter +nfeb-load+ 1)
(defparameter +nfeb-sac+ 2)
(defparameter +nfeb-sas+ 3)
(defparameter +nfeb-tfas+ 4)

(defun compute-location-and-operations-immediately (identity location columns)
  "Given a list of operations sorted by timestamp, isolate the operations up to and including the local
 operation, compute the location state through that point, and return that as the local location state.
 If there are no further operations, return the operation list to incidate that also the global location
 state. Otherwise, leave the global changes to the later operations to compute and store the subsequent
 state.

 VALUES : location : the local state
          effective-operations : if the local state should be written as the global, the set of
                                 operations to delete"
  
  (let* ((after (rest (member identity columns :key #'dsc:column-name :test #'equalp)))
         (before-and-self (ldiff columns after)))
    ;; (print (list :after after :before-and-self before-and-self))
    (cond (before-and-self
           (let* ((new-location (reduce-operations-immediately location (mapcar #'dsc:column-value before-and-self))))
             (values new-location
                     (unless after before-and-self))))
          (t
           ;; if there are no operations, the local state is whatever the location now stores
           ;; and no change is to be made
           (values location nil)))))

(defun reduce-operations-immediately (location operations)
  (dolist (operation operations)
    (setf location
          (ecase (aref #(+nfeb-load+ +nfeb-sac+ +nfeb-sas+ +nfeb-tfas+)
                       (1- (operation-op-code operation)))
            (+nfeb-load+  location)
            (+nfeb-sac+   (make-location :feb 0 :value (operation-value operation)))
            (+nfeb-sas+   (make-location :feb 1 :value (operation-value operation)))
            (+nfeb-tfas+  (if (plusp (location-feb location))
                            location
                            (make-location :feb 1 :value (operation-value operation)))))))
  location)


(defgeneric combine-operations-indirectly (op1 op2 v1 v2)
  (:documentation "this version is true iff the results are reported back to both source.
 It is not true - eg, tfas+sac if one party may see only an early part of the chronology
 as it will have acted on just that part. this means that the combination must be on the
 location directly, not directly amongst themselves.

 It will still require that the timestamps accurately reflect the order of appearance in the
 store. a later appearing column with an early timestamp would invalidate an earlier result.")

  (:method ((op1 (eql +nfeb-load+)) (op2 (eql +nfeb-load+)) (v1 t) (v2 t))
    (values +nfeb-load+ nil))
  (:method ((op1 (eql +nfeb-load+)) (op2 (eql +nfeb-sac+)) (v1 t) (v2 t))
    (values +nfeb-sac+ v2))
  (:method ((op1 (eql +nfeb-load+)) (op2 (eql +nfeb-sas+)) (v1 t) (v2 t))
    (values +nfeb-sas+ v2))
  (:method ((op1 (eql +nfeb-load+)) (op2 (eql +nfeb-tfas+)) (v1 t) (v2 t))
    (values +nfeb-tfas+ v2))

  (:method ((op1 (eql +nfeb-sac+)) (op2 (eql +nfeb-load+)) (v1 t) (v2 t))
    (values +nfeb-sac+ v1))
  (:method ((op1 (eql +nfeb-sac+)) (op2 (eql +nfeb-sac+)) (v1 t) (v2 t))
    (values +nfeb-sac+ v2))
  (:method ((op1 (eql +nfeb-sac+)) (op2 (eql +nfeb-sas+)) (v1 t) (v2 t))
    (values +nfeb-sas+ v2))
  (:method ((op1 (eql +nfeb-sac+)) (op2 (eql +nfeb-tfas+)) (v1 t) (v2 t))
    (values +nfeb-sas+ v2))

  (:method ((op1 (eql +nfeb-sas+)) (op2 (eql +nfeb-load+)) (v1 t) (v2 t))
    (values +nfeb-sas+ v1))
  (:method ((op1 (eql +nfeb-sas+)) (op2 (eql +nfeb-sac+)) (v1 t) (v2 t))
    (values +nfeb-sac+ v2))
  (:method ((op1 (eql +nfeb-sas+)) (op2 (eql +nfeb-sas+)) (v1 t) (v2 t))
    (values +nfeb-sas+ v2))
  (:method ((op1 (eql +nfeb-sas+)) (op2 (eql +nfeb-tfas+)) (v1 t) (v2 t))
    (values +nfeb-sas+ v1))

  (:method ((op1 (eql +nfeb-tfas+)) (op2 (eql +nfeb-load+)) (v1 t) (v2 t))
    (values +nfeb-tfas+ v1))
  (:method ((op1 (eql +nfeb-tfas+)) (op2 (eql +nfeb-sac+)) (v1 t) (v2 t))
    (values +nfeb-sac+ v2))
  (:method ((op1 (eql +nfeb-tfas+)) (op2 (eql +nfeb-sas+)) (v1 t) (v2 t))
    (values +nfeb-sas+ v2))
  (:method ((op1 (eql +nfeb-tfas+)) (op2 (eql +nfeb-tfas+)) (v1 t) (v2 t))
    (values +nfeb-tfas+ v1)))
    
                         
          
(defun reduce-operations (location operations)
  (let* ((operation (pop operations))
        (op-code (operation-op-code operation))
        (op-value (operation-value operation)))
    (dolist (next operations)
      (multiple-value-setq (op-code op-value)
        (combine-operations-temporally location op-code (operation-op-code next) op-value (operation-value next))))
    (make-operation :op-code op-code :value op-value)))

(defgeneric combine-operations-temporally (op1 op2 v1 v2)
  (:documentation "this version is true iff the results are reported back to both source.
 It is not true - eg, tfas+sac if one party may see only an early part of the chronology
 as it will have acted on just that part. this means that the combination must be on the
 location directly, not directly amongst themselves.

 It will still require that the timestamps accurately reflect the order of appearance in the
 store. a later appearing column with an early timestamp would invalidate an earlier result.

 in order to accomplish this, either the timestamp is generated by the store simultaneous with the write,
 of a process must retry if there is a later timestamp in the read state.")

  (:method ((op1 (eql +nfeb-load+)) (op2 (eql +nfeb-load+)) (v1 t) (v2 t))
    (values +nfeb-load+ nil))
  (:method ((op1 (eql +nfeb-load+)) (op2 (eql +nfeb-sac+)) (v1 t) (v2 t))
    (values +nfeb-sac+ v2))
  (:method ((op1 (eql +nfeb-load+)) (op2 (eql +nfeb-sas+)) (v1 t) (v2 t))
    (values +nfeb-sas+ v2))
  (:method ((op1 (eql +nfeb-load+)) (op2 (eql +nfeb-tfas+)) (v1 t) (v2 t))
    (values +nfeb-tfas+ v2))

  (:method ((op1 (eql +nfeb-sac+)) (op2 (eql +nfeb-load+)) (v1 t) (v2 t))
    (values +nfeb-sac+ v1))
  (:method ((op1 (eql +nfeb-sac+)) (op2 (eql +nfeb-sac+)) (v1 t) (v2 t))
    (values +nfeb-sac+ v2))
  (:method ((op1 (eql +nfeb-sac+)) (op2 (eql +nfeb-sas+)) (v1 t) (v2 t))
    (values +nfeb-sas+ v2))
  (:method ((op1 (eql +nfeb-sac+)) (op2 (eql +nfeb-tfas+)) (v1 t) (v2 t))
    (values +nfeb-sas+ v2))

  (:method ((op1 (eql +nfeb-sas+)) (op2 (eql +nfeb-load+)) (v1 t) (v2 t))
    (values +nfeb-sas+ v1))
  (:method ((op1 (eql +nfeb-sas+)) (op2 (eql +nfeb-sac+)) (v1 t) (v2 t))
    (values +nfeb-sac+ v2))
  (:method ((op1 (eql +nfeb-sas+)) (op2 (eql +nfeb-sas+)) (v1 t) (v2 t))
    (values +nfeb-sas+ v2))
  (:method ((op1 (eql +nfeb-sas+)) (op2 (eql +nfeb-tfas+)) (v1 t) (v2 t))
    (values +nfeb-sas+ v1))

  (:method ((op1 (eql +nfeb-tfas+)) (op2 (eql +nfeb-load+)) (v1 t) (v2 t))
    (values +nfeb-tfas+ v1))
  (:method ((op1 (eql +nfeb-tfas+)) (op2 (eql +nfeb-sac+)) (v1 t) (v2 t))
    (values +nfeb-sac+ v2))
  (:method ((op1 (eql +nfeb-tfas+)) (op2 (eql +nfeb-sas+)) (v1 t) (v2 t))
    (values +nfeb-sas+ v2))
  (:method ((op1 (eql +nfeb-tfas+)) (op2 (eql +nfeb-tfas+)) (v1 t) (v2 t))
    (values +nfeb-tfas+ v1)))



(defun test-nbfeb (&key (id 1) (length 3) (feb 0) (stream *trace-output*))
  (labels ((make-binary (&rest contents)
             (make-array  (length contents) :element-type '(unsigned-byte 8) :initial-contents contents))
           (call-with-columns (op levels &rest columns)
             (if (zerop levels)
               (funcall op (sort (copy-list columns) #'< :key #'dsc:column-timestamp))
               (let ((id (- length levels)))
                 (dotimes (op-code 4)
                   (apply #'call-with-columns op (1- levels)
                          (make-operation-column :name (make-binary id) 
                                                 :value (make-operation :op-code (1+ op-code) :value id)
                                                 :timestamp id)
                          columns))))))
    (flet ((test-columns (columns)
             (let ((location (make-location :feb feb :value 100))
                   (operation-names #(load sac sas tfas)))
               (multiple-value-bind (new-location op-columns)
                                    (compute-location-and-operations-immediately id location columns)
                 (format stream "~&[~s ~s] -> [~s ~s]~24t: ~{  [~{~s (~4s ~s) ~s~}]~}"
                         (location-feb location) (location-value location)
                         (location-feb new-location) (location-value new-location)
                         (mapcar #'(lambda (column)
                                     (let ((operation (dsc:column-value column)))
                                       (list (dsc:column-name column)
                                             (aref operation-names (1- (operation-op-code operation)))
                                             (operation-value operation)
                                             (dsc:column-timestamp column))))
                                 op-columns)
                         )))))
      (call-with-columns #'test-columns length))))

;;; (test-nbfeb :length 2)
;;; (test-nbfeb :length 3)

(test:test resource.nbfeb.1
  (flet ((make-column (&rest args)
           (apply #'cassandra_2.1.0:make-column args))
         (make-binary (&rest contents)
           (make-array  (length contents) :element-type '(unsigned-byte 8) :initial-contents contents)))
    (let* ((location (make-location :feb 0 :value 100))
           (id (make-binary 1))
           (columns (list (make-operation-column :name (make-binary 0) :value (make-operation :op-code +nfeb-load+ :value 0) :timestamp 1)
                          (make-operation-column :name id :value (make-operation :op-code +nfeb-load+ :value 0) :timestamp 2))))
      (multiple-value-list (compute-location-and-operations id location columns)))))

|#
