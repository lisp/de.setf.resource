;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: common-lisp-user; -*-
;;; 20100808T000125Z00
;;; from "http://www.w3.org/2006/time#"

(in-package :common-lisp-user)


(defpackage "http://www.w3.org/2006/time#"
  (:use)
  (:nicknames "time" "http://www.w3.org/2006/timezone#")
  (:export "hasDurationDescription" "hasEnd" "hasBeginning" "before" "TemporalEntity" "inXSDDateTime" "inDateTime" "Instant" "inside" "Interval" "intervalFinishes" "intervalDuring" "intervalStarts" "intervalOverlaps" "intervalMeets" "intervalBefore" "intervalEquals" "ProperInterval" "seconds" "minutes" "hours" "days" "weeks" "months" "years" "DurationDescription" "Year" "xsdDateTime" "hasDateTimeDescription" "DateTimeInterval" "TemporalUnit" "timeZone" "second" "minute" "hour" "dayOfYear" "dayOfWeek" "day" "week" "month" "year" "unitType" "DateTimeDescription" "DayOfWeek" "January"))

(rdfs:defvocabulary "time" :uri "http://www.w3.org/2006/time#" :package "http://www.w3.org/2006/time#"
 :definitions
 ((DE.SETF.RESOURCE:DEFCLASS |http://www.w3.org/2006/time#|::|January|
                             (|http://www.w3.org/2006/time#|::|DateTimeDescription|
                              #:|| #:||)
                             ()
                             (:DATATYPE
                              |http://www.w3.org/2006/time#|::|January|))
  (DE.SETF.RESOURCE:DEFCLASS |http://www.w3.org/2006/timezone#|::|TimeZone|
                             ()
                             ()
                             (:DATATYPE
                              |http://www.w3.org/2006/timezone#|::|TimeZone|))
  (DE.SETF.RESOURCE:DEFCLASS |http://www.w3.org/2006/time#|::|DayOfWeek|
                             ()
                             ()
                             (:DATATYPE
                              |http://www.w3.org/2006/time#|::|DayOfWeek|))
  (DE.SETF.RESOURCE:DEFCLASS |http://www.w3.org/2006/time#|::|DateTimeDescription|
                             (#:|| #:|| #:|| #:|| #:|| #:|| #:|| #:|| #:|| #:||
                              #:||)
                             ((|http://www.w3.org/2006/time#|::|unitType| :TYPE
                               |http://www.w3.org/2006/time#|::|TemporalUnit|
                               :DATATYPE
                               |http://www.w3.org/2006/time#|::|TemporalUnit|)
                              (|http://www.w3.org/2006/time#|::|year| :TYPE
                               |http://www.w3.org/2001/XMLSchema|:|gYear|
                               :DATATYPE
                               |http://www.w3.org/2001/XMLSchema|:|gYear|)
                              (|http://www.w3.org/2006/time#|::|month| :TYPE
                               |http://www.w3.org/2001/XMLSchema|:|gMonth|
                               :DATATYPE
                               |http://www.w3.org/2001/XMLSchema|:|gMonth|)
                              (|http://www.w3.org/2006/time#|::|week| :TYPE
                               |http://www.w3.org/2001/XMLSchema|:|nonNegativeInteger|
                               :DATATYPE
                               |http://www.w3.org/2001/XMLSchema|:|nonNegativeInteger|)
                              (|http://www.w3.org/2006/time#|::|day| :TYPE
                               |http://www.w3.org/2001/XMLSchema|:|gDay|
                               :DATATYPE
                               |http://www.w3.org/2001/XMLSchema|:|gDay|)
                              (|http://www.w3.org/2006/time#|::|dayOfWeek|
                               :TYPE
                               |http://www.w3.org/2006/time#|::|DayOfWeek|
                               :DATATYPE
                               |http://www.w3.org/2006/time#|::|DayOfWeek|)
                              (|http://www.w3.org/2006/time#|::|dayOfYear|
                               :TYPE
                               |http://www.w3.org/2001/XMLSchema|:|nonNegativeInteger|
                               :DATATYPE
                               |http://www.w3.org/2001/XMLSchema|:|nonNegativeInteger|)
                              (|http://www.w3.org/2006/time#|::|hour| :TYPE
                               |http://www.w3.org/2001/XMLSchema|:|nonNegativeInteger|
                               :DATATYPE
                               |http://www.w3.org/2001/XMLSchema|:|nonNegativeInteger|)
                              (|http://www.w3.org/2006/time#|::|minute| :TYPE
                               |http://www.w3.org/2001/XMLSchema|:|nonNegativeInteger|
                               :DATATYPE
                               |http://www.w3.org/2001/XMLSchema|:|nonNegativeInteger|)
                              (|http://www.w3.org/2006/time#|::|second| :TYPE
                               |http://www.w3.org/2001/XMLSchema|:|decimal|
                               :DATATYPE
                               |http://www.w3.org/2001/XMLSchema|:|decimal|)
                              (|http://www.w3.org/2006/time#|::|timeZone| :TYPE
                               |http://www.w3.org/2006/timezone#|::|TimeZone|
                               :DATATYPE
                               |http://www.w3.org/2006/timezone#|::|TimeZone|))
                             (:DATATYPE
                              |http://www.w3.org/2006/time#|::|DateTimeDescription|))
  (DE.SETF.RESOURCE:DEFCLASS |http://www.w3.org/2006/time#|::|TemporalUnit|
                             ()
                             ()
                             (:DATATYPE
                              |http://www.w3.org/2006/time#|::|TemporalUnit|))
  (DE.SETF.RESOURCE:DEFCLASS |http://www.w3.org/2006/time#|::|DateTimeInterval|
                             (|http://www.w3.org/2006/time#|::|ProperInterval|)
                             ((|http://www.w3.org/2006/time#|::|hasDateTimeDescription|
                               :TYPE
                               |http://www.w3.org/2006/time#|::|DateTimeDescription|
                               :DATATYPE
                               |http://www.w3.org/2006/time#|::|DateTimeDescription|)
                              (|http://www.w3.org/2006/time#|::|xsdDateTime|
                               :TYPE
                               |http://www.w3.org/2001/XMLSchema|:|dateTime|
                               :DATATYPE
                               |http://www.w3.org/2001/XMLSchema|:|dateTime|))
                             (:DATATYPE
                              |http://www.w3.org/2006/time#|::|DateTimeInterval|))
  (DE.SETF.RESOURCE:DEFCLASS |http://www.w3.org/2006/time#|::|Year|
                             (|http://www.w3.org/2006/time#|::|DurationDescription|
                              #:|| #:|| #:|| #:|| #:|| #:|| #:||)
                             ()
                             (:DATATYPE
                              |http://www.w3.org/2006/time#|::|Year|))
  (DE.SETF.RESOURCE:DEFCLASS |http://www.w3.org/2006/time#|::|DurationDescription|
                             (#:|| #:|| #:|| #:|| #:|| #:|| #:||)
                             ((|http://www.w3.org/2006/time#|::|years| :TYPE
                               |http://www.w3.org/2001/XMLSchema|:|decimal|
                               :DATATYPE
                               |http://www.w3.org/2001/XMLSchema|:|decimal|)
                              (|http://www.w3.org/2006/time#|::|months| :TYPE
                               |http://www.w3.org/2001/XMLSchema|:|decimal|
                               :DATATYPE
                               |http://www.w3.org/2001/XMLSchema|:|decimal|)
                              (|http://www.w3.org/2006/time#|::|weeks| :TYPE
                               |http://www.w3.org/2001/XMLSchema|:|decimal|
                               :DATATYPE
                               |http://www.w3.org/2001/XMLSchema|:|decimal|)
                              (|http://www.w3.org/2006/time#|::|days| :TYPE
                               |http://www.w3.org/2001/XMLSchema|:|decimal|
                               :DATATYPE
                               |http://www.w3.org/2001/XMLSchema|:|decimal|)
                              (|http://www.w3.org/2006/time#|::|hours| :TYPE
                               |http://www.w3.org/2001/XMLSchema|:|decimal|
                               :DATATYPE
                               |http://www.w3.org/2001/XMLSchema|:|decimal|)
                              (|http://www.w3.org/2006/time#|::|minutes| :TYPE
                               |http://www.w3.org/2001/XMLSchema|:|decimal|
                               :DATATYPE
                               |http://www.w3.org/2001/XMLSchema|:|decimal|)
                              (|http://www.w3.org/2006/time#|::|seconds| :TYPE
                               |http://www.w3.org/2001/XMLSchema|:|decimal|
                               :DATATYPE
                               |http://www.w3.org/2001/XMLSchema|:|decimal|))
                             (:DATATYPE
                              |http://www.w3.org/2006/time#|::|DurationDescription|))
  (DE.SETF.RESOURCE:DEFCLASS |http://www.w3.org/2006/time#|::|ProperInterval|
                             (|http://www.w3.org/2006/time#|::|Interval|)
                             ((|http://www.w3.org/2006/time#|::|intervalEquals|
                               :TYPE
                               |http://www.w3.org/2006/time#|::|ProperInterval|
                               :DATATYPE
                               |http://www.w3.org/2006/time#|::|ProperInterval|)
                              (|http://www.w3.org/2006/time#|::|intervalBefore|
                               :TYPE
                               |http://www.w3.org/2006/time#|::|ProperInterval|
                               :DATATYPE
                               |http://www.w3.org/2006/time#|::|ProperInterval|)
                              (|http://www.w3.org/2006/time#|::|intervalMeets|
                               :TYPE
                               |http://www.w3.org/2006/time#|::|ProperInterval|
                               :DATATYPE
                               |http://www.w3.org/2006/time#|::|ProperInterval|)
                              (|http://www.w3.org/2006/time#|::|intervalOverlaps|
                               :TYPE
                               |http://www.w3.org/2006/time#|::|ProperInterval|
                               :DATATYPE
                               |http://www.w3.org/2006/time#|::|ProperInterval|)
                              (|http://www.w3.org/2006/time#|::|intervalStarts|
                               :TYPE
                               |http://www.w3.org/2006/time#|::|ProperInterval|
                               :DATATYPE
                               |http://www.w3.org/2006/time#|::|ProperInterval|)
                              (|http://www.w3.org/2006/time#|::|intervalDuring|
                               :TYPE
                               |http://www.w3.org/2006/time#|::|ProperInterval|
                               :DATATYPE
                               |http://www.w3.org/2006/time#|::|ProperInterval|)
                              (|http://www.w3.org/2006/time#|::|intervalFinishes|
                               :TYPE
                               |http://www.w3.org/2006/time#|::|ProperInterval|
                               :DATATYPE
                               |http://www.w3.org/2006/time#|::|ProperInterval|))
                             (:DATATYPE
                              |http://www.w3.org/2006/time#|::|ProperInterval|))
  (DE.SETF.RESOURCE:DEFCLASS |http://www.w3.org/2006/time#|::|Interval|
                             (|http://www.w3.org/2006/time#|::|TemporalEntity|)
                             ((|http://www.w3.org/2006/time#|::|inside| :TYPE
                               |http://www.w3.org/2006/time#|::|Instant|
                               :DATATYPE
                               |http://www.w3.org/2006/time#|::|Instant|))
                             (:DATATYPE
                              |http://www.w3.org/2006/time#|::|Interval|))
  (DE.SETF.RESOURCE:DEFCLASS |http://www.w3.org/2006/time#|::|Instant|
                             (|http://www.w3.org/2006/time#|::|TemporalEntity|)
                             ((|http://www.w3.org/2006/time#|::|inDateTime|
                               :TYPE
                               |http://www.w3.org/2006/time#|::|DateTimeDescription|
                               :DATATYPE
                               |http://www.w3.org/2006/time#|::|DateTimeDescription|)
                              (|http://www.w3.org/2006/time#|::|inXSDDateTime|
                               :TYPE
                               |http://www.w3.org/2001/XMLSchema|:|dateTime|
                               :DATATYPE
                               |http://www.w3.org/2001/XMLSchema|:|dateTime|))
                             (:DATATYPE
                              |http://www.w3.org/2006/time#|::|Instant|))
  (DE.SETF.RESOURCE:DEFCLASS |http://www.w3.org/2006/time#|::|TemporalEntity|
                             ()
                             ((|http://www.w3.org/2006/time#|::|before| :TYPE
                               |http://www.w3.org/2006/time#|::|TemporalEntity|
                               :DATATYPE
                               |http://www.w3.org/2006/time#|::|TemporalEntity|)
                              (|http://www.w3.org/2006/time#|::|hasBeginning|
                               :TYPE |http://www.w3.org/2006/time#|::|Instant|
                               :DATATYPE
                               |http://www.w3.org/2006/time#|::|Instant|)
                              (|http://www.w3.org/2006/time#|::|hasEnd| :TYPE
                               |http://www.w3.org/2006/time#|::|Instant|
                               :DATATYPE
                               |http://www.w3.org/2006/time#|::|Instant|)
                              (|http://www.w3.org/2006/time#|::|hasDurationDescription|
                               :TYPE
                               |http://www.w3.org/2006/time#|::|DurationDescription|
                               :DATATYPE
                               |http://www.w3.org/2006/time#|::|DurationDescription|))
                             (:DATATYPE
                              |http://www.w3.org/2006/time#|::|TemporalEntity|))))