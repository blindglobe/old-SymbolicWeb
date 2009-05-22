;;;; http://nostdal.org/ ;;;;

(in-package #:sw)


(defparameter *compilation-inlining-p* nil)


(defmethod maybe-inline (function-name-designator &key)
  (if *compilation-inlining-p*
      `(declaim (inline ,function-name-designator))
      nil))


(defmethod maybe-inline ((function-name-designator (eql 'run)) &key)
  nil)


(defmethod optimizations (&key context)
  (declare (ignore context))
  '(optimize (speed 0) (space 0) (safety 3) (debug 3) (compilation-speed 0)))
