;;;; http://nostdal.org/ ;;;;

(in-package #:sw)


(define-variable **compilation-inlining-p**
    :value nil
    :kind :global)


(defmethod maybe-inline (function-name-designator &key)
  (if **compilation-inlining-p**
      `(declaim (inline ,function-name-designator))
      nil))


(defmethod maybe-inline ((function-name-designator (eql 'run)) &key)
  nil)


(defmethod optimizations ((context t) &key)
  '(optimize (speed 0) (space 0) (safety 3) (debug 3) (compilation-speed 0)))


(defmethod optimizations ((context (eql :dom-property)) &key)
  '(optimize (speed 3) (space 0) (safety 0) (debug 3) (compilation-speed 0)))


(defmethod optimizations ((context (eql :widgets/dom-cache.lisp)) &key)
  '(optimize (speed 3)))


(defmethod optimizations ((context (eql :server-sw-http.lisp)) &key)
  '(optimize (speed 3)))


(defmethod optimizations ((context (eql :ajax.lisp)) &key)
  '(optimize (speed 3)))


(defmethod optimizations ((context (eql :comet.lisp)) &key)
  '(optimize (speed 3)))


(defmethod optimizations ((context (eql :shtml-of)) &key)
  '(optimize speed (safety 1)))