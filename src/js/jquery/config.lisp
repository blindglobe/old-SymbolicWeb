;;;; http://nostdal.org/ ;;;;

(in-package :sw-jquery)


(defvar *js-before* "return true;") (export '*js-before*)
(defvar *js-after* "") (export '*js-after*)


(defmethod optimizations (&key context)
  (declare (ignore context))
  ;;'(optimize (speed 0) (space 0) (safety 3) (debug 3) (compilation-speed 0))
  '(optimize (speed 3) (safety 0) (compilation-speed 0))
  )
