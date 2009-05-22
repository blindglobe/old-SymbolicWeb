;;;; http://nostdal.org/ ;;;;

(in-package :sw-jquery)


(defmethod optimizations (&key context)
  (declare (ignore context))
  '(optimize (speed 0) (space 0) (safety 3) (debug 3) (compilation-speed 0)))
