;;;; http://nostdal.org/ ;;;;

(in-package #:sw)


(define-variable -allow-compilation-inlining-p-
    :value nil
    :kind :global)


(define-variable -default-compilation-inlining-p-
    :value nil
    :kind :global)


(defmethod maybe-inline :around (function-name &key)
  (if (and -allow-compilation-inlining-p- (call-next-method))
      `(declaim (inline ,function-name))
      nil))


(defmethod maybe-inline (function-name &key)
  (if -default-compilation-inlining-p-
      `(declaim (inline ,function-name))
      nil))



;;; INLINE declarations.

(defmethod maybe-inline ((function-name (eql 'run-js)) &key)
  t)


(defmethod maybe-inline ((function-name (eql 'run)) &key)
  t)


(defmethod maybe-inline ((function-name (eql 'append-to-response-data-of)) &key)
  t)



;;; OPTIMIZE declarations.

(defmethod optimizations ((context t) &key)
  '(optimize (speed 0) (space 0) (safety 3) (debug 3) (compilation-speed 0)))


(defmethod optimizations ((context (eql :dom-property)) &key)
  '(optimize (speed 3) (space 0) (safety 0) (debug 3) (compilation-speed 0)))


(defmethod optimizations ((context (eql :widgets/dom-cache.lisp)) &key)
  '(optimize (speed 3) (safety 2)))


(defmethod optimizations ((context (eql :widgets/css.lisp)) &key)
  '(optimize (speed 3) (safety 2)))


(defmethod optimizations ((context (eql :widgets/attributes.lisp)) &key)
  '(optimize (speed 3) (safety 2)))


(defmethod optimizations ((context (eql :server-sw-http.lisp)) &key)
  '(optimize (speed 3) (safety 2)))


(defmethod optimizations ((context (eql :ajax.lisp)) &key)
  '(optimize (speed 3) (safety 2)))


(defmethod optimizations ((context (eql :comet.lisp)) &key)
  '(optimize (speed 3) (safety 2)))


(defmethod optimizations ((context (eql :shtml-of)) &key)
  '(optimize (speed 3) (safety 2)))


(defmethod optimizations ((context (eql :code.lisp)) &key)
  '(optimize (speed 3) (safety 2)))


(defmethod optimizations ((context (eql :viewport.lisp)) &key)
  '(optimize (speed 3) (safety 2)))
