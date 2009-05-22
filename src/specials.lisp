;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(defvar *id->object*
  (make-hash-table :test #'equal :weakness :value :synchronized t))

(defvar *server* nil)
(export '*server*)

(defvar *app* nil)
(export '*app*)

(defvar *viewport* nil)
(export '*viewport*)


;; Ok, this might be somewhat of a "wrong" thing to do..
(define-symbol-macro *root* (root-widget-of *viewport*))
(export '*root*)


(defvar *request-type* :unknown
  ":AJAX, :COMET, :REGULAR or :UNKNOWN.")
(export '*request-type*)


(defvar *js-code-only-p* nil
  "Used by the macros JS-CODE-OF and WITH-JS-CODE-FROM (src/js/util.lisp).")


;; :AROUND INITIALIZE-INSTANCE ((.. widget) ..)
(defvar *currently-constructing-widget* nil)


;; ajax.lisp
(defvar *current-event-widget* nil)


;; Used by WITH-CODE-BLOCK (code.lisp).
(defvar *code-block*)
(defvar *creating-code-block-p* nil)


;; Used by things like MAYBE-VALUE-CHANGE.
(defvar *old-value*)
(export '*old-value*)
(defvar *new-value*)
(export '*new-value*)


;;; Stuff for HTML-CONTAINER.
(defvar *creating-html-container-p* nil)
(defvar *html-container-children* nil)


;; Other stuff.
(define-symbol-macro +newline+ #.(format nil "~C" #\Newline))
(export '+newline+)


(defvar *request-time* nil)
