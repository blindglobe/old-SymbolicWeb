;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations :object.lisp))


(defclass object ()
  ((urlized-p :reader urlized-p-of
              :initform nil
              :documentation "
Initarg can be one of T :T-ADD :NO-HISTORY :NO-HISTORY-ADD NIL :NIL-ADD.")))


(defmethod initialize-instance :after ((object object) &key urlized-p)
  (when urlized-p
    (let ((opr (if (member urlized-p '(:t-add :no-history-add :nil-add))
                   :add
                   nil))
          (urlized-p (case urlized-p
                       (:t-add t)
                       (:no-history-add :no-history)
                       (:nil-add nil)
                       (otherwise urlized-p))))
      (setf (urlized-p-of object opr) urlized-p))))


(defmethod (setf urlized-p-of) (new-state (object object) &optional opr)
  "NEW-STATE can be one of: T :NO-HISTORY NIL
OPR can be one of: NIL :ADD :REMOVE"
  (setf (slot-value object 'urlized-p) new-state)
  (case opr
    ((nil)) ;; lol @ CL.
    (:add (add-to-address-bar object :replace-p t))
    (:remove (remove-from-address-bar object :replace-p t))
    (otherwise (error "Invalid argument ~A given to parameter OPR of (SETF URLIZED-P-OF)." opr))))


(defmethod uri-value<-state ((object object))
  (error "Tried to serialize state of ~A to URL, but a URI-VALUE<-STATE method has not been defined for ~A."
         (type-of object) (type-of object)))


(defmethod state<-uri-value ((uri-value string) (object object) from-browser-history-p)
  "FROM-BROWSER-HISTORY-P is T when the state change comes from the user manipulating
the browser back/forward buttons."
  (declare (ignore from-browser-history-p))
  (error "Tried to de-serialize ~A (from URL) to ~A, but a STATE<-URI-VALUE method has not been defined for ~A."
         uri-value (type-of object) (type-of object)))
