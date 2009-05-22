;;;; http.//nostdal.org/ ;;;;

(in-package :sw)


(defparameter *address-bar-serializing-p* nil
  "This is T when we're in the process of serializing values from the URL (client)
to server side objects.")


(defvar *replace-address-bar-p* nil)


(defclass address-bar ()
  ((viewport :initarg :viewport :reader viewport-of
             :initform (error "Initarg :VIEWPORT needed."))
   
   (mutex :reader mutex-of
          :initform (make-recursive-lock))
   
   (objects :reader objects-of
            :initform nil)
   
   (dirty-p :reader dirty-p-of
            :initform nil)
   
   (replace-p :reader replace-p-of
              :initform t)))


#.(maybe-inline 'address-bar)
(defun address-bar ()
  "Returns the ADDRESS-BAR of the current viewport; *VIEWPORT*."
  (address-bar-of *viewport*))


(defmethod print-object ((address-bar address-bar) stream)
  (print-unreadable-object (address-bar stream :type t :identity t)
    (format stream "(URL-HASH ~S)" (query-str-of-address-bar address-bar))))


;; FIXME: The default value stuff for :REPLACE-P should be simpler ..
(defun add-to-address-bar (object &key (address-bar (address-bar))
                           (replace-p (or *replace-address-bar-p* *address-bar-serializing-p*
                                          (eq :no-history (urlized-p-of object)))))
  "Update (or add) server side URI representation based on new state of WIDGET."
  (declare (address-bar address-bar))
  (with-lock-held ((mutex-of address-bar))
    (pushnew object (slot-value address-bar 'objects))
    (when (urlized-p-of object)
      (maybe-update-address-bar address-bar :replace-p replace-p))))
  

(defun add-to-address-bar* (&rest objects)
  (let ((address-bar (address-bar)))
    (dolist (object objects)
      (add-to-address-bar object :address-bar address-bar))))

  
(defun remove-from-address-bar (object &key (address-bar (address-bar))
                                (replace-p *address-bar-serializing-p*))
  "Remove mention of WIDGET from URI."
  (declare (address-bar address-bar))
  (with-lock-held ((mutex-of address-bar))
    (deletef (slot-value address-bar 'objects) object)
    (maybe-update-address-bar address-bar :replace-p replace-p)))


(defun remove-from-address-bar* (&rest objects)
  (let ((address-bar (address-bar)))
    (dolist (object objects)
      (remove-from-address-bar object :address-bar address-bar))))


#.(maybe-inline 'handle-address-bar)
(defun handle-address-bar (object)
  ;; TODO: Should probably make this check optional when I'm done debugging widgets.
  (flet ((check ()
           (if (find object (objects-of (address-bar)))
               t
               (progn
                 (warn "HANDLE-ADDRESS-BAR: ~A not found in address bar of ~A (no action done with URL)" object *viewport*)
                 nil))))
    (declare (inline check))
    (when-let (urlized-p (urlized-p-of object))
      (case urlized-p
        ((t) (when (check) (add-to-address-bar object)))
        (:no-history (when (check) (add-to-address-bar object :replace-p t)))
        (otherwise (error "Slot URLIZED-P in ~A has invalid value ~A." object urlized-p))))))


#.(maybe-inline 'maybe-update-address-bar)
(defun maybe-update-address-bar (address-bar &key
                                 (replace-p *address-bar-serializing-p*))
  (declare (address-bar address-bar))
  (when (not *address-bar-serializing-p*)
    (with-recursive-lock-held ((mutex-of address-bar))
      ;; We don't want to overwrite this with a T value later; one widget wanting a new history entry should take precedence over
      ;; those that don't.
      (unless replace-p (nilf (slot-value address-bar 'replace-p)))
      (when (not (dirty-p-of address-bar)) ;; Make sure we don't send more JS to the client than needed.
        (tf (slot-value address-bar 'dirty-p))
        (push (lambda ()
                (with-lock-held ((mutex-of address-bar))
                  ;; Using RUN-JS because we don't want to push stuff to the DO-AT-END slot (we're already handling this slot).
                  (run-js (catstr "updateHash(\"" (query-str-of-address-bar address-bar) "\", "
                                  (if (replace-p-of address-bar) "true" "false") ");")
                          (viewport-of address-bar))
                  (nilf (slot-value address-bar 'dirty-p))
                  (tf (slot-value address-bar 'replace-p))))
              (do-at-end-of (viewport-of address-bar)))))))


(defun add-history-entry (&optional (address-bar nil address-bar-supplied-p))
  "Add a history entry at the current location or state in time to the client side browser history."
  (let ((address-bar (if address-bar-supplied-p
                         address-bar
                         (address-bar-of *viewport*))))
    ;; NOTE: Instead of using (window.location.hash).substring(1) we generate
    ;; the server to be certain things are right.
    (run (catstr "swUpdateHash(\"" (query-str-of-address-bar address-bar)  "\", false);")
         (viewport-of address-bar))))


(defun query-str-of-address-bar (address-bar)
  "The hash part of an URI."
  (declare (address-bar address-bar))
  (with-recursive-lock-held ((mutex-of address-bar))
    (if (zerop (length (objects-of address-bar)))
        ""
        (string-left-trim "&"
                          (with-output-to-string (s)
                            (dolist (object (reverse (objects-of address-bar)))
                              (when (urlized-p-of object)
                                (princ #\& s)
                                (princ (id-of object) s)
                                (princ #\= s)
                                (princ (url-encode (uri-value<-state object)) s))))))))


(defun mk-href (objects)
  (subseq
   (with-output-to-string (s)
     (dolist (object objects)
       (princ #\& s)
       (princ (id-of object) s)
       (princ #\= s)
       (princ (url-encode (uri-value<-state object)) s)))
   1))


#.(maybe-inline 'sync-widgets)
(defun sync-widgets (hash-string page-load-p)
  "Sync server side widgets with client side URI: client-state -> server-state.
PAGE-LOAD-P: T, user is currently loading or refreshing the page.
             NIL, user is using back/forward buttons in browser."
  (declare (string hash-string))
  (setf hash-string (url-decode hash-string))
  (unless (zerop (length hash-string))
    (let ((*address-bar-serializing-p* t)
          (sync-last nil)) ;; List of objects to sync last.
      (flet ((do-sync (key-value object)
               (when (or page-load-p ;; Ok, NIL; after this point the user is doing the browser back-button thing..
                         ;; ..which means we do NOT want to serialize from url to state if things are :NO-HISTORY.
                         (not (eq :no-history (urlized-p-of object))))
                 (state<-uri-value (if-let (value (second key-value)) (url-decode value) "")
                                   object
                                   (not page-load-p)))))
        (declare (inline do-sync))
        (dolist (key-value (group 2 (split "[&=]" hash-string)))
          ;; The reason we search in *VIEWPORT* first, then in *APP*, is because cross-viewport widgets might have the same ID.
          (if-let (object (let ((id (first key-value)))
                            (if-let (object (find id (objects-of (address-bar)) :key #'id-of :test #'string=))
                              object
                              (if-let (widget (gethash id (widgets-of *viewport*)))
                                widget
                                (gethash id (widgets-of *app*))))))
            ;; FIXME: This is a stupid workaround to ensure that these widget types are dealt with last.
            ;; Now, as to why this needs to happen; it might not matter in most cases, but I have 1 case
            ;; where these things matter ... but, this isn't good. Some options:
            ;; * Let the user control sequence of serialization.
            ;; * Make sure these things do not matter (i like this option the best ..).
            ;; NOTE-TO-SELF (Lars): This was needed for the "Våre Falne" project (the GENERATE-HTML method in search-results.lisp).
            (if (or (typep object 'location-callback)
                    (typep object 'location-container))
                (push (iambda (do-sync key-value object)) sync-last)
                (do-sync key-value object))
            (progn
              #| TODO:
              This might happen when one of the "URL-entries" triggers the addition of a widget
              to the (WIDGETS-OF *APP*) hash-table.
              A solution is to collect these then try again at the end after DOLIST has finished
              and keep trying until nothing is added anymore.
              Currently the user will have to make sure the sequence of key/value
              pairs in his URL is correct; first pair adds widget to container ; ;
              (in viewport), and second pair manipulates this widget.
              |#
            (warn "SYNC-WIDGETS: Tried to manipulate an object (~S) which doesn't exist in session ~A."
                  (first key-value) (id-of *app*)))))
        (dolist (fn sync-last) (funcall fn)))))
  (unless (string= (query-str-of-address-bar (address-bar-of *viewport*))
                   hash-string)
    (maybe-update-address-bar (address-bar-of *viewport*) :replace-p t)))



