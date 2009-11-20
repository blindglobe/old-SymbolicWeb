;;;; http://nostdal.org/ ;;;;

(in-package sw)
(in-readtable symbolicweb)
(declaim #.(optimizations :widgets/dom-cache.lisp))


(defclass dom-mirror ()
  ((dom-mirror-data :reader dom-mirror-data-of
                    :type hash-table
                    :initform (make-hash-table :test #'eq :synchronized t)
                    :documentation "
DOM attributes, CSS properties etc.")

   (callbacks :reader callbacks-of
              :type hash-table
              :initform (make-hash-table :test #'equal :synchronized t)
              :documentation "
Strong hash table; ID->CALLBACK-BOX.")))


(defgeneric render-dom (dom-mirror property-name property-vaule))


(defmethod render :after ((dom-mirror dom-mirror))
  (let ((dom-mirror-data (dom-mirror-data-of dom-mirror)))
    (sb-ext:with-locked-hash-table (dom-mirror-data)
      (maphash (lambda (name value)
                 (render-dom dom-mirror name value))
               dom-mirror-data))))


(declaim (inline dom-server-reader))
(defun dom-server-reader (dom-mirror lisp-accessor-name)
  (declare (dom-mirror dom-mirror)
           (symbol lisp-accessor-name))
  (let ((dom-mirror-data (dom-mirror-data-of dom-mirror)))
    (sb-ext:with-locked-hash-table (dom-mirror-data)
      (gethash lisp-accessor-name dom-mirror-data))))


(declaim (inline dom-server-writer))
(defun dom-server-writer (dom-mirror lisp-accessor-name new-value)
  (declare (dom-mirror dom-mirror)
           (symbol lisp-accessor-name))
  (let ((dom-mirror-data (dom-mirror-data-of dom-mirror)))
    (sb-ext:with-locked-hash-table (dom-mirror-data)
      (setf (gethash lisp-accessor-name (dom-mirror-data-of dom-mirror))
            new-value))))


(declaim (inline dom-server-remover))
(defun dom-server-remover (dom-mirror lisp-accessor-name)
  (declare (dom-mirror dom-mirror)
           (symbol lisp-accessor-name))
  (remhash lisp-accessor-name (dom-mirror-data-of dom-mirror)))


(defmethod define-dom-property ((lisp-accessor-name symbol)
                                &key
                                (lisp-reader-name lisp-accessor-name)
                                (lisp-writer-name (list 'setf lisp-accessor-name))

                                (dom-client-reader
                                 (lambda (%not-used dom-mirror)
                                   (declare (ignore %not-used))
                                   (error "~A: A DOM-CLIENT-READER for ~A isn't implemented."
                                          lisp-accessor-name dom-mirror)))

                                (dom-client-writer
                                 (lambda (%not-used dom-mirror)
                                   (declare (ignore %not-used))
                                   (error "~A: A DOM-CLIENT-WRITER for ~A isn't implemented."
                                          lisp-accessor-name dom-mirror)))

                                (dom-client-remover
                                 (lambda (%not-used dom-mirror)
                                   (declare (ignore %not-used))
                                   (error "~A: A DOM-CLIENT-REMOVER for ~A isn't implemented."
                                          lisp-accessor-name dom-mirror)))

                                (dom-server-reader-fallback-value nil dom-server-reader-fallback-value-supplied-p)
                                (dom-server-reader #'dom-server-reader)
                                (dom-server-writer #'dom-server-writer)
                                (dom-server-remover #'dom-server-remover)
                                (value-marshaller #'princ-to-string)
                                (value-removal-checker #'not)) ;; Essentially (lambda (value) (eq value nil)).
  (declare (function
            dom-client-reader dom-client-writer dom-client-remover
            dom-server-reader dom-server-writer dom-server-remover))

  (setf (get lisp-accessor-name 'value-marshaller) value-marshaller)

  ;; Add DOM reader.
  (compile-and-execute
    `(defun ,lisp-reader-name (dom-mirror)
       (declare #.(optimizations :dom-property)
                (dom-mirror dom-mirror))
       (if *js-code-only-p*
           (funcall ,dom-client-reader dom-mirror)
           (multiple-value-bind (value found-p)
               (funcall ,dom-server-reader dom-mirror ',lisp-accessor-name)
             (if found-p
                 (values value found-p)
                 ,(when dom-server-reader-fallback-value-supplied-p
                   `(values ,dom-server-reader-fallback-value :fallback)))))))

  ;; Add DOM writer.
  (compile-and-execute
    `(defun ,lisp-writer-name (property-value dom-mirror &rest args &key client-only-p &allow-other-keys)
       (declare #.(optimizations :dom-property)
                (dom-mirror dom-mirror)
                ((member nil t) client-only-p))
       (delete-from-plistf args :client-only-p)
       (let ((remove-entry-p
              (and ,value-removal-checker (funcall ,value-removal-checker property-value))))
         (flet ((client-writer ()
                  (if remove-entry-p
                      ;; Remove entry on client side.
                      (apply ,dom-client-remover dom-mirror args)
                      ;; Add and/or set entry on client side.
                      (apply ,dom-client-writer
                             ,(if value-marshaller
                                  `(funcall ,value-marshaller property-value)
                                  `property-value)
                             dom-mirror args)))
                (server-writer ()
                  (unless client-only-p
                    (if remove-entry-p
                        ;; Remove entry on server side.
                        (funcall ,dom-server-remover dom-mirror ',lisp-accessor-name)
                        ;; Add and/or set entry on server side.
                        (funcall ,dom-server-writer dom-mirror ',lisp-accessor-name property-value)))))
           (declare (inline client-writer server-writer))
           (if *js-code-only-p*
               (client-writer)
               (prog1 property-value
                 (server-writer)
                 (client-writer)))))))

  ;; Add DOM renderer.
  (compile-and-execute
    ;; TOOD: Using a method to dispatch wrt. PROPERTY-NAME might be a bit overkill; try to think of a faster way?
    `(defmethod render-dom ((dom-mirror dom-mirror) (property-name (eql ',lisp-accessor-name)) property-value)
       (declare #.(optimizations :dom-property))
       (run (js-code-of (funcall ,dom-client-writer
                                 ,(if value-marshaller
                                      `(funcall ,value-marshaller property-value)
                                      `property-value)
                                 dom-mirror))
            dom-mirror))))
(export 'define-dom-property)


(defmethod remove-dom-property ((lisp-accessor-name symbol) (dom-client-remover function)
                                &key
                                (lisp-reader-name lisp-accessor-name)
                                (lisp-writer-name (list 'setf lisp-accessor-name))
                                (dom-server-remover #'dom-server-remover))
  (declare (function dom-server-remover))
  ;; Remove entry from DOM-MIRROR-DATA hash-table and call DOM-CLIENT-REMOVER.
  ;; We do this by iterating through every active widget of *SERVER*.
  (with-each-viewport-in-server ()
    (with-each-widget-in-tree (:root (root-widget-of *viewport*))
      (funcall dom-server-remover widget lisp-accessor-name)
      ;; TODO: This thing needs a replacement.
      #|(funcall dom-client-remover dom-name widget)|#))

  ;; Remove DOM reader.
  (fmakunbound lisp-reader-name)

  ;; Remove DOM writer.
  (fmakunbound lisp-writer-name)

  ;; Remove DOM renderer.
  (remove-method (ensure-function 'render-dom)
                 (find-method (ensure-function 'render-dom)
                              nil
                              (list (find-class 'dom-mirror)
                                    (make-instance 'eql-specializer :object lisp-accessor-name)
                                    (find-class 't)))))
(export 'remove-dom-property)


(defun value-marshaller-of (lisp-accessor-name)
  (declare (symbol lisp-accessor-name))
  (with1 (get lisp-accessor-name 'value-marshaller)
    (check-type it function)))
(export 'value-marshaller-of)