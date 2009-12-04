;;;; http://nostdal.org/ ;;;;

(in-package sw)
(in-readtable symbolicweb)
(declaim #.(optimizations :widgets/dom-cache.lisp))


(defmethod define-dom-property ((lisp-accessor-name symbol)
                                &key
                                (lisp-reader-name lisp-accessor-name)
                                (lisp-writer-name (list 'setf lisp-accessor-name))

                                (dom-client-reader
                                 (lambda (%not-used widget)
                                   (declare (ignore %not-used))
                                   (error "~A: A DOM-CLIENT-READER for ~A isn't implemented."
                                          lisp-accessor-name widget)))

                                (dom-client-writer
                                 (lambda (%not-used widget)
                                   (declare (ignore %not-used))
                                   (error "~A: A DOM-CLIENT-WRITER for ~A isn't implemented."
                                          lisp-accessor-name widget)))

                                (dom-client-remover
                                 (lambda (%not-used widget)
                                   (declare (ignore %not-used))
                                   (error "~A: A DOM-CLIENT-REMOVER for ~A isn't implemented."
                                          lisp-accessor-name widget)))

                                dom-server-reader
                                (value-marshaller #'princ-to-string)
                                (value-removal-checker #'not)) ;; Essentially (lambda (value) (eq value nil)).
  (declare (function
            dom-client-reader dom-client-writer dom-client-remover))

  (setf (get lisp-accessor-name 'value-marshaller) value-marshaller)

  ;; Add DOM reader.
  (eval
   `(defun ,lisp-reader-name (widget)
      (declare #.(optimizations :dom-property)
               (widget-base widget))
      (if *js-code-only-p*
          (funcall ,dom-client-reader widget)
          ,(when dom-server-reader
            `(funcall ,dom-server-reader widget ',lisp-accessor-name)))))

  ;; Add DOM writer.
  (eval
   `(defun ,lisp-writer-name (property-value widget &rest args &key client-only-p &allow-other-keys)
      (declare #.(optimizations :dom-property)
               (widget-base widget)
               ((member nil t) client-only-p))
      (delete-from-plistf args :client-only-p)
      (let ((remove-entry-p
             (and ,value-removal-checker (funcall ,value-removal-checker property-value))))
        (flet ((client-writer ()
                 (if remove-entry-p
                     ;; Remove entry on client side.
                     (apply ,dom-client-remover widget args)
                     ;; Add and/or set entry on client side.
                     (apply ,dom-client-writer
                            ,(if value-marshaller
                                 `(funcall ,value-marshaller property-value)
                                 `property-value)
                            widget args))))
          (if *js-code-only-p*
              (client-writer)
              (prog1 property-value
                (client-writer))))))))


(defmethod remove-dom-property ((lisp-accessor-name symbol) (dom-client-remover function)
                                &key
                                (lisp-reader-name lisp-accessor-name)
                                (lisp-writer-name (list 'setf lisp-accessor-name)))

  ;; Remove DOM reader.
  (fmakunbound lisp-reader-name)

  ;; Remove DOM writer.
  (fmakunbound lisp-writer-name))


(defun value-marshaller-of (lisp-accessor-name)
  (declare (symbol lisp-accessor-name))
  (with1 (get lisp-accessor-name 'value-marshaller)
    (check-type it function)))
