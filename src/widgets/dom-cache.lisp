;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations :widgets/dom-cache.lisp))


#| TODO:
There is a chance "value marshalling" isn't needed here anymore since it is
something the Model -> View connection (SW-MVC) can take care of. Or, well,
I need to think about stuff like CSS-CLASS-OF.
|#


(defclass dom-mirror ()
  ((dom-mirror-data :reader dom-mirror-data-of
                    :type hash-table
                    :initform (make-hash-table :test #'eq))

   ;; TODO: I think I can remove this now.
   (event-router :reader event-router-of
                 :type hash-table
                 :initform (make-hash-table :test #'eq))))


(defgeneric render-dom (dom-mirror property-name property-vaule))


(defmethod render :after ((dom-mirror dom-mirror))
  (maphash (lambda (name value)
             (render-dom dom-mirror name value))
           (dom-mirror-data-of dom-mirror)))


(defun dom-server-reader (dom-mirror lisp-accessor-name)
  (declare (dom-mirror dom-mirror)
           (symbol lisp-accessor-name))
  (gethash lisp-accessor-name (dom-mirror-data-of dom-mirror)))


(defun dom-server-writer (dom-mirror lisp-accessor-name new-value)
  (declare (dom-mirror dom-mirror)
           (symbol lisp-accessor-name))
  (setf (gethash lisp-accessor-name (dom-mirror-data-of dom-mirror))
        new-value))


(defun dom-server-remover (dom-mirror lisp-accessor-name)
  (declare (dom-mirror dom-mirror)
           (symbol lisp-accessor-name))
  (remhash lisp-accessor-name (dom-mirror-data-of dom-mirror)))


(defmethod define-dom-property ((lisp-accessor-name symbol) (dom-name string)
                                (dom-client-writer function)
                                (dom-client-reader function)
                                (dom-client-remover function)
                                &key
                                (lisp-reader-name lisp-accessor-name)
                                (lisp-writer-name (list 'setf lisp-accessor-name))
                                (dom-server-reader-fallback-value nil dom-server-reader-fallback-value-supplied-p)
                                (dom-server-reader #'dom-server-reader)
                                (dom-server-writer #'dom-server-writer)
                                (dom-server-remover #'dom-server-remover)
                                (value-marshaller #'princ-to-string)
                                (value-removal-checker #'not)) ;; Essentially (lambda (value) (eq value nil)).
  (declare (function dom-server-reader dom-server-writer dom-server-remover
                     value-removal-checker))

  ;; Add DOM reader.
  (compile-and-execute
    `(defun ,lisp-reader-name (dom-mirror)
       (declare (dom-mirror dom-mirror))
       (if *js-code-only-p*
           (funcall ,dom-client-reader ,dom-name dom-mirror)
           (multiple-value-bind (value found-p)
               (funcall ,dom-server-reader dom-mirror ',lisp-accessor-name)
             (if found-p
                 (values value found-p)
                 ,(when dom-server-reader-fallback-value-supplied-p
                   `(values ,dom-server-reader-fallback-value :fallback)))))))

  ;; Add DOM writer.
  (compile-and-execute
    `(defun ,lisp-writer-name (property-value dom-mirror &rest args)
       (declare (dom-mirror dom-mirror))
       (prog1 property-value
         (unless *js-code-only-p*
           (if (funcall ,value-removal-checker property-value)
               (progn
                 (funcall ,dom-server-remover dom-mirror ',lisp-accessor-name)
                 (apply ,dom-client-remover ,dom-name dom-mirror args))
               (progn
                 (funcall ,dom-server-writer dom-mirror ',lisp-accessor-name property-value)
                 (apply ,dom-client-writer
                        ,(if value-marshaller
                             `(funcall ,value-marshaller property-value)
                             `property-value)
                        ,dom-name dom-mirror args)))))))

  ;; Add DOM renderer.
  (compile-and-execute
    `(defmethod render-dom ((dom-mirror dom-mirror) (property-name (eql ',lisp-accessor-name)) property-value)
       (run (js-code-of (funcall ,dom-client-writer
                                 ,(if value-marshaller
                                      `(funcall ,value-marshaller property-value)
                                      `property-value)
                                 ,dom-name
                                 dom-mirror))
            dom-mirror))))
(export 'define-dom-property)


(defmethod remove-dom-property ((lisp-accessor-name symbol) (dom-name string) (dom-client-remover function)
                                &key
                                (lisp-reader-name lisp-accessor-name)
                                (lisp-writer-name (list 'setf lisp-accessor-name))
                                (dom-server-remover #'dom-server-remover))

  ;; Remove entry from DOM-MIRROR-DATA hash-table and call DOM-CLIENT-REMOVER.
  ;; We do this by iterating through every active widget of *SERVER*.
  (with-each-viewport-in-server ()
    (with-each-widget-in-tree (:root (root-widget-of *viewport*))
      (funcall dom-server-remover widget lisp-accessor-name)
      (funcall dom-client-remover dom-name widget)))

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
