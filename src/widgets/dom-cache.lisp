;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations))




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
                     value-marshaller value-removal-checker)
           (inline dom-server-reader dom-server-writer dom-server-remover
                   value-marshaller value-removal-checker))

  ;; Add DOM reader.
  (ensure-generic-function lisp-reader-name :lambda-list '(dom-mirror))
  (add-method (ensure-function lisp-reader-name)
              (make-instance 'standard-method
                             :lambda-list '(dom-mirror)
                             :specializers (list (find-class 'dom-mirror))
                             :function
                             (lambda (args method)
                               (declare (ignore method))
                               (let ((dom-mirror (first args)))
                                 (if *js-code-only-p*
                                     (funcall dom-client-reader dom-name dom-mirror)
                                     (multiple-value-bind (value found-p)
                                         (funcall dom-server-reader dom-mirror lisp-accessor-name)
                                       (if found-p
                                           (values value found-p)
                                           (when dom-server-reader-fallback-value-supplied-p
                                             (values dom-server-reader-fallback-value :fallback)))))))))

  ;; Add DOM writer.
  (ensure-generic-function lisp-writer-name :lambda-list '(property-value dom-mirror &rest args))
  (add-method (ensure-function lisp-writer-name)
              (make-instance 'standard-method
                             :lambda-list '(property-value dom-mirror &rest args)
                             :specializers (list (find-class 't) (find-class 'dom-mirror))
                             :function
                             (lambda (args method)
                               (declare (ignore method))
                               (let ((property-value (first args))
                                     (dom-mirror (second args)))
                                 (prog1 property-value
                                   (unless *js-code-only-p*
                                     (if (funcall value-removal-checker property-value)
                                         (progn
                                           (funcall dom-server-remover dom-mirror lisp-accessor-name)
                                           (apply dom-client-remover dom-name dom-mirror (cddr args)))
                                         (progn
                                           (funcall dom-server-writer dom-mirror lisp-accessor-name property-value)
                                           (apply dom-client-writer
                                                  (funcall value-marshaller property-value)
                                                  dom-name dom-mirror (cddr args))))))))))

  ;; Add DOM renderer.
  (add-method (ensure-function 'render-dom)
              (make-instance 'standard-method
                             :lambda-list '(dom-mirror property-name property-value)
                             :specializers (list (find-class 'dom-mirror)
                                                 (make-instance 'eql-specializer :object lisp-accessor-name)
                                                 (find-class 't))
                             :function
                             (lambda (args method)
                               (declare (ignore method))
                               (let ((dom-mirror (first args))
                                     ;;(property-name (second args))
                                     (property-value (third args)))
                                 (run (js-code-of (funcall dom-client-writer
                                                           (funcall value-marshaller property-value)
                                                           dom-name dom-mirror))
                                      dom-mirror))))))


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
  (remove-method (ensure-function lisp-reader-name)
                 (find-method (ensure-function lisp-reader-name)
                              nil
                              (list (find-class 'dom-mirror))))

  ;; Remove DOM writer.
  (remove-method (ensure-function lisp-writer-name)
                 (find-method (ensure-function lisp-writer-name)
                              nil
                              (list (find-class 't) (find-class 'dom-mirror))))

  ;; Remove DOM renderer.
  (remove-method (ensure-function 'render-dom)
                 (find-method (ensure-function 'render-dom)
                              nil
                              (list (find-class 'dom-mirror)
                                    (make-instance 'eql-specializer :object lisp-accessor-name)
                                    (find-class 't)))))
