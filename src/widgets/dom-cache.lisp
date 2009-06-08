;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations))


(defun mk-dom-accessor-sym (name &key (prefix "") (suffix "-of"))
  (intern (string-upcase (format nil "~A~A~A" prefix name suffix))
          (symbol-package name)))

(defun mk-dom-initarg-sym (name &key (prefix "") (suffix ""))
  (intern (string-upcase (format nil "~A~A~A" prefix name suffix))
          :keyword))



(defclass dom-mirror ()
  ((dom-mirror-data :reader dom-mirror-data-of
                    :type hash-table
                    :initform (make-hash-table :test #'eq))

   (event-router :reader event-router-of
                 :type hash-table
                 :initform (make-hash-table :test #'eq))))


(defmethod render :after ((dom-mirror dom-mirror))
  (maphash (lambda (name value)
             (render-dom dom-mirror name value))
           (dom-mirror-data-of dom-mirror)))


(defmethod dom-mirror-before-writing ((dom-mirror dom-mirror) value args)
  (values dom-mirror value args))


;; Best way to understand this macro is to look at the expansions of it.
(defmacro def-dom-class (lisp-class-accessor-name lisp-dom-accessor-fn-name dom-name &key
                         (export-p t)
                         (accessor (mk-dom-accessor-sym lisp-class-accessor-name))
                         (reader (or accessor (error "No :ACCESSOR or :READER given.")))
                         (writer (or accessor (error "No :ACCESSOR or :WRITER given.")))
                         (reader-value-on-no-entry nil reader-value-on-no-entry-supplied-p)
                         (writer-old-value-binding-if-slot-is-unbound nil writer-old-value-binding-if-slot-is-unbound-supplied-p)
                         (writer-extra-keyargs nil writer-extra-keyargs-supplied-p)

                         (dom-get-code `(gethash ',lisp-class-accessor-name (dom-mirror-data-of dom-mirror)))

                         (writer-check-for-value-designating-removal-code nil writer-check-for-value-designating-removal-code-supplied-p)

                         (writer-value-marshaller-code nil writer-value-marshaller-code-supplied-p)

                         (remover-code `((remhash ',lisp-class-accessor-name (dom-mirror-data-of dom-mirror))))

                         (reader-code `((,lisp-dom-accessor-fn-name ,dom-name dom-mirror
                                          (lambda ()
                                            ,(if reader-value-on-no-entry-supplied-p
                                              `(multiple-value-bind (value found-p) ,dom-get-code
                                                 (if found-p
                                                     (values value found-p)
                                                     (values ,reader-value-on-no-entry :fallback)))
                                              dom-get-code)))))

                         (writer-code `((setf (,lisp-dom-accessor-fn-name ,dom-name dom-mirror
                                                :dom-cache-writer-fn
                                                (if render-only-p
                                                  (lambda (&optional inner-value) (declare (ignore inner-value)))
                                                  ,(let ((dom-set-code `(setf ,dom-get-code
                                                                              (if inner-value-supplied-p
                                                                                  inner-value
                                                                                  value))))
                                                     (if writer-check-for-value-designating-removal-code-supplied-p
                                                      `(if ,writer-check-for-value-designating-removal-code
                                                         (lambda (&optional (inner-value nil inner-value-supplied-p))
                                                           (declare (ignorable inner-value inner-value-supplied-p))
                                                           ,@remover-code)
                                                         (lambda (&optional (inner-value nil inner-value-supplied-p))
                                                           (declare (ignorable inner-value inner-value-supplied-p))
                                                           ,dom-set-code))
                                                      `(lambda (&optional (inner-value nil inner-value-supplied-p))
                                                         (declare (ignorable inner-value inner-value-supplied-p))
                                                         ,dom-set-code))))
                                                :server-only-p server-only-p
                                                ,@(when writer-extra-keyargs-supplied-p
                                                    (flatten
                                                     (loop :for keyarg :in writer-extra-keyargs
                                                        :collect (let ((keyarg (if (listp keyarg) (first keyarg) keyarg)))
                                                                   (list (format-symbol :keyword "~A" keyarg) keyarg))))))
                                              ,(if writer-value-marshaller-code-supplied-p
                                                   writer-value-marshaller-code
                                                   'value))))

                         (render-code `((run (js-code-of (setf (,lisp-dom-accessor-fn-name ,dom-name dom-mirror)
                                                               ,(if writer-value-marshaller-code-supplied-p
                                                                    writer-value-marshaller-code
                                                                    'value)))
                                             dom-mirror))))



  `(progn

     ,@(when reader
        `((defmethod ,reader ((dom-mirror dom-mirror))
            (declare (optimize speed))
            ,@reader-code)
          ,(when export-p
            `(export ',reader))))


     ,(when render-code
       `(defmethod render-dom ((dom-mirror dom-mirror) (name (eql ',lisp-class-accessor-name)) value)
          (declare (optimize speed))
          ,@render-code))


     ,@(when writer
        `((defmethod (setf ,writer) :around (value (dom-mirror dom-mirror) &rest args)
            (declare (optimize speed))
            (let ((*old-value* ,(if writer-old-value-binding-if-slot-is-unbound-supplied-p
                                    `(multiple-value-bind (value found-p) ,dom-get-code
                                       (if found-p
                                           value
                                           ,writer-old-value-binding-if-slot-is-unbound))
                                    dom-get-code)))
              (catch 'cancel-writer
                (multiple-value-bind (widget value args)
                    (dom-mirror-before-writing dom-mirror value args)
                  (apply #'call-next-method value widget args)))))

          (defmethod (setf ,writer) (value (dom-mirror dom-mirror)
                                     &key server-only-p render-only-p ,@writer-extra-keyargs)
            (declare (optimize speed))
            ,@writer-code)
          ,(when export-p
            `(export ',writer))))))
