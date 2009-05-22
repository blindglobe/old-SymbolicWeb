;;;; http://nostdal.org/ ;;;;

(in-package #:sw)


(defclass pagination (object)
  ((add-entries-fn :accessor add-entries-fn-of
                   :initform (iambda))
   
   (remove-entries-fn :accessor remove-entries-fn-of
                      :initform (iambda))
   
   (entry-ids :reader entry-ids-of
              :type list
              :initform nil)
   
   (current-page :reader current-page-of
                 :type integer
                 :initform 0)
   
   (current-entry-widgets :reader current-entry-widgets-of
                          :type list
                          :initform nil)
   
   (num-entries-pr-page :reader num-entries-pr-page-of :initarg :num-entries-pr-page
                        :type integer
                        :initform 10)
   
   (total-num-entries :reader total-num-entries-of
                      :type integer
                      :initform 0)
   
   (total-num-pages :reader total-num-pages-of :initarg :total-num-pages
                    :type integer
                    :initform 0)
   
   (on-page-change-fn :accessor on-page-change-fn-of :initarg :on-page-change-fn
                      :initform (iambda))
   
   (on-state-change-of-pages-before-current-p :accessor on-state-change-of-pages-before-current-p-of
                                              :initform (iambda))
   
   (on-state-change-of-pages-after-current-p :accessor on-state-change-of-pages-after-current-p-of
                                             :initform (iambda))))
(export '(pagination add-entries-fn-of remove-entries-fn-of entry-ids-of current-page-of num-entries-pr-page-of total-num-entries-of
          total-num-pages-of on-state-change-of-pages-before-current-p-of on-state-change-of-pages-after-current-p-of
          on-page-change-fn-of
          ))


(defmethod (setf entry-ids-of) (new-entry-ids (pagination pagination))
  (setf (slot-value pagination 'entry-ids) new-entry-ids
        (slot-value pagination 'total-num-entries) (length new-entry-ids)
        (slot-value pagination 'total-num-pages) (ceiling (/ (total-num-entries-of pagination)
                                                             (num-entries-pr-page-of pagination))))
  ;; We do this to make sure stuff "boots up" proper.
  (funcall (on-state-change-of-pages-before-current-p-of pagination)
           (more-pages-before-current-p-of pagination))
  (funcall (on-state-change-of-pages-after-current-p-of pagination)
           (more-pages-after-current-p-of pagination))
  (render pagination))
(export 'entry-ids-of)


(defmethod num-pages-before-current-of ((pagination pagination))
  (current-page-of pagination))
(export 'num-pages-before-current-of)


(defmethod num-pages-after-current-of ((pagination pagination))
  (max (- (total-num-pages-of pagination) (current-page-of pagination) 1)
       0))
(export 'num-pages-after-current-of)


(defmethod more-pages-after-current-p-of ((pagination pagination))
  (plusp (num-pages-after-current-of pagination)))
(export 'more-pages-after-current-p-of)


(defmethod more-pages-before-current-p-of ((pagination pagination))
  (plusp (num-pages-before-current-of pagination)))
(export 'more-pages-before-current-p-of)


(defmethod render ((pagination pagination))
  ;; Remove old widgets..
  (when (current-entry-widgets-of pagination)
    (funcall (remove-entries-fn-of pagination)
             (current-entry-widgets-of pagination))
    (nilf (slot-value pagination 'current-entry-widgets)))
  ;; ..and add new ones.
  (let ((entries (get-entries-for-page pagination (current-page-of pagination))))
    (appendf (slot-value pagination 'current-entry-widgets)
             (mklst (funcall (add-entries-fn-of pagination)
                             entries)))))


(defmethod get-entries-for-page ((pagination pagination) (page-index integer))
  (let ((start (* (num-entries-pr-page-of pagination) page-index)))
    (if (> start (total-num-entries-of pagination))
        nil
        (subseq (entry-ids-of pagination)
                start
                (min (+ start (num-entries-pr-page-of pagination))
                     (total-num-entries-of pagination))))))


(defmethod navigate-to-next-page ((pagination pagination))
  (incf (current-page-of pagination)))
(export 'navigate-to-next-page)


(defmethod navigate-to-prev-page ((pagination pagination))
  (decf (current-page-of pagination)))
(export 'navigate-to-prev-page)


(define-condition pagination-out-of-bounds ()
  ())
(export 'pagination-out-of-bounds)


(defmethod (setf current-page-of) ((page-num integer) (pagination pagination) &optional force-update-p)
  "Might throw an error PAGINATION-OUT-OF-BOUNDS; no change is then made and no
callback is triggered."
  (if (or force-update-p
          (and (not (minusp page-num))
               (> (total-num-pages-of pagination)
                  page-num)))
      (let ((old-pages-before-p (more-pages-before-current-p-of pagination))
            (old-pages-after-p  (more-pages-after-current-p-of pagination)))
        (setf (slot-value pagination 'current-page) page-num)
        (when (or force-update-p (not (eq old-pages-before-p (more-pages-before-current-p-of pagination))))
          (funcall (on-state-change-of-pages-before-current-p-of pagination)
                   (more-pages-before-current-p-of pagination)))
        (when (or force-update-p (not (eq old-pages-after-p (more-pages-after-current-p-of pagination))))
          (funcall (on-state-change-of-pages-after-current-p-of pagination)
                   (more-pages-after-current-p-of pagination)))
        (render pagination)
        (handle-address-bar pagination)
        (funcall (on-page-change-fn-of pagination) page-num))
      (error 'pagination-out-of-bounds)))
(export 'current-page-of)



(defmethod force-update ((pagination pagination))
  (setf (current-page-of pagination t)
        (current-page-of pagination)))
(export 'force-update)


(defmethod uri-value<-state ((pagination pagination))
  (princ-to-string (current-page-of pagination)))


(defmethod state<-uri-value ((uri-value string) (pagination pagination) from-browser-history-p)
  (let ((new-page-num (parse-integer uri-value)))
    ;; TODO: This seems a bit weird ..
    (if from-browser-history-p
        (setf (current-page-of pagination)
              new-page-num)
        (setf (slot-value pagination 'current-page)
              new-page-num))))
