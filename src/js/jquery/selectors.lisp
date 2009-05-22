;;;; http://nostdal.org/ ;;;;

(in-package :sw-jquery)

#|
(defun selector-type-checker (selector)
  (when (listp selector)
    ;; It is actually supported in this "layer" of SW (see SELECTOR below),
    ;; but not in the upper layers yet. So we short circuit any attempt that
    ;; might come from higher level code to be safe.
    (error "TODO: SELECTOR is of type list, and this is not supported yet: ~A" selector)))


(declaim (inline selector))
(defun selector (elt-desc &key (url-p nil))
  (declare (ignore url-p))
  elt-desc)
|#

#|
  (selector-type-checker elt-desc)
  (setf elt-desc (mklst elt-desc))
  (flet ((string<-selector (elt-desc)
           (cond
             ((keywordp elt-desc)
              (let ((selector-str (string-downcase (string elt-desc))))
                (apply #'format nil 
                       (case (char selector-str 0)
                         (#\* (list "*"))
                         (#\. (list (if url-p ":~A" "~A") selector-str))
                         (#\# (if url-p
                                  (list ":~A" selector-str)
                                  (list "~A" (subseq selector-str 1))))
                         (t   (list (if url-p ":~A" "#~A") selector-str))))))
             ((stringp elt-desc)
              (format nil (if url-p "~A" "~A") elt-desc))
             (t (error "SELECTOR: Invalid format ~A" elt-desc)))))
    (let ((res (with-output-to-string (s)
                 (dolist (elt-d (subseq elt-desc 0 (1- (length elt-desc))))
                   (princ (string<-selector elt-d) s)
                   (if url-p (princ " " s) (princ ", " s)))
                 (princ (string<-selector (last1 elt-desc)) s))))
      (if url-p
          (ht:url-encode res))
          res)))
|#
;;(export 'selector)

