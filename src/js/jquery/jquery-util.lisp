;;;; http://nostdal.org/ ;;;;

(in-package :sw-jquery)

(declaim #.(optimizations))


(let ((counter 0))
  (declare (integer counter))
  (defun js-genid (&optional str)
    (declare (optimize (speed 3) (debug 0) (safety 0) (space 0))
             (sb-ext:muffle-conditions sb-ext:compiler-note))
    (if str
        (catstr str "-" (princ-to-string (incf counter)))
        (princ-to-string (incf counter)))))
(export 'js-genid)


(declaim (inline js-focus))
(defun js-focus (selector)
  (declare (string selector))
  ;; TODO: Firefox has a bug that requires one to add a silly delay like this in some cases. I need to research this further.
  ;; https://bugzilla.mozilla.org/show_bug.cgi?id=53579
  ;; https://bugzilla.mozilla.org/show_bug.cgi?id=297134
  ;; TODO: Ok, seems Firefox-3.x has fixed this but, uh, IE6.x still requires this in some cases ...
  (catstr "setTimeout(function(){ $(\"#" selector "\").focus(); }, 100);"))
(export 'js-focus)


(declaim (inline js-scroll-to-bottom))
(defun js-scroll-to-bottom (selector)
  (declare (string selector))
  (catstr "$(\"#" selector "\").get(0).scrollTop = $(\"" selector "\").get(0).scrollHeight + 1000;"))
(export 'js-scroll-to-bottom)