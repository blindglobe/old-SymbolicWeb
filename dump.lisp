(eval-when (:execute :load-toplevel :compile-toplevel)
  (require :symbolicweb)
  (require :symbolicweb-examples))

(sb-ext:gc :full t)
(sb-ext:save-lisp-and-die "symbolicweb.sbcl"
                          :executable t)
