;;; fr-clpl.asd --- ASDF system definition
;; TODO : put beerware license

;;; Code:

(in-package :cl-user)

(defpackage :fr-clpl
    (:use :cl)
    (:export
    :*exec-table*
    :*log-file*
    :define-bench
    :run-bench))

(asdf:defsystem
 :fr-clpl
 :description "Common Lisp Profiling Library"
 :author "Francois Ripault"
 :license "Beerware"
 :components
  ((:file "bench")))
 
