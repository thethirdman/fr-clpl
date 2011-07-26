;;; fr-clpl.asd --- ASDF system definition
; ----------------------------------------------------------------------------
; "THE BEER-WARE LICENSE" (Revision 42):
; <francois.ripault@epita.fr> wrote this file. As long as you retain this notice you
; can do whatever you want with this stuff. If we meet some day, and you think
; this stuff is worth it, you can buy me a beer in return.
; Francois Ripault
; ----------------------------------------------------------------------------
;


;;; Code:

(in-package :cl-user)

(defpackage :fr-clpl
    (:use :cl)
    (:export
    ;; fr-clpl.lisp
    :*exec-table*
    :*log-file*
    :define-bench
    :run-bench
    ;; output.lisp
    time-info
    print-info-list
    get-info-list))


(asdf:defsystem
 :fr-clpl
 :description "Common Lisp Profiling Library"
 :author "Francois Ripault"
 :license "Beerware"
 :components
  ((:file "fr-clpl")
  (:file "output")
  (:file "time-info")))
 
