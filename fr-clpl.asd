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
    :bench
    :define-bench
    :run-bench
    ;; time-info.lisp
    :time-info
    :make-time-info
    :get-time-info-list
    :resolve-slots
    ;; output.lisp
    :dashes
    :print-table
    :print-overhead
    ;; percent-class - output.lisp
    :percent-time-info
    :make-percent-time-info
    :to-percent-time-info
    :get-percent-info-list))

(asdf:defsystem
 :fr-clpl
 :description "Common Lisp Profiling Library"
 :author "Francois Ripault"
 :license "Beerware"
 :components
 ((:file "fr-clpl")
  (:file "output")
  (:file "time-info")))
 
