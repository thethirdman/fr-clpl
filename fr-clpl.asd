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
 
