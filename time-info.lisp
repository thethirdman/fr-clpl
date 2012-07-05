; time-info.lisp --- Class for profiled results
; ----------------------------------------------------------------------------
; "THE BEER-WARE LICENSE" (Revision 42):
; <francois.ripault@epita.fr> wrote this file. As long as you retain this notice you
; can do whatever you want with this stuff. If we meet some day, and you think
; this stuff is worth it, you can buy me a beer in return.
; Francois Ripault
; ----------------------------------------------------------------------------
; Some parts of code in this file are inspired from the Steel bank common lisp compiler
; Particularly from the package sb-profile
; Therefore, if you meet one of the authors some day, don't hesitate to buy them a beer


(in-package :fr-clpl)

(defclass time-info ()
  ((name :initarg :name)
   (calls :initarg :calls)
   (second :initarg :second)
   (consing :initarg :consing)
   (gc-run-time :initarg :gc-run-time))
  (:documentation "Class containing the results of a profiling"))

(defun make-time-info (name calls second consing gc-run-time)
  (make-instance 'time-info :name name :calls calls :second second :consing consing :gc-run-time gc-run-time))

(defun get-time-info-list ()
  "Get the results of the profiling, and retrun a time-info-list"
  (unless (boundp 'sb-profile::*overhead*)
    (setf sb-profile::*overhead* (sb-profile::compute-overhead)))
  (delete-if #'null
   (loop for key being the hash-keys of sb-profile::*profiled-fun-name->info* collect
        (let ((pinfo (gethash key sb-profile::*profiled-fun-name->info*)))
          (multiple-value-bind (calls ticks consing profile gc-run-time)
            (funcall (sb-profile::profile-info-read-stats-fun pinfo))
            (when (not (zerop calls))
              (make-time-info key calls (sb-profile::compensate-time calls ticks profile) consing gc-run-time)))))))

(defmethod resolve-slots (arg-list time-info)
  "Replace slots by the corresponding values"
  (loop for arg in arg-list collect
        (if (typep arg 'cons)
          (write-to-string (slot-value time-info (cadr arg)) :escape nil)
          arg)))
