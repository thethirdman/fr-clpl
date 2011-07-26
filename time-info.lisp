; time-info.lisp --- Class for profiling results
; ----------------------------------------------------------------------------
; "THE BEER-WARE LICENSE" (Revision 42):
; <francois.ripault@epita.fr> wrote this file. As long as you retain this notice you
; can do whatever you want with this stuff. If we meet some day, and you think
; this stuff is worth it, you can buy me a beer in return.
; Francois Ripault
; ----------------------------------------------------------------------------
;

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

(defgeneric print-info-list (out time-list)
  (:documentation "Generic method for printing an element, using an output object and a time-info struct"))

(defun get-info-list ()
  "Get the results of the profiling, and retrun a time-info-list"
  (loop for key being the hash-keys of sb-profile::*profiled-fun-name->info* collect
    (let ((pinfo (gethash key sb-profile::*profiled-fun-name->info*)))
      (multiple-value-bind (calls ticks consing profile gc-run-time)
	  (funcall (sb-profile::profile-info-read-stats-fun pinfo))
	(when (not (zerop calls))
	  (make-time-info key calls (sb-profile::compensate-time calls ticks profile) consing gc-run-time))))))

(defmethod resolve-slots (arg-list (time-info time-info))
  "Replace slots by the corresponding values"
  (loop for arg in arg-list collect
    (if (typep arg 'cons)
	(write-to-string (slot-value time-info (cadr arg)) :escape nil)
      arg)))

(defun %format-el (n padding)
  (let ((name (if (typep n 'cons) (write-to-string (cadr n) ) n)))
    (setf padding (- padding (length name)))
    (concatenate 'string (make-string padding :initial-element #\space) name)))

(defun dashes (n c)
  (format t "~%")
  (when c
    (progn (format t "~a" (make-string n :initial-element c))
	   (format t "~%"))))

(defun print-table (output arg-list time-info-list &key (padding t) (head t) (linesep #\-))
  "Print a table on output, defined by arg-list, of the objects in time-info-list
   :padding makes a padding for each call
   :head shows the names of the slots used
   :linesep is the separator of the table lines"
  (let* ((max-length
	  (make-array (length arg-list)
		      :initial-contents (mapcar #'(lambda (x)
						    (length (write-to-string 
							     (if (typep x 'symbol)
								 (cadr x)
							       x) :escape nil)))
							   arg-list)))
	 (line-length (reduce #'+ max-length))
	 (acc 0)
	 (arg-solved ()))
    ;; Let's get the max lengthes for each cell, used for the padding
    (when padding
      (loop for time-info in time-info-list do
	(setf arg-solved (resolve-slots arg-list time-info))
	(loop for arg in arg-solved do
	  (setf (aref max-length acc) (max (length arg) (aref max-length acc)))
	  (incf acc))
	(setf acc 0)))
    ;; Print the head of the table
    (when head
      (progn 
	(dashes line-length linesep)
	(loop for arg in arg-list do
	  (if padding
	      (progn
		(format output "~a" (%format-el arg (aref max-length acc)))
		(incf acc))
	    (format output "~a" arg)))
	  (dashes line-length linesep)
	  (setf acc 0)))
    ;; Print the values of the table
    (loop for time-info in time-info-list do
      (setf arg-solved (resolve-slots arg-list time-info))
      (loop for arg in arg-solved do
	(if padding
	    (progn
	      (format output "~a" (%format-el arg (aref max-length acc)))
	      (incf acc))
	  (format output "~a" arg)))
      (dashes line-length linesep)
      (setf acc 0))))

	