; output.lisp --- Useful functions for output !
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


(defun %format-el (n padding)
  "Takes a string or a cons cell containing an quoted symbol, and returns a string
   with padding blank spaces before n"
  (let ((name (if (typep n 'cons) (write-to-string (cadr n) ) n)))
    (setf padding (- padding (length name)))
    (concatenate 'string (make-string padding :initial-element #\space) name)))

(defun dashes (n c)
  "Print a line of character c of length n. If c is nil, just prints a newline"
  (format t "~%")
  (when c
    (progn (format t "~a" (make-string n :initial-element c))
	   (format t "~%"))))

(defun print-table (output arg-list time-info-list &key (padding t) (head t) (linesep #\-) (overhead t))
  "Print a table on output, defined by arg-list, of the objects in time-info-list
   :padding makes a padding for each call
   :head shows the names of the slots used
   :linesep is the separator of the table lines"
  (let* ((max-length (make-array (length arg-list)
				 :initial-contents (mapcar #'(lambda (x)
							       (length (write-to-string 
									(if (typep x 'cons)
									    (cadr x)
									  x) :escape nil)))
							   arg-list)))
	 (line-length 0)
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
    (setf line-length (reduce #'+ max-length))
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
      (setf acc 0))
    (when overhead (print-overhead output time-info-list))))

(defun print-overhead (output time-info-list)
  "Prints the overhead after profiling, writes on the output stream"
  (let ((total-time 0))
    (loop for time-info in time-info-list do
      (incf total-time (slot-value time-info 'calls)))
    (format output "Total estimated overhead time : ~A~%" 
	    (* total-time (sb-profile::overhead-total sb-profile::*overhead*)))
    (format output "Overhead estimation, Internal :  ~S, Calls : ~S/s, Total : ~A~%"
	    (sb-profile::overhead-internal sb-profile::*overhead*)
	    (sb-profile::overhead-call sb-profile::*overhead*)
	    (sb-profile::overhead-total sb-profile::*overhead*))))


;; Percent time-info class, mostly an example and a test. It inherits from the time-info class 
;; and adds a new slots, percent, which contains the percentage of the execution time, compared
;; to the total execution time

(defclass percent-time-info (time-info)
  ((percent :initarg :percent)))

(defun make-percent-time-info (name calls second consing gc-run-time percent)
			(make-instance 'percent-time-info 
				       :name name
				       :calls calls
				       :second second
				       :consing consing
				       :gc-run-time gc-run-time
				       :percent percent))
(defmethod to-percent-time-info ((time-info time-info) &optional (percent-val 0))
  (with-slots (name calls second consing gc-run-time) time-info
    (make-percent-time-info name calls second consing gc-run-time percent-val)))

(defun get-percent-info-list (time-info-list)
  (let ((acc 0))
    (loop for time-info in time-info-list do
      (incf acc (slot-value time-info 'second)))
    (loop for time-info in time-info-list collect
      (to-percent-time-info time-info (if (zerop acc) 0
					 (/ (slot-value time-info 'second) acc)))))))
      
