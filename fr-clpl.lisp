; bench.lisp --- Main lisp file, contains profiling scheduler
; ----------------------------------------------------------------------------
; "THE BEER-WARE LICENSE" (Revision 42):
; <francois.ripault@epita.fr> wrote this file. As long as you retain this notice you
; can do whatever you want with this stuff. If we meet some day, and you think
; this stuff is worth it, you can buy me a beer in return.
; Francois Ripault
; ----------------------------------------------------------------------------
;

(in-package :fr-clpl)

(defparameter *exec-table* (make-hash-table))


(defstruct (bench (:constructor make-bench (name function number args)))
  "Benchmark structure containing the number of times the function is called, the name of the function and an args list"
  name
  function
  number
  args)

(defun define-bench (name function number args)
  "Generate tests for function func, each one using an element of arg-list,
  being executed number times"
  (let ((bar (make-bench name function number args)))
    (setf (gethash name *exec-table*) bar)))

(defun update-profiling ()
  (unless (boundp 'sb-profile::*overhead*)
    (setf sb-profile::*overhead* (sb-profile::compute-overhead)))
  (loop for key being the hash-keys of sb-profile::*profiled-fun-name->info* collect
       (let ((pinfo (gethash key sb-profile::*profiled-fun-name->info*)))
	 (multiple-value-bind (calls consing profile gc-run-time)
	     (funcall (sb-profile::profile-info-read-stats-fun pinfo))
	   (when (zerop calls)
	     (eval `(sb-profile:unprofile ,key)))))))

	      
(defmacro %dump (function bench &key (sub-tracking nil))
  "Takes a name, the number of executions of the function func
  and its args under a list, and run a bench mark, which is
  reported in *log-file*"
  `(progn
     (with-slots ((arg-list args) number) ,bench
       (sb-profile:reset)
       ,(eval `(sb-profile:profile ,function))
       (loop for arg in arg-list do
	    (loop repeat number do
		 (handler-case
                     (apply ',function arg)
		   (error (e) (print e *trace-output*)))
		 (when ,sub-tracking 
		   (progn ,(setf sub-tracking nil)
			   (update-profiling))))))
     (get-time-info-list)))

(defmacro run-bench (&optional (sub-tracking nil) &rest bench-list)
  "Run all the benches stored in *exec-list*, or the one given in argument"
  (progn
    (let ((current-bench nil))
      (if bench-list
	  (loop for bench in bench-list
	     for current-bench = (gethash bench *exec-table*) do
	       (eval `(%dump ,(slot-value current-bench 'function) ,current-bench :sub-tracking ,sub-tracking)))
	  (loop for key being the hash-keys of *exec-table*
	     for current-bench = (gethash key *exec-table*) do
	       (eval `(%dump ,(slot-value current-bench 'function) ,current-bench :sub-tracking ,sub-tracking))))
      '(prog1
	(get-time-info-list)
	;(sb-profile:unprofile)
	(sb-profile:reset)))))
