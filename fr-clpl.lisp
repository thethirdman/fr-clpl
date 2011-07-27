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


(defstruct (bench (:constructor make-bench (function number args)))
  "Benchmark structure containing the number of times the function is called, the name of the function and an args list"
  function
  number
  args)

(defmacro define-bench (name function number args)
  "Generate tests for function func, each one using an element of arg-list,
   being executed number times"
  (let ((bar (make-bench function number args)))
    (setf (gethash name *exec-table*) bar)
    (format t "exec-table : ~A~%" *exec-table*)))

(defmacro dump (function bench)
  "Takes a name, the number of executions of the function func
   and its args under a list, and run a bench mark, which is
   reported in *log-file*"
  (progn
    (with-slots ((arg-list args) number) bench
      `(sb-profile:reset)
      (eval `(sb-profile:profile ,function))
      `(loop for args in arg-list do
	 (loop repeat number do 
	   (handler-case
	       (apply ',function args)
	     (error (e) (print e *trace-output*)))))
      (eval `(sb-profile:unprofile ,function)))))

  
(defmacro run-bench (&rest bench-list)
  "Run all the benches stored in *exec-list*"
  (progn
    (let ((current-bench nil))
      (if bench-list
	  (loop for bench in bench-list do
	    (progn
	      (format t "~A" bench)
	      (setf current-bench (gethash bench *exec-table*))
	      `(dump ,(slot-value current-bench 'function) ,current-bench)))
	(loop for key being the hash-keys of *exec-table* do
	  (progn
	    (setf current-bench (gethash key *exec-table*))    
	    (format t "~A" key)
	    `(dump ,(slot-value current-bench 'function) ,current-bench)))))
    `(get-info-list)))


