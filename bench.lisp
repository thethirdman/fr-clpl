; ----------------------------------------------------------------------------
; "THE BEER-WARE LICENSE" (Revision 42):
; <francois.ripault@epita.fr> wrote this file. As long as you retain this notice you
; can do whatever you want with this stuff. If we meet some day, and you think
; this stuff is worth it, you can buy me a beer in return Francois Ripault
; ----------------------------------------------------------------------------
;

(in-package :fr-clpl)

(defparameter *exec-table* (make-hash-table))
(defparameter *log-file* "report.log")

(defmacro dump (name number func arg-list)
  "Takes a name, the number of executions of the function func
   and its args under a list, and run a bench mark, which is
   reported in *log-file*"
  `(progn
    (sb-profile:reset)
    (sb-profile:profile ,func)
    (with-open-file (*trace-output* ,*log-file* :direction :output :if-exists :append :if-does-not-exist :create)
      (loop for args in ,arg-list do
	(print ,name *trace-output*)
	(loop repeat ,number do 
	  (handler-case
	      (time (apply #',func args))
	    (error (e) (print e *trace-output*))))
	(sb-profile:report)
	(sb-profile:reset))
      (sb-profile:unprofile ,func))))
  
(defmacro define-bench (name number func arg-list)
  "Generate tests for function func, each one using an element of arg-list,
   being executed number times"
  (setf (gethash name *exec-table*) `( ,number ,func ,arg-list))
  (format t "exec-table : ~A~%" *exec-table*))


(defmacro run-bench (&rest bench-list)
  "Run all the benches stored in *exec-list*"
  (cons 'progn
	(if bench-list
	    (loop for bench in bench-list collect
	      `(dump ',bench ,@(gethash bench *exec-table*)))

	  (loop for key being the hash-keys of *exec-table* collect
	    `(dump ',key ,@(gethash key *exec-table*))))))

