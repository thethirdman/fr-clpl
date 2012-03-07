;; This is an example file which explains how to use fr-clpl

(require :asdf)
(asdf:load-system :fr-clpl)
(in-package :fr-clpl)

;; We define to different benchmarks, one analysing format performances
;; on simple string, the other one containing digits
;;
(define-bench 'string-format 'format 10
'((t "Hello World")
  (t "My second test-case")))

(define-bench 'number-format 'format 10
  '((t "42")
  (t "123456789")))

;; Here with define our format string for the output
;; the string represents separators, while the symbols are slots of a time-info
;; class, or an inheriting class
(defvar *format* '("|| " 'name "|" 'second " ||"))

;; Here is an example of what is achievable through print-table
;; It gives us a LaTeX output which is insertable in a tabular
(defvar *latex-format* '('name " & " 'second " \\\\"))


;; then, we dump our benches into an open-file
;; since run-bench returns a list of the results for each bench (which is
;; itself a list for each arg clause)
(with-open-file  (out "my-example.txt" :direction :output :if-exists :append)
  (loop for i in (run-bench) do
    (print-table out *latex-format* i :padding nil :linesep nil)))

;; We do not care about the padding in latex, so with disable it because we can
;; We also would like to avoid dashes in the output, since it is useless in a
;; LaTeX file
