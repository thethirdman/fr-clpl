; output.lisp --- Output part of the package
; ----------------------------------------------------------------------------
; "THE BEER-WARE LICENSE" (Revision 42):
; <francois.ripault@epita.fr> wrote this file. As long as you retain this notice you
; can do whatever you want with this stuff. If we meet some day, and you think
; this stuff is worth it, you can buy me a beer in return.
; Francois Ripault
; ----------------------------------------------------------------------------
;

(in-package :fr-clpl)


;(defclass output ()
;  stream)





									    



;(defmethod print-percent-info-list ((st stream) (time-info-list list))
;  (let ((acc 0))
;    (progn
;      (loop for l in time-info-list do
;	(setf acc (+ acc (time-info-second l))))
;      (loop for l in time-info-list do
;	(format st "~A : ~A % ~%" (time-info-name l) (round (* (/ (time-info-second l) acc) 100)))))))
