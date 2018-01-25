;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File:    pop-env.lsp
;;;
;;; Author:  Daniel James Ross (mr.danielross@gmail.com)
;;;
;;; Date:    22 July 2015
;;;
;;; Update:  Thu 25 Jan 2018 09:15:26 GMT
;;;
;;; Purpose: Generate an evelope using make-popcorn
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sc)

(defun pop-env (&key (high 10.0) (low 1.0) (min 2.0) (max 4.0))
  "Popcorn envelope"
  (let* ((pop (scale 
	       (make-popcorn '(0.01 0.2)
			     :min-spike min
			     :max-spike max)
	       high low)))
    (flatten
     (loop for i in pop
	for j from 0 to 100 by (length pop)
	collect
	  (list (round j) (round i))))))

; (pop-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF mypop.lsp
