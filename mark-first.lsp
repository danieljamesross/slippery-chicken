;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File:    mark-first.lsp
;;;
;;; Author:  Daniel James Ross (mr.danielross@gmail.com)
;;;
;;; Date:    5 January 2018
;;;
;;; Update:  Thu 25 Jan 2018 09:32:24 GMT
;;;
;;; Purpose: Add any marks to the first note
;;;          of any part in your sc piece.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :sc)

(defmethod mark-first ((sc slippery-chicken) marks &key (players (players sc)) (bar-num 1))
  (when (typep players 'atom) (setf players (list players)))
  (when (typep marks 'atom) (setf marks (list marks)))
  (loop for ins in players do
       (loop for m in marks do
	    (add-mark-to-event sc bar-num 1 ins m))) 
  marks)

;; (mark-first +mini+ '(a ff))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF mark-first.lsp
