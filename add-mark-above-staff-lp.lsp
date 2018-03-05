;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File:    add-mark-above-staff-lp.lsp
;;;
;;; Author:  Daniel James Ross (mr.danielross@gmail.com)
;;;
;;; Date:    3rd March 2018
;;;
;;; Update:  Mon  5 Mar 2018 09:59:29 GMT
;;;
;;; Purpose: Add a mark above the staff in lilypond (not CMN!).
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :sc)

(defmethod add-mark-above-staff-lp ((sc slippery-chicken) bar-num event player mark
				    &key (before-mark-event '(text "\\once \\dynamicUp "))
				      (make-italic t)
				      (make-bold t))
				      
  (let* ((ev (get-event sc bar-num event player)))
  (add-mark-before ev before-mark-event)
  (add-mark ev mark)
  ;; The next-event, and its loop, enures any subsequent
  ;; events are under the stave as normal.
  (next-event sc player nil bar-num)
  (loop for ne = (next-event sc player)
     with next = 0
     while ne
     do (print-simple ne)
       (when (equalp ne ev)
	 (incf next))
     until
       (when (> next 0)
	 (print next)
	 (unless (has-mark-before ne '(text "\\once \\dynamicDown "))
	   (add-mark-before ne '(text "\\once \\dynamicDown ")))))
  
  (marks ev)))

#|

(progn
  (make-slippery-chicken
   '+mini+
   :ensemble '(((vn (violin :midi-channel 1))))
   :set-palette '((1 ((c4 e4 g4))))
   :set-map '((1 (1 1 1 1)))
   :rthm-seq-palette '((1 ((((2 4) e (e) e e))
			   :pitch-seq-palette ((1 2 3)))))
   :rthm-seq-map '((1 ((vn (1 1 1 1))))))
  
(add-mark-to-event +mini+ 1 3 'vn "strings always above staff")
(add-mark-above-staff-lp +mini+ 3 1 'vn 'ff)
(add-mark-to-event +mini+ 3 2 'vn 'ff)
(add-mark-above-staff-lp +mini+ 4 1 'vn 'pp)

(lp-display +mini+ :base-path "/tmp/"))


|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; eof add-mark-above-staff-lp.lsp
