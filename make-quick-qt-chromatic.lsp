;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File:     make-quick-qt-chromatic.lsp
;;;
;;; Author:   Daniel James Ross (mr.danielross@gmail.com)
;;;
;;; Created:  17 March 2015
;;;
;;; Date:     Mon Nov 23 12:41:36 GMT 2015
;;;
;;; Purpose:  Make all quarter-tone notes with rhythmic values of 'e
;;;           or quicker chromatic. Handy for fast passages when playing
;;;           qt notes is difficult.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod make-quick-qt-chromatic ((sc slippery-chicken) player
				    &optional
				    start-bar end-bar)
  (unless end-bar (setf end-bar (num-bars sc)))
  (unless start-bar (setf start-bar 1))
  (loop for bn from start-bar to end-bar
     for bar = (get-bar sc bn player)
     do
       (loop for e in (rhythms bar)
	  for i from 1
	  for event = (get-event sc bn i player)
	  do 
	    (when (and (not (is-tied-to e))
		       (not (is-tied-from e))
		       (needs-new-note e)
		       (<= (compound-duration e) 0.5)
		       (qtr-tone (lowest event)))
	      (change-pitch sc bn i player 
			    (nearest-chromatic (lowest event))))))
  t)

;; Example using an adapted and simplified version of Michael Edwards'
;; template.lsp to show (make-quick-qt-chromatic ...) at work.

#|
(in-package :sc)
(in-scale :quarter-tone)

(make-slippery-chicken  
 '+your-title-here+ 
 :title "Your Title Here" 
 :composer "Your Name Here"
 :ensemble '(((flt (flute :midi-channel 1 :microtones-midi-channel 2))))
 :staff-groupings '(1)
 :tempo-map '((1 (q 60)))
 :set-palette '((set1 ((fqs2 b2 dqs4 aqf4 dqs5 e5 a5 d6))))
 :set-map '((1 (set1 set1 set1 set1 set1 set1)))
 :rthm-seq-palette
 '((seq1 ((((4 4) q - s s s s -  - e e - q))   
          :pitch-seq-palette (1 2 3 4 5 6 7 8))))
 :rthm-seq-map
 '((1 ((flt (seq1 seq1 seq1 seq1 seq1 seq1))))))

(make-quick-qt-chromatic +your-title-here+ 'flt 1)
(cmn-display +your-title-here+)

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF make-quick-qt-chromatic.lsp
