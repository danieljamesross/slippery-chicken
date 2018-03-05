;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File:     make-thresh-qt-chromatic.lsp
;;;
;;; Author:   Daniel James Ross (mr.danielross@gmail.com)
;;;
;;; Created:  5 March 2018
;;;
;;; Date:     Mon  5 Mar 2018 13:49:09 GMT
;;;
;;; PURPOSE:  Like make-quickqt-chromatic but this time set a
;;;           threshold value (in seconds).Make a All quarter-tone notes
;;;           with rhythmic durations less that the threshold value
;;;           will be made chromatic. Handy for fast passages when playing
;;;           qt notes is difficult.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sc)

(defmethod make-thresh-qt-chromatic ((sc slippery-chicken) player
				    &key
				      (start-bar 1)
				      (end-bar nil)
				      (threshold 0.5))
  (unless end-bar (setf end-bar (num-bars sc)))
  (next-event sc player t start-bar)
  (loop for ne = (next-event sc player t)
       while ne
     do
       
	    (when (and (not (is-tied-to ne))
		       (not (is-tied-from ne))
		       (needs-new-note ne)
		       (<= (duration-secs ne) threshold)
		       (qtr-tone (lowest ne)))
	      (change-pitch sc (bar-num ne) (1+ (bar-pos ne))
			    player (nearest-chromatic (lowest ne)))))
  t)

;; Example
#|
(progn

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

(make-thresh-qt-chromatic +your-title-here+ 'flt :threshold 4)
(cmn-display +your-title-here+))

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF make-thresh-qt-chromatic.lsp
