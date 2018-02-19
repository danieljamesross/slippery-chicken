;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File:    make-chord-auto.lsp
;;;
;;; Author:  Daniel James Ross (mr.danielross@gmail.com)
;;;
;;; Date:    25 January 2018
;;;
;;; Update:  Thu 25 Jan 2018 08:48:59 GMT
;;;
;;; Purpose: Automatically turn a single pitch into a chord bearing
;;;          in mind the instruments set-limits and playing range.
;;;          You can even specify the chord function
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :sc)

(defmethod make-chord-auto ((sc slippery-chicken)
			    bar-num 
			    event ; events are 1-based
			    instrument ; single player
			    &key (chord-fun #'piano-chord-fun) print)
  (let* ((ev (get-event sc bar-num event instrument))
	 (bar (get-bar sc bar-num instrument))
	 (sr (set-ref ev))
	 (dyn (get-dynamic ev))
	 (mrks (marks ev))
	 (mrks-b4 (marks-before ev))
	 (tt)
	 (tf))
    (if (is-chord ev)
	(format t "~%WARNING! make-chord-auto:~%event is a chord already: bar ~a, event ~a"
		bar-num event)
	(if (or (is-rest ev)
		(is-whole-bar-rest ev)
		(is-rest-bar bar))
	    (format t "~%WARNING! make-chord-auto:~%event is a rest: bar ~a, event ~a"
		    bar-num event)
	    (let* ((sll (get-set-limit-low sc instrument (if (zerop (nth-seq bar))
							     1 (nth-seq bar))))
		   (slh (get-set-limit-high sc instrument (if (zerop (nth-seq bar))
							      1 (nth-seq bar))))
		   (refz (get-data-data sr (set-palette sc)))
		   (pcurve (flatten (pitch-curve (get-sequenz-from-bar-num
						  (piece sc) bar-num instrument))))
		   (new-pitches))
	      ;; Account for set-limits low...
	      (unless sll (setq sll (lowest-sounding
				     (player-get-instrument
				      (get-player sc instrument)))))
	      ;; ...and high
	      (unless slh (setq slh (highest-sounding
				     (player-get-instrument
				      (get-player sc instrument)))))
	      (setq refz (loop for p in refz
			    when (and (pitch>= p sll)
				      (pitch<= p slh))
			    collect p)
		    new-pitches (funcall chord-fun 1
					 (if pcurve 
					     (nth (bar-pos ev) pcurve) 1)
					 refz 1 instrument 1))
	      ;; Account for ties later
	      (when (is-tied-to ev) (setf tt t))
	      (when (is-tied-from ev) (setf tf t))
	      
	      (when new-pitches
		(setf (pitch-or-chord ev) new-pitches)
		(when print
		  (format t "~%make-chord-auto:~%ins: ~a~%bar: ~a~%ev: ~a~%new-pitches: ~a"
			  instrument (bar-num bar) event (get-pitch-symbols new-pitches))))
	      ;; Sort out marks etc.
	      (when dyn (setf (amplitude ev) (dynamic-to-amplitude dyn)))
	      (when mrks (setf (marks ev) mrks))
	      (when mrks-b4 (setf (marks-before ev) mrks-b4))
	      ;; Sort ties
	      (when tt (setf (is-tied-to ev) t))
	      (when tf (setf (is-tied-from ev) t))
	      ;; Avoid errors with this
	      (update-slots sc)))))
  t)

#|

;;; Example

(let* ((mini
	(make-slippery-chicken
	 '+mini+
	 :ensemble '(((vn (violin :midi-channel 1))))
	 :set-palette '((1 ((c4 e4 g4))))
	 :set-map '((1 (1)))
	 :rthm-seq-palette '((1 ((((2 4) q e e))
				 :pitch-seq-palette ((1 2 3)))))
	 :rthm-seq-map '((1 ((vn (1))))))))
  (make-chord-auto mini 1 1 'vn :chord-fun #'violin-chord-selection-fun :print t)
  (cmn-display mini))

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF make-chord-auto.lsp
