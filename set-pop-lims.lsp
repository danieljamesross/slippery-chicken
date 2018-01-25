;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File:    set-pop-lims.lsp
;;;
;;; Author:  Daniel James Ross (mr.danielross@gmail.com)
;;;
;;; Created: 8 November 2015
;;;
;;; Date:    Mon Nov 23 11:13:31 GMT 2015
;;;
;;; Purpose: Use make-popcorn to create arguments for
;;;          :set-limits-high or :set-limits-low
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The function takes two arguments, the highest and lowest
;; pitches for the set limits - in note-name symbols or midi notes.
;; Optional arguments for min and max are 'kernal' values for
;; the call to (make-popcorn).
;; See (make-popcorn) for more details
;; http://www.michael-edwards.org/sc/robodoc/popcorn_lsp.html#robo297

(defun set-pop-lims (high low &optional (max 0.2) (min 0.01))
      (unless (numberp high)
	(setf high (note-to-midi high)))
      (unless (numberp low)
	(setf low (note-to-midi low)))
      (let* ((pop (scale (make-popcorn (list min max)) high low)))
	(flatten
	 (loop for k from 1 to (length pop)
	    for i in pop
	for j from 1 to 100 by (/ 100 (length pop))
	      when (eql k (length pop))
	      do (setq j 100)
	collect
	  (list (round j)(midi-to-note (round i)))))))

;; Example using an adapted and simplified version of Michael Edwards'
;; template.lsp to show (set-pop-lims) at work.

#|

(in-package :sc)
(in-scale :chromatic)

(make-slippery-chicken  
 '+your-title-here+ 
 :title "Your Title Here" 
 :composer "Your Name Here"
 :ensemble '(((flt (flute :midi-channel 1))))
 :staff-groupings '(1)
 :tempo-map '((1 (q 60)))
 :set-palette '((set1 ((fs2 b2 d4 a4 d5 e5 a5 d6))) 
                (set2 ((b2 fs3 d4 e4 a4 d5 e5 a5 d6))))
 :set-map '((1 (set1 set1 set2 set1 set1 set2)))
 :set-limits-high `((flt ,(set-pop-lims 'f5 'f7)))
 :rthm-seq-palette
 '((seq1 ((((4 4) (q) (q) q q))   
          :pitch-seq-palette (1 2)))  
   (seq2 ((((4 4) (e) e q h)) 
          :pitch-seq-palette (1 2 3))))
 :rthm-seq-map '((1 ((flt (seq1 seq1 seq2 seq1 seq1 seq2))))))

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF set-pop-lims.lsp
