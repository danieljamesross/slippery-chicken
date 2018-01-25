;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File:    swap-marks.lsp
;;;
;;; Author:  Daniel James Ross (mr.danielross@gmail.com)
;;;
;;; Created: Tue Sep 22 14:26:30 BST 2015
;;;
;;; Date:    Thu 25 Jan 2018 09:30:40 GMT
;;;
;;; Purpose: Replace one dynamic mark with another
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DJR - Mon Nov 23 12:54:01 GMT 2015
;; Have turned this lambda function into a method

(defmethod swap-marks ((sc slippery-chicken) start-bar end-bar
		       players		; DJR - Mon Jun 20 14:21:02 BST 2016
		       ;; multiple player possibility
		       old-mark new-mark)
  (unless end-bar (setf end-bar (num-bars sc)))
  (unless start-bar (setf start-bar 1))
  (unless players (setf players (players sc)))
  (force-list players)
  (loop for player in players do
       (loop for bn from start-bar to end-bar
	  for bar = (get-bar sc bn player)
	  do
	    (loop for e in (rhythms bar) do
		 (when (has-mark e old-mark)
		   (rm-marks e old-mark)
		   (add-mark-once e new-mark))))))
       


;; Example using an adapted and simplified version of Michael Edwards'
;; template.lsp to show swap-marks at work.

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
 :rthm-seq-palette
 '((seq1 ((((4 4) (q) (q) q q))   
          :pitch-seq-palette (1 2)
	  :marks (p 1)))  
   (seq2 ((((4 4) (e) e q h)) 
          :pitch-seq-palette (1 2 3)
	  :marks (pp 1))))
 :rthm-seq-map
 '((1 ((flt (seq1 seq1 seq2 seq1 seq1 seq2))))))

(swap-marks +your-title-here+ 1 2 'flt 'p 'fff)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Media content generation:

;;; cmn score
;;; #+ notation means only run the next Lisp form if e.g. the CMN package is
;;; available  
#+cmn (cmn-display +your-title-here+ :file "/tmp/your-title-here.eps")

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF swap-marks.lsp

