;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File:     add-slap.lsp
;;;
;;; Author:   Daniel James Ross (mr.danielross@gmail.com)
;;;
;;; Created:  6 March 2018
;;;
;;; Date:     Tue  6 Mar 2018 12:41:57 GMT
;;;
;;; Purpose:  Change notehead to slap notation (i.e. <)
;;;           NB: Lilypond only!
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sc)

(defmethod add-slap ((sc slippery-chicken) bar-num note player)
  (let ((slap '(text "\\once \\override NoteHead.stencil =
                     #(lambda (grob)
		       (grob-interpret-markup grob
			(markup #:musicglyph \"scripts.sforzato\")))
                       \\temporary \\override NoteHead.stem-attachment =
                       #(lambda (grob)
			 (let* ((thickness (ly:staff-symbol-line-thickness 
					    grob))
				(stem (ly:grob-object grob 'stem))
				(dir (ly:grob-property stem 'direction UP)))
			   (cons 1 (+ (if (= dir DOWN)
					  0.5
					  0)
				      (/ thickness 2))))) ")))
    (add-mark-before-note sc bar-num note player slap))
  (format t "~%add-slap: bar: ~a, note: ~a, player: ~a"
	  bar-num note player))

;; Example
#|
(progn

  (make-slippery-chicken  
   '+mini+ 
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

  (add-slap +mini+ 1 1 'flt)
  (lp-display +mini+))

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF add-slap.lsp
