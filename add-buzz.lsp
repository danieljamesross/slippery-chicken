;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File:    add-buzz.lsp
;;;
;;; Author:  Daniel James Ross (mr.danielross@gmail.com)
;;;
;;; Date:    Thu Apr 21 18:31:19 BST 2016
;;;
;;; Purpose: Add a buzz roll mark to a
;;;          note in lilypond
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod add-buzz ((sc slippery-chicken) bar-num note player
		     &key
		       (h-offset .4) ; horizontal positioning
		       (v-offset -3.5))  ; vertical positioning
  (let ((lp-command (format nil "\\once \\override 
TextScript #'font-size = #2 \\once \\override TextScript 
#'font-family = #'typewriter \\once \\override TextScript 
#'extra-offset = #'(~a . ~a)" h-offset v-offset)))
    (add-mark-before-note sc bar-num note player
     `(text ,lp-command))
    (add-mark-to-note sc bar-num note player "z")))

#|

(let* ((mini
	(make-slippery-chicken
	 '+mini+
	 :ensemble '(((vn (violin :midi-channel 1))))
	 :set-palette '((1 ((c4 e4 g4))))
	 :set-map '((1 (1)))
	 :rthm-seq-palette '((1 ((((2 4) q e e))
				 :pitch-seq-palette ((1 2 3)))))
	 :rthm-seq-map '((1 ((vn (1))))))))
  (add-buzz mini 1 1 'vn)
  (lp-display mini))

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF add-buzz.lsp
