;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File:    custom-text-spanner-lp.lsp
;;;
;;; Author:  Daniel James Ross (mr.danielross@gmail.com)
;;;
;;; Date:    4th March 2018
;;;
;;; Update:  Mon  5 Mar 2018 09:59:18 GMT
;;;
;;; Purpose: Add a custom text spanner above the staff.
;;;          You can specify the custom text and the line-style.
;;;          e.g. rit.................
;;;          e.g. accel.--------------
;;;          e.g. trill~~~~~~~~~~~~~~~
;;;          Requires add-mark-above-staff-lp.lsp
;;;          https://github.com/danieljamesross/slippery-chicken/blob/master/add-mark-above-staff-lp.lsp
;;;          NB: Lilypond only
;;;          NB: if you supply more than one word for custom-text
;;;          only the first will be bold.
;;;          NB: only one line at a time at the moment
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :sc)

(defmethod custom-text-spanner-lp ((sc slippery-chicken) start-bar-num start-event
				end-bar-num end-event player custom-text
				&key
				  ;; line, zigzag, dotted-line,
				  ;; dashed-line, trill
				  (line-style 'dotted-line))
  (custom-spanner-start sc start-bar-num start-event player
			custom-text line-style)
  (custom-spanner-stop sc end-bar-num end-event player)
  custom-text)

(defmethod custom-spanner-start ((sc slippery-chicken) start-bar-num start-event
				 player custom-text &optional line-style)
  (let ((bmt (format nil "\\once \\dynamicUp \\override TextSpanner.bound-details.left.text = \\markup { \\small \\italic \\bold ~a } \\override TextSpanner.style = #'~a"
		     custom-text (string-downcase line-style))))
    ;(print bmt)
    (add-mark-above-staff-lp sc start-bar-num start-event player
			 '(text "\\startTextSpan")
			 :before-mark-event (list 'text bmt))))

(defmethod custom-spanner-stop ((sc slippery-chicken) end-bar-num end-event player)
  (add-mark-above-staff-lp sc end-bar-num end-event player
			   '(text "\\stopTextSpan")))

;;; Example

#|
(progn


  (make-slippery-chicken
   '+mini+
   :ensemble '(((vn (violin :midi-channel 1))))
   :set-palette '((1 ((c4 e4 g4))))
   :set-map '((1 (1 1 1 1)))
   :rthm-seq-palette '((1 ((((2 4) q e e))
			   :pitch-seq-palette ((1 2 3)))))
   :rthm-seq-map '((1 ((vn (1 1 1 1))))))
  
  (custom-text-spanner-lp +mini+  1 1 4 2 'vn "Text" :line-style 'zigzag)
  (add-mark-to-event +mini+ 3 2 'vn 'ff)
  (lp-display +mini+ :base-path "/tmp/"))

|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; eof custom-text-spanner-lp.lsp
