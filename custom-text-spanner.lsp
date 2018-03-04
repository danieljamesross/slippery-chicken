;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File:    custom-text-spanner.lsp
;;;
;;; Author:  Daniel James Ross (mr.danielross@gmail.com)
;;;
;;; Date:    4th March 2018
;;;
;;; Update:  Sun 4 Mar 2018 15:16:42 GMT
;;;
;;; Purpose: Add a custom text spanner above the staff.
;;;          You can specify the custom text and the line-style
;;;          e.g. rit.................
;;;          e.g. accel.--------------
;;;          e.g. trill~~~~~~~~~~~~~~~
;;;          Requires add-mark-above-staff-lp.lsp
;;;          http://
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :sc)

(defmethod custom-text-spanner ((sc slippery-chicken) start-bar-num start-event
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
				 player custom-text
				 &optional (line-style 'line))
  (let ((bmt (format nil "\\once \\dynamicUp \\override TextSpanner.bound-details.left.text = \\markup { \\small \\italic \\bold ~a } \\override TextSpanner.style = #'~a"
		     custom-text line-style)))
    (print bmt)
    (add-mark-above-staff-lp sc start-bar-num start-event player
			 '(text "\\startTextSpan")
			 :before-mark-event (list 'text bmt))))

(defmethod custom-spanner-stop ((sc slippery-chicken) end-bar-num end-event player)
  (add-mark-above-staff-lp sc end-bar-num end-event player
			   '(text "\\stopTextSpan")))

;;; Example

#|
(make-slippery-chicken  
 '+your-title-here+ 
 :title "Your Title Here" 
 :composer "Your Name Here"
 ;; +slippery-chicken-standard-instrument-palette+ is the default instrument
 ;; palette. Only specify this slot if you need something special. See
 ;; instruments.lsp in the src directory (or http://tinyurl.com/pjbgnae) for
 ;; details of all pre-defined instruments. But here's an example of the kind
 ;; of thing to do if you do want to define your own.
 :instrument-palette
 '((piccolo (:transposition-semitones 12 :lowest-written d4
             :highest-written c6))
   ;; a more complete definition, by way of example
   (flute (:staff-name "flute" :staff-short-name "fl" 
           :lowest-written c4 :highest-written d7 
           :missing-notes (cqs4 dqf4) 
           :largest-fast-leap 19
           :starting-clef treble
           :chords nil 
           :microtones t 
           :midi-program 74))
   (b-flat-clarinet (:transposition-semitones -2 :lowest-written e3
                 :highest-written c6))  
   (violin (:lowest-written g3 :highest-written c7 :chords t)))
 :ensemble '(((flt ((flute piccolo) :midi-channel 1))
              (clr (b-flat-clarinet :midi-channel 2))
              (vln-one (violin :midi-channel 3))))
 :staff-groupings '(2 1)
 ;; this can contain bar references or bar numbers and looks something like
 ;; '((1 (q 160)) ((2 2 2) 96)) (200 (q 120 "meno mosso"))) where 1 is the bar,
 ;; q is the beat and 160 is the bpm. The (2 2 2) is a reference to a bar of
 ;; the form (section-number sequence-number bar-number). References are
 ;; converted to bar numbers before being stored in the map.
 :tempo-map '((1 (q 60)))
 :set-palette '((set1 ((fs2 b2 d4 a4 d5 e5 a5 d6))) 
                (set2 ((b2 fs3 d4 e4 a4 d5 e5 a5 d6))))
 :set-map '((1 (set1 set1 set2))
            (2 (set2 set2 set1 set2)))
 :set-limits-high '((vla (0 b4 100 b4)))
 ;; NB this should be called players-hierarchy but for historical reasons it's
 ;; not i.e. these are player names, not instrument names.
 :instruments-hierarchy '(vln-one flt clr)
 ;; this is a list of bars where rehearsal letters will be set or a list of
 ;; (bar-nmber letter) pairs if you want to set the letter (or other moniker)
 ;; by hand.
 :rehearsal-letters '(10)
 :avoid-melodic-octaves nil
 ;; section number then player, the paired lists of sequence number, instrument
 :instrument-change-map '((1 ((flt ((1 flute) (3 piccolo))))))
 :rthm-seq-palette
 '((seq1 ((((4 4) - 16 16 8 - { 5 - 20 10 20 20 - } { 3 3 6 } )   
           ( - s s s s - (s) - s s s - - +e. s - q))   
          :pitch-seq-palette (1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9)
          :marks (mp 1 a 1 s 3 slur 1 3 a 4 slur 4 7 s 9 a 10 slur 16 18)))  
   (seq2 ((((4 4) - e e - - s s s s - (s) s (s) s { 3 - te te te - } )  
           ( - s s s - (s)  { 3 - te te - (te) } q \+8 (e) )) 
          :pitch-seq-palette (11 10 9 11 10 8 7 11 10 6 5 11 10 9 8 7 11) 
          :marks (mf 1 s 1 2 a 3 slur 3 4 a 5 slur 5 6 s 7 s 8 a 9 
                     slur 9 11 a 12 slur 12 14 s 15 16 a 17))))
 :rthm-seq-map
 '((1 ((flt (seq1 seq1 seq2)) 
       (clr (seq1 seq2 seq1))))
   (2 ((flt (seq1 seq1 seq2 seq1))
       (clr (seq1 seq1 seq2 seq1))))))

(custom-text-spanner +your-title-here+  4 1 7 3 'flt "rit" :line-style 'dotted-line)

(lp-display +your-title-here+ :base-path "/tmp/")

|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; eof add-mark-above-staff-lp.lsp
