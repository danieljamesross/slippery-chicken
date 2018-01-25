;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File:    make-scale.lsp
;;;
;;; Author:  Daniel James Ross (mr.danielross@gmail.com)
;;;
;;; Date:    23 May 2016
;;;
;;; Update:  Fri Sep 23 10:54:44 BST 2016
;;;
;;; Purpose: Create a list of pitches contained in a specific scale,
;;;          e.g. major, natural minor, harmonic minor, ionian, etc.
;;;          Custom scales also possible.
;;;
;;; Output:  A list of pitch symbols e.g. '(c4 d4 e4 f4 g4 a4 b4 c5)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-scale (scale-type		; see possibilities below
		   root			; lowest note of scale
		   &key
		     (num-octaves 2)
		     ;; list of semitones or tones
		     ;; e.g. '(1 2 1 1 1)
		     custom-scale 
		     enharm ; sharps or flats
		     replacements ; substitute pitches '((fs1 f1)(g3 af3))
		     remove) ; delete
  ;; Generate a scale
  (unless (typep scale-type 'atom)
    (error "~a is not a valid scale type" scale-type))
  (let ((scale-pattern
	 ;; scale types
	 (cond ((equalp scale-type 'major) 
		'(2 2 1 2 2 2 1))
	       ((equalp scale-type 'nat-minor)
		'(2 1 2 2 1 2 2))
	       ((equalp scale-type 'harm-minor)
		'(2 1 2 2 2 1 2))
	       ((equalp scale-type 'mel-minor-up)
		'(2 1 2 2 2 2 1))
	       ((equalp scale-type 'mel-minor-down)
		'(2 1 2 2 2 1 2))
	       ((equalp scale-type 'ionian)
		'(2 2 1 2 2 2 1))
	       ((equalp scale-type 'dorian)
		'(2 1 2 2 2 1 2))
	       ((equalp scale-type 'mixolydian)
		'(2 2 1 2 2 1 2))
	       ((equalp scale-type 'phrygian)
		'(1 2 2 2 1 2 2))
	       ((equalp scale-type 'lydian)
		'(2 2 2 1 2 2 1))
	       ((equalp scale-type 'aeolian)
		'(2 1 2 2 1 2 2))
	       ((equalp scale-type 'locrian)
		'(1 2 2 1 2 2 2))
	       ((equalp scale-type 'ahava-raba)
		'(1 2 1 2 1 2 2))
	       ((equalp scale-type 'whole-tone)
		'(2 2 2 2 2))
	       ((equalp scale-type 'pentatonic)
		'(2 3 2 2))
	       ((equalp scale-type 'chromatic)
		'(1 1 1 1 1 1 1 1 1 1))
	       ((equalp scale-type 'custom)
		custom-scale)))
	(new-scale))
    (unless enharm
      (if (or (is-flat root)
	      (octave-freqs (note-to-freq 'f4)
			    (note-to-freq root)))
	  (setf enharm 'flats)
	  (setf enharm 'sharps)))
    (unless scale-pattern
      (error "~a is not recognised.~%Misspelling or no custom data?"
	     scale-type))
    (setf new-scale 
	  (flatten
	   (loop repeat num-octaves collect
		(list root
		      (loop for interval in scale-pattern
			 with note = root
			 do (setf note
				  (midi-to-note
				   (+ (note-to-midi note)
				      interval)))
			 collect (if (or (and (equalp enharm 'sharps)
					      (is-flat note))
					 (and (equalp enharm 'flats)
					      (is-sharp note)))
				     (data (enharmonic
					    (make-pitch note)))
				     note)))
	      do (setf root (midi-to-note (+ 12 (note-to-midi root)))))))
    (setf new-scale (remove-duplicates new-scale :test #'equalp))
    (when replacements
      (setf new-scale (flatten (loop for reps in replacements collect
			   (loop for i in new-scale collect
				(if (equalp i (first reps))
				    (second reps) i))))))
    (when remove
      (unless (listp remove)
	(setf remove (list remove)))
      (loop for i in remove do
	   (setf new-scale (remove i new-scale))))
    new-scale))

;; (make-scale 'major 'c3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF make-scale.lsp
