(in-package :slippery-chicken)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass sc-scale (sc-set)
  ;; I bet some things are missing from here...
  ((tonic :accessor tonic :initarg :tonic :initform nil)
   (scale-type :accessor scale-type :initarg :scale-type :initform nil)
   (intonation :accessor intonation :initarg :intonation :initform 'equal-temperament)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((s sc-scale) &rest initargs)
  (declare (ignore initargs))
  (init-sc-scale (tonic s) (scale-type s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Hmmm... this kinda works now I think...
(defun make-sc-scale (tonic scale-type)
    (make-instance 'sc-scale :tonic tonic :scale-type scale-type))

(defun scale-pattern (scale-type &optional custom-scale)
  (when (equalp scale-type 'custom)
    (unless custom-scale
      (error (format t "~%scale-pattern: no custom scale pattern supplied"))))
  (case scale-type
    (major '(2 2 1 2 2 2 1))
    (nat-minor '(2 1 2 2 1 2 2))
    (harm-minor '(2 1 2 2 2 1 2))
    (mel-minor-up '(2 1 2 2 2 2 1))
    (mel-minor-down	'(2 1 2 2 2 1 2))
    (ionian	'(2 2 1 2 2 2 1))
    (dorian '(2 1 2 2 2 1 2))
    (mixolydian '(2 2 1 2 2 1 2))
    (phrygian '(1 2 2 2 1 2 2))
    (lydian '(2 2 2 1 2 2 1))
    (aeolian '(2 1 2 2 1 2 2))
    (locrian '(1 2 2 1 2 2 2))
    (ahava-raba '(1 2 1 2 1 2 2))
    (whole-tone '(2 2 2 2 2))
    (pentatonic '(2 3 2 2))
    (chromatic '(1 1 1 1 1 1 1 1 1 1 1))
    (custom `,custom-scale)))

(defun init-sc-scale (tonic scale-type
		      &key
			(num-octaves 2)
			(intonation 'equal-temperament)
			midi-channel microtone-midi-channel)
  (declare (ignore intonation))
  (let* ((sp (scale-pattern scale-type))
	 (scale-pitches (loop repeat num-octaves
			   with p
			   with scale = '()
			   initially (setf p tonic)
			     (push tonic scale)
			   collect
			     (loop for interval in sp
				do
				  (setf p (midi-to-note
					   (+ (note-to-midi p)
					      interval)))
				  (push p scale))
			   finally (return (nreverse scale)))))
    (make-sc-set (init-pitch-list scale-pitches t midi-channel
				  microtone-midi-channel))))
