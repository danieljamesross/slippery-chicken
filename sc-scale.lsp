(in-package :slippery-chicken)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass sc-scale (sc-set)
  ;; I bet some things are missing from here...
  ((scale-type :accessor scale-type :initarg :scale-type :initform 'major)
   (tonic :accessor tonic :initarg :tonic :initform 'c4)
   (intonation :accessor intonation :initarg :intonation :initform 'equal-temperament)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((s sc-scale) &rest initargs)
  (declare (ignore initargs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Hmmm... this doesn't work, but init-sc-scale does. For some reason the 'tonic
;; and 'scale-type args are not being passed. Think it's something do do with
;; how I've defined the class...(?)
(defun make-sc-scale (tonic scale-type)
    (make-instance 'sc-scale :tonic tonic :scale-type scale-type))

(defun scale-pattern (scale-type)
      (case scale-type
	(major '(2 2 1 2 2 2 1))
	(minor '(2 1 2 2 1 2 2))))

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
