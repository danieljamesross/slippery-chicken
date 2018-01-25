;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File:             mexpand.lsp
;;;
;;; Purpose:          A simple granular synthesis instrument.  Two
;;;                   overlapping streams of grains are played
;;;                   simultaneously.  The start time of the grain is
;;;                   controlled automatically to move through the
;;;                   input file over the duration of the output file.
;;;                   E.g. if the input duration is 10 secs and output
;;;                   is 20 secs, given a grain duration of 0.1 secs,
;;;                   then the grain increment will be 0.05 secs.
;;;                   Thus a simple form of time-stretching is
;;;                   accomplished.  See instrument parameters for
;;;                   more information.
;;;
;;; Author:           Michael Edwards - m@michael-edwards.org
;;;                   Adapted for use in slippery chicken by
;;;                   DJR - mr.danielross@gmail.com
;;;
;;;
;;; $$ Last modified: Fri 10 Feb 2017 16:37:12 GMT
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DJR Thu 19 Jan 2017 18:55:32 GMT
;; Added for clm-play compatibility
(in-package :clm)

;; We generally set this to double-float to avoid floating point precision
;; errors but CLM uses single-floats.  Change back at the end of this file if
;; necessary.
;; DJR Thu 19 Jan 2017 18:55:32 GMT
;; Added for clm-play compatibility
(setf *read-default-float-format* 'single-float)

(definstrument mexpand

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Beginning of Instrument Parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (file          ;; Input file
     time          ;; Output file start time
     &key
     ;; DJR Thu 19 Jan 2017 18:55:32 GMT
     ;; Moved duration to the &key args for clm-play compatibility
     duration    ;; Output duration
     ;; DJR Thu 19 Jan 2017 18:45:35 GMT
     ;; ignore, for clm-play compatibility
     frequency
     (start 0)     ;; Input file start time (seconds)
     (end nil)     ;; Input file end time (seconds); nil indicates the
                   ;; end of the file  
     (srt 1.0) ;; Sampling-Rate Conversion factor (1 = no transposition)
     (amp 1.0) ;; amplitude (generally 0.0 - 1.0)
     (degree 45)     ;; Stereo Panning: 0 = left, 90 = right
     (distance 0)    ;; how far away is the sound?
     (grain-dur .01) ;; Grain duration (seconds)
     (grain-env '(0 0 25 1 75 1 100 0)) ;; Grain Amplitude-Envelope 
     (amp-env '(0 0 5 1 95 1 100 0)) ;; Amp-Envelope for the output file
     (overlap .5)           ;; How much the grains overlap (0.0 - 1.0)
     (rev-amt 0)            ;; Reverb amount
     (rev-env '(0 1 100 1)) ;; Reverb envelope
     (width 20)              ;; the src width
     (printing t)) ;; whether to print the number of seconds computed 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; End of Instrument Parameters, beginning of Instrument Setup Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Which sample in the output file do we begin on?
  (let* ((st (floor (* time *srate*)))
         ;; input file start sample
         (input-start-sample (floor (* start *srate*)))
         ;; open file handles for the src generators
         (f1 (open-input* file :start input-start-sample))
         ;; start sample for the second stream will be set in the run
         ;; loop once this stream kicks in
         (f2 (open-input* file))
         ;; src generator for the first grain stream
         (src-gen1 (make-src :input f1 :srate srt :width width))
         ;; src generator for the second grain stream
         (src-gen2 (make-src :input f2 :srate srt :width width))
         ;; Panning und Reverb generator
         (loc (make-locsig :degree degree :distance distance :reverb rev-amt)) 
         ;; grain envelope for the first grain steam
         (gr-env1 (make-env :envelope grain-env :duration grain-dur  
                            :scaler 0.5))
         ;; grain envelope for the second grain steam
         (gr-env2 (make-env :envelope grain-env :duration grain-dur 
                            :scaler 0.5))
         ;; Envelope for the complete output file
         (ampf (make-env :envelope amp-env :scaler amp :duration duration))
         ;; Envelope for the Reverb Stream
         (renv (make-env :envelope rev-env :scaler rev-amt :duration duration))
         ;; how many grains per stream will there be in the output file?
         (number-of-grains (ceiling duration grain-dur))
         ;; duration of the input file
         (snd-dur (sound-duration f1))
         ;; how many samples we have from the input file.  Remember
         ;; that :end nil means use the whole file (from the given :start)
         (input-dur (* *srate*
                       (if end
                           (- end start)
                           (- snd-dur start))))
         ;; Now we know how many grains we're going to need, as well
         ;; as how many input samples we have.  Now calculate the
         ;; grain increment (through the input file) in samples.
         (read-inc (floor input-dur number-of-grains))
         ;; convert grain duration in seconds to samples
         (grain-dur-in-samples (floor (* *srate* grain-dur)))
         (grain-overlap-samples (floor (* overlap grain-dur-in-samples))) 
         ;; where (in samples) do we start the first stream in the input file?
         (start1-in-samples input-start-sample)
         ;; And for the second stream?  This is dependent on :overlap
         (start2-in-samples (+ start1-in-samples grain-overlap-samples))
         ;; At which output file samples does the first grain stop?
         ;; This is where the first envelope stops and restarts too.
         (env1-stop (+ st grain-dur-in-samples))
         ;; Same for the second stream first grain.  Note here that
         ;; this is actually the point at which the second stream strarts.
         (env2-stop (+ st grain-overlap-samples))
         ;; The second stream doesn't play from the beginning
         (do-stream2 nil) 
         ;; Sample counter for the print function
         (count 0)
         ;; At which sample in the output file do we stop?
         (nd (+ st (floor (* *srate* duration)))))
    ;; Print the start time
    (when printing 
      (format t "~&Start time ~a.~%" time))
    ;; Some sanity checks:
    ;; Check that the given start and end times are not bigger than
    ;; the input file duration
    (when (or (> start snd-dur) (and end (> end snd-dur)))
      (error "Illegal file access requested: ~
              Input duration = ~a, :start = ~a, :end = ~a"
             snd-dur start end))
    ;; Make sure that start isn't greater than end
    (when (and end (>= start end))
      (error ":start (~a) >= :end (~a) ????" start end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Ende of setup code.  Beginning of run loop.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (run 
     ;; We loop from the output file start to the end sample
     (loop for i from st to nd do
        ;; if requested, print each time we've computed a second of sound
          (when printing 
            (if (= count *srate*)
                (progn
                  (setq count 1)
                  (print (round (/ i *srate*))))
                (incf count)))
        ;; Have we finished the first stream's current grain?
        ;; Im ersten Stream, haben wir den ersten Grain erledigt?
          (when (> i env1-stop)
            ;; update the next grain end sample and the input file 
            ;; start time for this grain
            (incf env1-stop grain-dur-in-samples)
            (incf start1-in-samples read-inc)
            ;; We can restart envelopes in CLM: we need to do this for 
            ;; the grain envelope here. 
            (mus-reset gr-env1)
            (mus-reset src-gen1) ;; reset the src too
            ;; If we're doing a time stretch, then our src generator
            ;; is now further along than the new start point so reset
            (setf (mus-location src-gen1) start1-in-samples))
        ;; Same for the second stream
          (when (> i env2-stop)
            (incf env2-stop grain-dur-in-samples)
            (mus-reset gr-env2)
            (mus-reset src-gen2)
            ;; The only difference here to stream 1 is that we now 
            ;; tell the system that stream 2 is in use (via 
            ;; do-stream2) and increment start2-in-samples after 
            ;; setting the src location rather than before 
            (setf do-stream2 t
                  (mus-location src-gen2) start2-in-samples)
            (incf start2-in-samples read-inc))
        ;; Set the reverb amount according to our envelope 
          (setf (locsig-reverb-ref loc 0) (env renv))
        ;; Output the sample via locsig 
          (locsig loc i (* (env ampf)   ; Signal * current amp-env value
                           ;; We add the two streams together
                           ;; Grain amp-env * SRC generator value 
                           (+ (* (env gr-env1) (src src-gen1)) 
                              ;; If stream 2 hasn't yet begun, we add 0
                              (if do-stream2
                                  (* (env gr-env2) (src src-gen2))
                                  0.0))))))
    (close-input f1)
    (close-input f2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End of instrument code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

(clm-play +tp+ 1 'el 'default-group
	  :pitch-synchronous t
	  :check-overwrite nil
	  :duration-run-over t
	  :output-name-uniquifier "mexpand-"
	  :pan-min-max '(30 60)
	  :rev-amt .05
	  :amp-env '(0 0 20 1 70 1 100 0)
	  :clm-ins #'clm::mexpand
	  :clm-ins-args '(:grain-dur .001
			  :overlap 0.01
			  :grain-env (0 0.1 100 .1)))

|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF mexpand.lsp

