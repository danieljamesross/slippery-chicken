;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File:             splinter.lsp
;;;
;;; Version:          1.1 for slippery chicken
;;;
;;; Purpose:          Common Lisp Music (clm-2) Instrument for multi-voice
;;;                   granulation of a sound file with sampling-rate conversion
;;;                   for each separate voice.  
;;;
;;;                   Accepts mono input files only and produces stereo output
;;;                   with grains distributed evenly across the two channels.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;                   Adapted for use in slippery chicken by
;;;                   DJR - mr.danielross@gmail.com
;;;
;;; Courtesy:         If you use this instrument, or any other written by me,
;;;                   please be courteous enough to acknowledge this fact in
;;;                   programme notes etc.
;;;
;;; Usage:            The instrument has the following parameters:
;;;
;;;                   FILE: Input sound file
;;;
;;;                   TIME: output start time (secs.)
;;;                   
;;;                   CHANNEL: If input is multi-channel, which
;;;                   channel to process (0=left, 1=right etc.) 
;;;                   
;;;                   START: input file start time (secs.)
;;;                   
;;;                   END: input file stop time (secs.).  When zero, then the
;;;                   end of the file.  Grains will be generated using samples
;;;                   only between start and end.
;;;                   
;;;                   DURATION: output file duration (secs.).  When zero, then
;;;                   also end - start.
;;;                   
;;;                   GRAIN-ENV: the grain lengths in secs, ranging over the
;;;                   length of the output file.  This will be set to
;;;                  +MIN-GRAIN-LENGTH+ if too small.
;;;                   
;;;                   CENTER-DEVIATION-ENV: the amount by which the grain can
;;;                   randomly deviate from the current input sample (which is
;;;                   incremented as time progresses by an amount related to
;;;                   the input file duration and output file duration--if the
;;;                   input is 10 secs long and the output is to be 100 secs,
;;;                   we will increment the current sample every 10 (output)
;;;                   samples).
;;;                   
;;;                   The envelope values should be between 0 and 1 (0 means
;;;                   use only the current sample as start of grain and 1 means
;;;                   range over the entire length of the sound file (or,
;;;                   rather, between START and END)--before and after current
;;;                   sample).
;;;                   
;;;                   GRAIN-LENGTH-SCALERS: After reading grain-env to
;;;                   calculate the current grain length, the value is scaled
;;;                   by a random amount between these two extremes.  Can only
;;;                   be a list of two values.
;;;                   
;;;                   PRINT-GRAIN-INFO: If t, then information about grain
;;;                   length etc. will be printed each time a new grain is
;;;                   created.
;;;                   
;;;                   DEBUG: If t, more information is printed for debugging
;;;                   purposes, no input samples are read and no output samples
;;;                   are written.
;;;                   
;;;                   SILENCE-ENV: Controls which percentage of the grains will
;;;                   be played.  y values should range between 0.0 and 1.0
;;;                   where 1.0 means all the grains will be silent and 0.0 all
;;;                   will be played.  (The envelope value is used as a
;;;                   threshold against which a random value > 0 and < 1 is
;;;                   compared to decide whether or not the grain will be
;;;                   output.)
;;;                   
;;;                   VOICES: How many grains (or grain layers) are played
;;;                   simultaneously.
;;;                   
;;;                   SRT: This can either be a single value or a list of
;;;                   values.  If a single value, then this will be the
;;;                   sampling-rate conversion factor for every grain in each
;;;                   voice.  If a list, then the values will be the src
;;;                   factors for each successive voice (remains the same for
;;;                   every grain in the voice, though c.f. srt-expt-env).  The
;;;                   number of values does not need to be the same as the
;;;                   number of voices, as they will simply be repeated (if not
;;;                   enough) or ignored (if too many), e.g. srt = (1 2 3)
;;;                   voices = 7, srt for each voice = (1 2 3 1 2 3 1).
;;;                   
;;;                   ***** N.B.  If srt = 1.0, then simple sound file
;;;                   reading is performed (via IN-ANY) instead of
;;;                   SRC, which for me is at least four times faster.
;;;
;;;                   For the case where srt is 1.0 but you want an srt-env,
;;;                   set srt to t 
;;;                   
;;;                   WIDTH: The sync width for the sampling-rate
;;;                   conversion (5 is the CLM default but a higher value will
;;;                   increase quality at the expense of processing time--try
;;;                   20 for example).
;;;                   
;;;                   SRT-ENV: a transposition envelope for glissandi
;;;                   
;;;                   SRT-EXPT-ENV: This tries to create some pitch development
;;;                   in the output file by raising the srts of each voice to
;;;                   the exponent given by the envelope.  This is done on a
;;;                   grain-by-grain, not a sample-by-sample basis.  If the
;;;                   envelope value is < 1, then the src range of the voices
;;;                   will be narrower, when > 1 wider.
;;;                   
;;;                   SRT-RANDOM-ENV: Another way to vary pitch: the srt of
;;;                   each grain will be scaled by a random factor between 1
;;;                   and 1 +/- the envelope value.
;;;                   
;;;                   GRAIN-FOCUS-ENV: This envelope creates a focus on certain
;;;                   voices in the mix and so only really makes sense when
;;;                   you're using multiple srts.  The y values range from 0 to
;;;                   1 where 0 means that most of the amplitude energy will be
;;;                   given to the first voice; 1 means that most of the energy
;;;                   will be on the highest voice; and .5 means most will be
;;;                   on the middle voice with the outer voices having the same
;;;                   (lowest) energy.
;;;                   
;;;                   GRAIN-FOCUS-ENV-EXPT: The exponent that the amplitudes
;;;                   applied to the grains from grain-focus-env are raised to.
;;;                   
;;;                   GRAIN-FOCUS-ENV-MIN: The minimum amplitude value that a
;;;                   voice should have when processed by grain-focus-env.  NB
;;;                   This does not relate to the actual envelope values (which
;;;                   always range >=0 <= 1) but rather the min value returned
;;;                   by our focussing function.
;;;                   
;;;                   RAMP: how long in samples the ramp up/down will be.  If
;;;                   this is less than +HALF-MIN-GRAIN-LENGTH+, then
;;;                   +HALF-MIN-GRAIN-LENGTH+ will be used instead
;;;                   
;;;                   AMP:  The amplitude
;;;                   
;;;                   AMP-ENV:  Standard amplitude envelope.
;;;                   
;;;                   REV-AMT:  Reverberation amount.
;;;                   
;;;                   REV-ENV: An envelope for reverb amount.  REV-AMT is the
;;;                   scaler to this envelope.
;;;                   
;;;                   START-TOGETHER: All voices will start their first grain
;;;                   at the same time if this parameter is non-nil (and
;;;                   thereafter never again--or at least it is highly
;;;                   unlikely).  This could be seen as a bug as it makes for
;;;                   an attack at the beginning of the output file that is
;;;                   considerably louder than the rest of the file.  This is
;;;                   seen by me at least as a feature and not a bug, however,
;;;                   as these attacks make for some very useful sounds.  If
;;;                   you don't want this, set start-together to nil.
;;;                   
;;;                   PRINTING: Whether to print the progress of the processing
;;;                   on a second-by-second basis.
;;;
;;; $$ Last modified: Fri 10 Feb 2017 16:44:21 GMT
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :clm)

(defconstant +SPLINTER-MIN-GRAIN-LENGTH+ 256)
(defconstant +HALF-SPLINTER-MIN-GRAIN-LENGTH+ 
  (/ +SPLINTER-MIN-GRAIN-LENGTH+ 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro get-grain-amp (focus voice num-voices &key (min 0.1) (expt 1.0))
  `(let* ((voice-val (* ,voice (/ 1.0 (1- ,num-voices))))
          (diff (abs (- ,focus voice-val)))
          (val (+ ,min (* (- 1.0 diff) (- 1.0 ,min)))))
     (if (= ,expt 1.0) val (expt val ,expt))))


(defun scale-value (item new-low new-high old-min old-max)
      (+ (/ (* (- item old-min)
	       (- new-high new-low))
	    old-max)
	 new-low))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(definstrument splinter                 
    ;;(splinter :c-file "/user/michael/ins/.cins/splinter.c")
    (file time &key
          (channel 0)
          ;; (output-channels 2)
          (start 0)
          (end 0)
	  frequency ; for compatibility with clm-play
	  degree
          (duration 0)
          (grain-env '(0 .01 100 .01))
          (center-deviation-env '(0 0.5 100 0.5))
          (grain-length-scalers '(.5 2.0))
          (print-grain-info nil)
          (debug nil)
          (silence-env nil)
          (voices 2)
          (srt 1.0)
          (width 20)
          (srt-expt-env nil)
          (srt-random-env nil)
          (srt-env '(0 0 100 0))
          (grain-focus-env nil)
          (grain-focus-env-expt 1.0)
          (grain-focus-env-min 0.1)
          (ramp 0)
          (amp 1.0)
          (amp-env '(0 1 100 1))
          (rev-amt 0)
          (rev-env nil)
          (start-together t)
          (printing t)
	  ;; DJR Fri 20 Jan 2017 17:19:43 GMT
	  ;; Make the amplitude control the number of voices
	  (amp-to-voices)
	  ;; DJR Fri 20 Jan 2017 17:21:30 GMT
	  ;; min and max number of voices when amp-to-voices is t
	  (min-max-voices '(2 32)))
  (when debug
    (setq print-grain-info t))
  (when (and (listp srt)
             (not voices))
    (setq voices (length srt)))
  (when amp-to-voices
    (setq voices (round (scale-value amp
				     (first min-max-voices)
				     (second min-max-voices)
				     0 1))))
  (let* ((st (floor (* time *srate*)))
         (file-dur (sound-duration file))
         (ramp-len (max ramp +HALF-SPLINTER-MIN-GRAIN-LENGTH+))
         (current-ramp-len ramp-len)
         (1-minus-ramp-len (1- ramp-len))
         (input-end (if (= end 0) 
                        file-dur 
                      (if (> end file-dur)
                          (progn
                            (warn "end (~f) > file duration (~f).  ~
                                 Using file duration instead of end."
                                  end file-dur)
                            file-dur)
                        end)))
         (real-dur (if (= duration 0) (- input-end start) duration))
         (input-start-samples (floor (* *srate* start)))
         (input-end-samples (floor (* *srate* input-end)))
         (num-input-samples (floor (- input-end-samples input-start-samples)))
         (nd (+ st (floor (* *srate* real-dur))))
         (output-chans (mus-channels *output*))
         (do-src (if (eq srt t)
                     (setf srt 1.0)
                   (or srt-random-env srt-expt-env 
                       (not (and (numberp srt) (= srt 1.0))))))
         (files (when do-src
                  (make-array voices :initial-contents 
                              (loop repeat voices collect
                                    (open-input* file :channel channel)))))
         (fil (unless do-src (open-input* file)))
         (srcs (when do-src
                 (make-array voices :initial-contents
                             (if (listp srt)
                                 (loop for i below voices collect 
                                       (nth (mod i (length srt)) srt))
                               (loop repeat voices collect srt)))))
         (src-gens (when do-src
                     (make-array voices :initial-contents
                                 (loop for i below voices collect
                                       (make-src :input (aref files i) 
                                                 :srate (aref srcs i)
                                                 :width width)))))
         (sexp-env (when srt-expt-env
                     (make-env :envelope srt-expt-env :duration real-dur )))
         (srtenv (make-env :envelope srt-env :duration real-dur))
         (renv (when rev-env 
                 (make-env :envelope rev-env :duration real-dur
                           :scaler rev-amt)))
         ;; We have a "central" sample which is used as a place from which we
         ;; can deviate when choosing grain starting points.  This is
         ;; incremented over the whole length of the input samples as we
         ;; progress through the algorithm.  We have to pace this so that we
         ;; use all the samples by the time we have finished the output file so
         ;; we calculate the increment here.
         (input-inc (/ (float num-input-samples) (float (- nd st))))
         (ampf (make-env :envelope amp-env :scaler amp :duration real-dur))
         (gr-env (make-env :envelope grain-env
                           :duration real-dur)) 
         (dev-env (make-env :envelope center-deviation-env :duration real-dur))
         (s-env (when silence-env
                  (make-env :envelope silence-env :duration real-dur)))
         (srt-renv (when srt-random-env
                     (make-env :envelope srt-random-env :duration real-dur)))
         (gfe (when grain-focus-env 
                (make-env :envelope grain-focus-env :duration real-dur)))
         (min-grain-scaler (min (car grain-length-scalers)
                                (cadr grain-length-scalers)))
         (max-grain-scaler (max (car grain-length-scalers)
                                (cadr grain-length-scalers)))
         (grain-scaler-diff (- max-grain-scaler min-grain-scaler))
         ;; the grain length at the beginning (samples).
         (grain-length-beg (floor (* *srate* (second grain-env))))
         (pos 0.0)
         (1-minus-pos 0.0)
         ;; we use pos to put the sound between two speakers and
         ;; these vars to choose which speakers these will be
         (out1-chan 0)
         (out2-chan 0)
         ;; This number gets incremented by <input-inc> each time through the
         ;; run loop (hence it's a float).  Each time it gets to 1.0 or more,
         ;; we increment the input sample and reset it to 0.0.
         (counter 0.0)
         (current-input-sample 0)
         (gr-len-samples 0)  
         ;; This will hold the envelope value from <center-deviation-env>
         (deviation 0.0)
         ;; The deviation is randomised between 0.0 and <deviation> so this is
         ;; the actual value we will use.
         (random-deviation 0.0)
         ;; The start sample for the current grain.
         (start-sample 0)
         ;; How many samples are before <current-input-sample>.
         (before-current 0)
         ;; How many samples are after <current-input-sample>.
         (after-current 0)
         ;; Sample value for left channel.
         (out1 0.0)
         ;; Sample value for right channel.
         (out2 0.0)
         (ampval 0.0)
         ;; We have to window (ramp up/down) the grains.  This is the current
         ;; window sample used to scale the grain values.
         (scl 1.0)
         ;; Are we going to deviate before or after <current-input-sample> when
         ;; we randomly choose our grain start point.
         (up? nil)
         ;; This one is just for printing seconds computed.
         (count 0)
         (gr-len-warnings 0)
         (srt-warnings 0)
         (grain-index 0)
         (actual-grain-length 0)
         (do-grain t)
         (steady-begins ramp-len)
         (sample 0.0)
         (samps-needed 0)
         (s-val 0.0)
         (srt-renv-val 0.0)
         (sexp-val 0.0)
         (gfe-val 0.0)
         (srt-offset 0.0)
         (gr-val 0.0)
         (current-srt 0.0)
         (actual-srt 0.0)
         (ramp-down-begins 0)
         (ramp-inc 1)
         ;; We do this here because we can't call make-fft-window inside run.
         (window (make-fft-window blackman2-window (* 2 ramp-len)))
         (ramp-up (make-array ramp-len :element-type 'double-float
                              :displaced-to window 
                              :displaced-index-offset 0))
         (ramp-down (make-array ramp-len :element-type 'double-float
                                :displaced-to window 
                                :displaced-index-offset ramp-len)))
    ;; The window created does not start and end with 0 so make it so.
    (setf (aref window 0) 0.0d0 ;;(make-short-float 0.0d0)
          (aref window (1- (car (array-dimensions window)))) 0.0d0
          ;;(make-short-float 0.0)
          )
    (when debug
      (format t "~&real-dur = ~f input-inc = ~f input-start-samples=~d ~
                  (~f secs) input-end-samples=~d (~f secs) ~%~%" 
              real-dur input-inc input-start-samples 
              (/ input-start-samples *srate*)
              input-end-samples (/ input-end-samples *srate*))
      (if do-src 
          (print "Using SRC")
        (print "Using INA")))
    (run
     (loop for voice below voices do
           #|
           (when do-src
             (declare (type (array sr) src-gens)
             (type (array io) files)))
             |#
           (declare (type :integer output-chans out1-chan out2-chan))
           (when (> voice 0)
             (restart-env ampf)
             (restart-env gr-env)
             (restart-env dev-env)
             (restart-env srtenv)
             (when grain-focus-env (restart-env gfe))
             (when silence-env (restart-env s-env))
             (when srt-random-env (restart-env srt-renv))
             (when srt-expt-env (restart-env sexp-env)))
           (setq current-input-sample input-start-samples
                 before-current 0
                 after-current num-input-samples
                 grain-index 0
                 gr-len-samples 
                 (if start-together 
                     0
                   (progn 
                     (setq do-grain nil)
                     (floor 
                      (if (<= grain-length-beg 
                              +SPLINTER-MIN-GRAIN-LENGTH+)
                          (+ +SPLINTER-MIN-GRAIN-LENGTH+
                             (random +SPLINTER-MIN-GRAIN-LENGTH+))
                        ;; 13/9/07: avoid randomness: make each voice start
                        ;; equally spaced across the opening grain length 
                        (* voice (/ grain-length-beg voices)))
                           ;; (+ +SPLINTER-MIN-GRAIN-LENGTH+
                           ;; (random (- grain-length-beg 
                           ;; +SPLINTER-MIN-GRAIN-LENGTH+)))
                           ))))
           (clm-print "~%~d" gr-len-samples)
           (loop for i from st to nd do
                 (when printing 
                   (if (= count *srate*)
                       (progn
                         (clm-print "~%Voice ~d/~d: ~d/~f secs"
                                    (+ 1 voice) voices 
                                    (round (/ i *srate*)) real-dur)
                         (setf count 1))
                     (incf count)))
                 ;; Get our envelope values.
                 (setq ampval (env ampf)
                       s-val (if silence-env (env s-env) 0.0)
                       sexp-val (if srt-expt-env (env sexp-env) 1.0)
                       gr-val (env gr-env)
                       srt-renv-val (when srt-random-env (env srt-renv))
                       deviation (env dev-env))
                 (incf counter input-inc)
                 ;; Time to increment our input sample?
                 (when (>= counter 1.0)
                   (incf current-input-sample)
                   (incf before-current)
                   (decf after-current)
                   (setq counter 0.0))
                 ;;
                 ;; START NEW GRAIN DATA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                 ;;
                 ;; Have we written out all the current grain's samples?  If so
                 ;; make a new grain:
                 (when (>= grain-index gr-len-samples)
                   ;; Silent grain?
                   (setq do-grain (if silence-env 
                                      (> (random 1.0) s-val)
                                    t)
                         ;; randomise the grain length based on the envelope
                         ;; value.  randomly choose whether to start the grain
                         ;; before or after our current input sample.
                         ;; clm's random function is not like lisp's as it 
                         ;; returns always a float, not an int, therefore this
                         ;; is a bit more complicated than a simple 
                         ;; (zerop (random 2))
                         up? (>= .5 (random 1.0))
                         ;; Get the deviation amount from our deviation
                         ;; envelope value.    
                         random-deviation (if (= deviation 0.0)
                                              0.0
                                            (random deviation))
                         pos (+ .05 (random .9))
                         1-minus-pos (- 1.0 pos)
                         out1-chan (random output-chans)
                         out2-chan (if (= out1-chan (1- output-chans))
                                       0
                                     (1+ out1-chan))
                         ;; Set the start sample based on whether we're going
                         ;; to start before or after the current sample and the
                         ;; random deviation.
                         actual-grain-length 
                         (* gr-val (+ min-grain-scaler 
                                      (random grain-scaler-diff)))
                         gr-len-samples (floor 
                                         (* *srate* actual-grain-length))
                         ;; the srt given for this voice
                         current-srt (when do-src (aref srcs voice))
                         ;; the srt we want, as given by the srt-expt-env
                         actual-srt (* (if srt-random-env
                                           (1+ (if up?
                                                   (random srt-renv-val)
                                                 (- (random srt-renv-val))))
                                         1.0)
                                       (if srt-expt-env
                                           (expt current-srt sexp-val)
                                         current-srt))
                         ;; what we'll have to add to the src gen to get our
                         ;; desired srt
                         srt-offset (if (or srt-expt-env srt-random-env)
                                           (- actual-srt current-srt)
                                         0.0)
                         samps-needed (if do-src
                                          (* gr-len-samples actual-srt)
                                        gr-len-samples)
                         start-sample 
                         (floor (if up? 
                                    (+ current-input-sample 
                                       (* after-current random-deviation))
                                  (- current-input-sample
                                     (* before-current random-deviation)))))
                   ;; (warn "~&out1-chan: ~a" out1-chan)
                   ;; (print out1-chan)
                   ;; Check to see if there's enough output samples left to 
                   ;; write our grain
                   (when (> (+ i gr-len-samples) nd)
                     ;; If there's not even enough for our min grain length
                     ;; then simply don't write the grain.
                     (if (> (+ i +SPLINTER-MIN-GRAIN-LENGTH+) nd)
                         (setq do-grain nil)
                       (setq gr-len-samples (- nd i))))
                   ;; Don't go past the end of the input samples.
                   (when (> (+ start-sample samps-needed)
                            input-end-samples) 
                     (setq start-sample (- input-end-samples samps-needed)))
                   ;; Don't try to start too soon!
                   (when (< start-sample input-start-samples)
                     (setq start-sample input-start-samples
                           gr-len-samples (floor (/ num-input-samples 
                                                    actual-srt)))
                     (when (< srt-warnings 30)
                       (clm-print "~%Not enough samps for grain len of ~fs."
                                  actual-grain-length)
                       (clm-print "~%   with an actual srt of ~f" actual-srt)
                       (clm-print "~%   num-input-samples = ~d" 
                                  num-input-samples)
                       (clm-print "~%   Grain length will be of ~d (~f secs)"
                                  gr-len-samples 
                                  (float (/ gr-len-samples *srate*)))
                       (incf srt-warnings)))
                   ;; Don't let the grain be smaller than our defined minimum. 
                   (when (< gr-len-samples +SPLINTER-MIN-GRAIN-LENGTH+)
                     (when (< gr-len-warnings 30)
                       (clm-print "~%Grain len too short. Changing ~d to ~d"
                                  gr-len-samples +SPLINTER-MIN-GRAIN-LENGTH+)
                       (incf gr-len-warnings))
                     (setq gr-len-samples +SPLINTER-MIN-GRAIN-LENGTH+))
                   ;; Get our ramp-down point.
                   (setq ramp-down-begins (- gr-len-samples ramp-len)
                         ramp-inc 1
                         current-ramp-len ramp-len
                         steady-begins current-ramp-len
                         ;; Reset the grain-index.
                         grain-index 0)
                   ;; If our grain length is too short for the ramp, adjust.
                   (when (< ramp-down-begins current-ramp-len)
                     (setq ramp-down-begins (floor gr-len-samples 2)
                           steady-begins ramp-down-begins
                           current-ramp-len ramp-down-begins
                           ;; Skip some of our ramp values so that we arrive
                           ;; at the end of the ramp in time.
                           ramp-inc (round ramp-len ramp-down-begins))
                     (when debug
                       (clm-print "~%ramp-down-begins ~d ramp-inc ~d"
                                  ramp-down-begins ramp-inc)))
                   (when do-src (setf (mus-location (aref src-gens voice)) 
                                  start-sample))
                   (when debug 
                     (clm-print 
                      "~%i = ~d (~f secs) current-input-sample = ~d (~f secs)"
                      i (/ i *srate*) 
                      current-input-sample 
                      (/ current-input-sample *srate*)))
                   (when print-grain-info
                     (clm-print "~%Grain start = ~d samps = ~f secs"
                                start-sample (/ start-sample *srate*))
                     (clm-print "~%   Grain length = ~d samps = ~f secs"
                                gr-len-samples 
                                (/ gr-len-samples *srate*))
                     (clm-print "~%   srt-offset=~f actual-srt=~f"
                                srt-offset actual-srt)
                     (clm-print "~%   ramp-inc = ~d grain index = ~d" 
                                ramp-inc grain-index)
                     (clm-print "~%   steady-begins ~d ramp-down-begins ~d"
                                steady-begins ramp-down-begins)))
                 ;;                 
                 ;; END NEW GRAIN DATA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                 ;;
                 (unless debug
                   (if do-grain
                       (setq scl (cond ((< grain-index steady-begins) 
                                        (aref ramp-up 
                                              (floor (min 1-minus-ramp-len 
                                                          (* ramp-inc 
                                                             grain-index)))))
                                       ((< grain-index ramp-down-begins) 1.0)
                                       (t (aref ramp-down 
                                                (floor 
                                                 (min 1-minus-ramp-len 
                                                      (- (* ramp-inc
                                                            grain-index)
                                                         ramp-down-begins))))))
                             gfe-val (if grain-focus-env
                                         (get-grain-amp (env gfe) voice voices
                                                        :min 
                                                        grain-focus-env-min
                                                        :expt 
                                                        grain-focus-env-expt)
                                       1.0)
                             sample (* scl gfe-val
                                       (if do-src
                                           (src (aref src-gens voice) 
                                                (+ (env srtenv) srt-offset))
                                         (in-any (+ grain-index start-sample) 
                                                 channel fil)))
                             out1 (* ampval 1-minus-pos sample)
                             out2 (* ampval pos sample))
                     (setq out1 0.0
                           out2 0.0))
                   ;; Write out the samples and reverb them.
                   (out-any i out1 out1-chan)
                   (out-any i out2 out2-chan)
                   (when *reverb* 
                     (outa i (* (if rev-env (env renv) rev-amt)
                                (/ (+ out1 out2) 2.0))
                           *reverb*)))
                 (incf grain-index))))
    (if do-src 
        (loop for v below voices do (close-input (aref files v)))
      (close-input fil))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

;; Example usage in clm-play

(clm-play +mini+ 1 'pno 'default-group
	      :pitch-synchronous t
	      :check-overwrite nil
	      :duration-run-over t
              :output-name-uniquifier "samp2-"
	      :snd-selector #'(lambda (sflist pitch event)
				(declare (ignore event))
				(get-nearest-by-freq
				 (frequency pitch) (data sflist)))
	      :pan-min-max '(30 60)
	      :rev-amt .05
	      :src-scaler 0.25
	      :amp-env '(0 0 5 1 70 1 100 0)
	      :output-name-uniquifier "splinter-"
	      :clm-ins #'clm::splinter	
	      :clm-ins-args '(:voices 8	
			      :ramp 5	
			      :grain-env (0 .01 50 .5 100 .01) 
			      :amp-to-voices t)

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF splinter.lsp
