;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File:             samp2.lsp
;;;
;;; Purpose:          A 'sampling' instrument: performs high-quality
;;;                   sampling-rate conversion (transposition)  of a sound
;;;                   file.  
;;;
;;;                   When duration is nil the instrument automatically plays
;;;                   the whole input file either forwards (or backwards: if
;;;                   <reverse> is t).  
;;;
;;;                   If output duration > input duration then the whole input
;;;                   file will be played forwards then backwards until the
;;;                   duration is used up. 
;;;
;;;                   See the instrument parameters for more possibilities.
;;;
;;;                   This instrument was extended by Jules Rawlinson to
;;;                   include High and Low Pass filtering using the Butterworth
;;;                   filter (cload "mus:clm-3;butterworth.cl") and also
;;;                   supports various lfo modulations including amplitude,
;;;                   filter modulation, vibrato (pitch), and a hanning window
;;;                   for grain enveloping. Look for the hidden waveshaping!
;;;
;;;                   This was extended again in Feb 2013 by Michael Edwards to
;;;                   allow for multi-channel input file processesing.  If a
;;;                   mono input file is passed, we use locsig to locate in
;;;                   n-channel output space.  If the input has 2+ channels
;;;                   then the original spacing is retained and the same
;;;                   processing is applied to each channel successively.
;;;
;;; Authors:          Michael Edwards - m@michael-edwards.org
;;;                   Jules Rawlinson - j@pixelmechanics.com
;;;                   Adapted for use in slippery chicken by
;;;                   DJR - mr.danielross@gmail.com
;;;
;;;
;;; $$ Last modified: Fri 10 Feb 2017 16:46:04 GMT
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :clm)
(setf *read-default-float-format* 'single-float)
;;; You must compile and load butterworth before doing the same with this ins;
;;; loading alone is not enough, e.g.
;(load (compile-file "/Applications/slippery-chicken-OSX.app/Contents/Resources/clm-4/butterworth.cl"))

(defscins samp2
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Beginning of Instrument Parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (file ;; Input file path
     time ;; Point of output in file in seconds
     &key ;; the following parameters are optional
     ;; MDE Mon Nov  4 10:08:45 2013 -- just for
     ;; compatibility with clm-play i.e. not used
     frequency
     (duration nil)         ;; Output file duration; when nil then the whole
     ;; input file will be played. 
     ;; In Lisp, Yes/No or True/False (so-called
     ;; Booleans) are indicated by T and Nil.
     (reflect nil)          ;; When duration is nil, whether the input file
     ;; should play forwards then backwards.
     (reverse nil)          ;; Begin going backwards?
     (start 0)              ;; Input file start time (seconds).
     (end 0)                ;; Input-file end time (seconds).
     (srt 1.0)              ;; Sampling-rate Conversion: 1 = no 
     ;; transposition, 2 = oktave high, 0.5 = oktave
     ;; lower etc.
     (width 5)              ;; How many samples to convolve with the sinc table
     ;; (the higher the better but also the slower the
     ;; processing).
     (srt-env '(0 0 100 0)) ;; Sampling-rate Conversion Envelope (glissando);
     ;; when the y value is 0 there is no transposition
     ;; beyond that of srt-scaler.
     (srt-scaler 1.0)		     ;; Scaler for srt-env.
     (amp 1.0)			     ;; Amplitude, usually > 0.0 <= 1.0
     (amp-env '(0 0 5 1 95 1 100 0)) ;; Amplitude Envelope, y-values, like amplitude,
     ;; are usually > 0.0 <= 1.0.  amp is used as a
     ;; scaler for this envelope. 
     ;; MDE Fri Feb 15 19:46:28 2013 -- this will only be used given mono input
     (degree 45)            ;; Stereo Panning:  0 = left, 45 = middle, 90 =
     ;; right.
     (distance 0)           ;; A distance effect.  This is used to create a
     ;; combination of direct and reverberated signal.
     ;; Try values between 0 (no effect) and 100.
     (rev-env '(0 1 100 1)) ;; Reverberation Envelope
     (rev-amt 0)            ;; Reverberation.  0.1 quite a lot. This value is 
     ;; a scaler for rev-env
     (printing t) ;; Whether the number of seconds processed should
     ;; be printed to the lisp listener whilst running.
     
     ;; NEW FEATURES ADDED BY JULES RAWLINSON

     (wv-shp-amt nil) ;; waveshaping amount 
 
     (lp-two nil) ;; second pass lowpass
     (hp-two nil) ;; second pass hipass

     (amp-base 1.0) ;; Break-point curve base for amp-env
     (srt-base 1.0) ;; Break-point curve base for srt-env

     ;; amplitude options
     (amp-lfo-frq-env '(0 0 100 0)) ;; amp lfo frequency envelope
     (amp-lfo-amt-env '(0 0 100 0)) ;; amp lfo amount envelope

     ;; lp filter options
     (lpflt-frq nil)                      ;; base lowpass frequency
     (lpflt-frq-env '(0 22050 100 22050)) ;; frequency envelope, y = hz  
     (lpflt-lfo-frq-env '(0 0 100 0))     ;; filter lfo frequency envelope
     (lpflt-lfo-amt-env '(0 0 100 0))     ;; filter lfo amount envelope
     (lp-base 1.0)                        ;; lowpass breakpoint curve

     ;; hp filter options
     (hpflt-frq nil)                  ;; base hipass frequency
     (hpflt-frq-env '(0 1 100 1))     ;; frequency envelope, y = hz  
     (hpflt-lfo-frq-env '(0 0 100 0)) ;; filter lfo frequency envelope
     (hpflt-lfo-amt-env '(0 0 100 0)) ;; filter lfo amount envelope
     (hp-base 1.0)                    ;; hipass breakpoint curve

     ;; ringmod options
     (rm-frq nil)              ;; base ringmod frequency
     (rm-amp 1.0)              ;; ringmod amp
     (rm-amt-env '(0 0 100 0)) ;; ringmod amount
     (rm-frq-env '(0 1 100 1)) ;; frequency envelope, y = hz
     (rm-amt-base 1.0)         ;; ring mod amount brkpoint curve
     (rm-frq-base 1.0)         ;; ring mod frq env brkpoint curve

     ;; ampmod options
     (am-frq nil)              ;; base ringmod frequency
     (am-amp 1.0)              ;; ringmod amp
     (am-amt-env '(0 0 100 0)) ;; ringmod amount
     (am-frq-env '(0 1 100 1)) ;; frequency envelope, y = hz
     (am-amt-base 1.0)         ;; amp mod amount brkpoint curve
     (am-frq-base 1.0)         ;; amp frq env brkpoint curve

     ;; pitchmod options
     (pm-frq nil)              ;; base ringmod frequency
     (pm-amp 1.0)              ;; ringmod amp
     (pm-amt-env '(0 0 100 0)) ;; ringmod amount
     (pm-frq-env '(0 1 100 1)) ;; frequency envelope, y = hz

     (hanning nil) ;; implement a hanning window
     ;; rather than use an envelope
     )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; End of Instrument Parameters, beginning of Instrument Setup Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; MDE Fri Feb 15 19:38:49 2013 -- added loop for each input file channel
  ;; here
  (let* ((input-chans (sound-chans file))
         (mono-in (= 1 input-chans))
         (st (floor (* time *srate*)))   ;; Output File Start Sample
         (start-sample (if (zerop start) ;; Input File Start Sample
                           0
                           (floor (* *srate* start))))
         ;; Input File End Sample
         (end-sample (if (zerop end)
			 ;; DJR Tue 20 Mar 2018 16:27:29 GMT
			 ;; sound-frames throws an error in clm-5
			 ;; so let's get rid of it.
                         ;(sound-frames file)
                         (floor (* *srate* end))))
         ;; The duration (seconds) of the input file, taking into consideration
         ;; whether we're reflecting or not. 
         (dur-full (/ (* (if reflect 2 1) 
                         (- end-sample start-sample))
                      srt
                      *srate*))
         ;; The actual output duration we'll use (seconds).
         (dur (if duration duration dur-full)))
    (loop for chan below input-chans do
         (let* ( ;; Input File Input/Output (I/O) Structure MDE Fri Feb 15
                ;; 19:38:23 2013 -- change to open-input* so we can have
                ;; directory search to find sndfiles
                (f (open-input* file :channel chan
                                :start (if (and reverse (zerop start))
                                           end-sample
                                           start-sample)))
                ;; The Sampling-Rate Conversion (SRC) Unit Generator
                (src-gen (make-src :input f :srate srt :width width))
                ;; The Sound Location Unit Generator
                ;; MDE Fri Feb 15 19:43:29 2013 -- use locsig only when mono
                ;; input, otherwise we map channel per channel, processing each
                ;; separately.
                (loc (when mono-in
                       (make-locsig :degree degree :distance distance :reverb
                                    rev-amt)))
                ;; The SRC envelope
                (senv (make-env :envelope srt-env :scaler srt-scaler
                                :offset 0.0 :duration dur :base srt-base))
                ;; The amplitude envelope
                (ampf (make-env :envelope amp-env :scaler amp 
                                :duration dur :base amp-base)) 
                ;; The reverb envelope
                (renv (make-env :envelope rev-env :scaler rev-amt   
                                :duration dur))
                (count 0) ;; used for printing only
                (ml 0)    ;; the current sample location of the SRC generator

                ;; NEW FIELDS ADDED BY JULES RAWLINSON

                (out-sig 0.0) ;; output sample object
                (lpflt-lfo 0.0) ;; lopass filter lfo object
                (hpflt-lfo 0.0) ;; hipass filter lfo object
                (amp-lfo 0.0)   ;; amplitude lfo object
                (am-sig 0.0)    ;; amp-mod object
                (rm-sig 0.0)    ;; ring-mod object
                (pm-sig 0.0)    ;; pitch-mod object
                (pm-max 0.594631) ;; 1 semitone

                ;; lowpass constructors
                ;; make the lowpass filter
                (lpflt (make-butterlp 22050))
                ;; make the filter frequency envelope
                (lpflt-frq-env (if lpflt-frq 
                                   ;; Y use the frequency as a scaler
                                   ;; for a basic envelope, so we
                                   ;; don't have to add switches in
                                   ;; code, more simple tho perhaps slower
                                   ;; as envelope is always calculated
                                   (make-env :envelope '(0 1 100 1)
                                             :duration dur :base lp-base
                                             :scaler lpflt-frq)
                                   ;; N use the default or variable envelope 
                                   (make-env :envelope lpflt-frq-env
                                             :duration dur :base lp-base)))
                ;; make the filter lfo (NB. a cosine)
                (lpflt-lfo-osc (make-oscil :frequency 440
                                           :initial-phase (/ pi 2.)))
                ;; make the oscillator frequency envelope
                (lpflt-lfo-frq-env (make-env :envelope lpflt-lfo-frq-env
                                             :duration dur ))
                ;; make the oscillator amount envelope
                (lpflt-lfo-amt-env (make-env :envelope lpflt-lfo-amt-env
                                             :duration dur ))
                ;; hipass constructors
                ;; make the hipass filter
                (hpflt (make-butterhp 1))
                ;; make the filter frequency envelope
                (hpflt-frq-env (if hpflt-frq 
                                   ;; Y use the frequency as a scaler
                                   ;; for a basic envelope, so we
                                   ;; don't have to add switches in
                                   ;; code, more simple tho perhaps slower
                                   ;; as envelope is always calculated
                                   (make-env :envelope '(0 1 100 1)
                                             :duration dur :base hp-base
                                             :scaler hpflt-frq)
                                   ;; N use the default or variable envelope 
                                   (make-env :envelope hpflt-frq-env
                                             :duration dur :base hp-base)))
                ;; make the filter lfo (NB. a cosine)
                (hpflt-lfo-osc (make-oscil :frequency 440
                                           :initial-phase (/ pi 2.)))
                ;; make the oscillator frequency envelope
                (hpflt-lfo-frq-env (make-env :envelope hpflt-lfo-frq-env
                                             :duration dur ))
                ;; make the oscillator amount envelope
                (hpflt-lfo-amt-env (make-env :envelope hpflt-lfo-amt-env
                                             :duration dur ))
                ;; amp constructors
                ;; make the amp lfo (NB. a cosine)
                (amp-lfo-osc (make-oscil :frequency 440
                                         :initial-phase (/ pi 2.)))
                ;; make the oscillator frequency envelope
                (amp-lfo-frq-env (make-env :envelope amp-lfo-frq-env
                                           :duration dur ))
                ;; make the oscillator amount envelope
                (amp-lfo-amt-env (make-env :envelope amp-lfo-amt-env
                                           :duration dur ))

                ;; ring-mod objects
                (rm-osc (make-oscil :frequency 440))
                (rm-amt-env (make-env :envelope rm-amt-env
                                      :scaler rm-amp
                                      :duration dur  :base rm-amt-base))
                (rm-frq-env (make-env :envelope rm-frq-env
                                      :duration dur :base rm-frq-base))

                ;; amp-mod objects
                (am-osc (make-oscil :frequency 440))
                (am-amt-env (make-env :envelope am-amt-env
                                      :scaler am-amp
                                      :duration dur :base am-amt-base))
                (am-frq-env (make-env :envelope am-frq-env
                                      :duration dur :base am-frq-base))

                ;; pitch-mod objects
                (pm-osc (make-oscil :frequency 440))
                (pm-amt-env (make-env :envelope pm-amt-env
                                      :scaler pm-amp
                                      :duration dur ))
                (pm-frq-env (make-env :envelope pm-frq-env
                                      :duration dur ))

                ;; THE END

                ;; The output file end sample
                (nd (+ st (floor (* *srate* dur)))))
           (when printing (format t "~&Start time ~a~%" time))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           ;; End of the setup code, beginning of the run loop (sample
           ;; generation) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

           (run ;; This is the part that CLM translates into C, for speed
            (loop for i from st to nd do ;; from output file start sample to end
                 (when printing ;; print out each second we've computed
                   (if (= count *srate*)
                       (progn
                         (setq count 1)
                         (clm-print "~%~d secs" (round (/ i *srate*))))
                       (incf count)))
               ;; get the current sample
                 (setf ml (mus-location src-gen))
               ;; if we're past the last input sample start going backwards.
                 (when  (>= ml end-sample) 
                   (setf (mus-increment src-gen) (- srt)))
               ;; similarly, if we're before the first input sample, start going
               ;; forwards. 
                 (when (<= ml start-sample)
                   (setf (mus-increment src-gen) srt))
               ;; if we're reverbing, set the right reverb amount from the
               ;; rev-env 
                 (when *reverb* 
                   (setf (locsig-reverb-ref loc chan) (env renv)))

               ;; NEW FUNCTIONALITY ADDED BY JULES RAWLINSON - STILL 2 ADD
               ;; AMPLFO 

               ;; a detailed decription of lfo-ing
               ;; update the lowpass lfo osc freq
                 (setf (mus-frequency lpflt-lfo-osc) (env lpflt-lfo-frq-env))
               ;; update lfo amount
                 (setf lpflt-lfo (* (oscil lpflt-lfo-osc)
                                    (env lpflt-lfo-amt-env)))
               ;; get absolute value of lfo to ensure
               ;; that amount is always positive.
                 (setf lpflt-lfo (abs lpflt-lfo))
               ;; then the science bit...
               ;; -- in the case of lfo amount is zero...
               ;; as we don't want to multiply our input by zero we subtract 1 
               ;; from the output and again return only a positive value
               ;; to ensure that phase of attenuated signal is not inverted.
               ;; -- i.e. because our lfo is a cosine initial value is 1.
               ;; 1 (the lfo) * 0 (the lfo amount) = 0 
               ;; > 0 - 1 = -1 > abs 1 * our input = unchanged input
               ;; -- in the case of 100% lfo amount...
               ;; 1 * 1 = 1 > 1 - 1 = 0 > abs 0 * input = 0 output (if amp lfo)
                 (setf lpflt-lfo (abs (- lpflt-lfo 1.)))
               ;; finally update freq from freq-env * lfo output
                 (set-butterlp lpflt (* (env lpflt-frq-env) lpflt-lfo))

               ;; do it all again for the hipass filter
                 (setf (mus-frequency hpflt-lfo-osc) (env hpflt-lfo-frq-env)
                       hpflt-lfo (* (oscil hpflt-lfo-osc)
                                    (env hpflt-lfo-amt-env))
                       hpflt-lfo (abs hpflt-lfo)
                       hpflt-lfo (abs (- hpflt-lfo 1.)))
                 (set-butterhp hpflt (* (env hpflt-frq-env) hpflt-lfo))

               ;; now do it all again for the amplitude lfo
                 (setf (mus-frequency amp-lfo-osc)(env amp-lfo-frq-env)
                       amp-lfo (* (oscil amp-lfo-osc) (env amp-lfo-amt-env))
                       amp-lfo (abs amp-lfo)
                       amp-lfo (abs (- amp-lfo 1.)))

               ;; pitch-mod stage
                 (setf (mus-frequency pm-osc)(env pm-frq-env)
                       pm-sig (* (* (oscil pm-osc) pm-max) (env pm-amt-env)))

               ;; calculate our output sample
                 (setf out-sig (src src-gen (+ (env senv) pm-sig)))

               ;; amp-mod stage
                 (setf (mus-frequency am-osc)(env am-frq-env)
                       am-sig (* (oscil am-osc) (env am-amt-env))
                       am-sig (+ am-sig (- 1 (env am-amt-env)))
                       out-sig (* out-sig am-sig))

               ;; ring-mod stage
                 (setf (mus-frequency rm-osc)(env rm-frq-env)
                       rm-sig (* (oscil rm-osc) (env rm-amt-env))
                       rm-sig (- 1 rm-sig)
                       out-sig (* out-sig rm-sig))

               ;; scaling stage

               ;; waveshaping
                 (if wv-shp-amt
                     (setf out-sig (/ (* out-sig (+ (abs out-sig) wv-shp-amt)) 
                                      (+ (expt out-sig 2) 
                                         (* 
                                          (- wv-shp-amt 1) (abs out-sig)) 1))))
           
               ;; are we graining by hanning
                 (if hanning
                     ;; Y - scale via hanning function
                     (setf out-sig 
                           (* out-sig
                              (* 0.5 (- 1.0 (cos (/ (* (* pi 2) i) 
                                                    (- (floor (* *srate* dur))
                                                       1)))))))
                     ;; N - scale it by the amp-env
                     (setf out-sig (* out-sig (env ampf)))
                     )

               ;; scale the output sample by the amp lfo
                 (setf out-sig (* out-sig amp-lfo))
               ;; filter stage
               ;; lowpass filter the output
                 (setf out-sig (butterlp lpflt out-sig))
               ;; lopass second stage
                 (if lp-two 
                     (setf out-sig (butterlp lpflt out-sig)))
               ;; hipass filter the output
                 (setf out-sig (butterhp hpflt out-sig))
               ;; hipass second stage
                 (if hp-two 
                     (setf out-sig (butterhp hpflt out-sig)))
               ;; output in stereo space
                 (if mono-in
                     (locsig loc i out-sig)
                     (out-any i out-sig chan))
               ;; END NEW FUNCTIONS
                 ))
           ;; close our input file
           (close-input f)))))

    ;;;;;;;;;;;;;;;;;;;;;;;;
    ;; End of definistrument
    ;;;;;;;;;;;;;;;;;;;;;;;;

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
	      :output-name-uniquifier "samp2-"
	      :clm-ins #'clm::samp2
	      :clm-ins-args '(:lpflt-frq 60
			      :lpflt-frq-env (0 200 50 250 100 200)
			      :lpflt-lfo-frq-env (0 100 100 100)
			      :lpflt-lfo-amt-env (0 .2 50 .8 100 .2)
			      :srt-env (0 0 50 1 100 -1)
			      :rev-env (0 0 70 1 100 1)))
|#
