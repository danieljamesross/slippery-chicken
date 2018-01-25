; SLIME 2.20
CL-USER> (sc)
(\  }\   
(  \_('> slippery chicken 1.0.8
(__(=_)  
   -"=   
T
SC> (let* ((mini
	(make-slippery-chicken
	 '+mini+
	 :ensemble '(((vn (violin :midi-channel 1))))
	 :set-palette '((1 ((c4 e4 g4))))
	 :set-map '((1 (1)))
	 :rthm-seq-palette '((1 ((((2 4) q e e))
				 :pitch-seq-palette ((1 2 3)))))
	 :rthm-seq-map '((1 ((vn (1))))))))
  (mak-chord-auto mini 1 1 'vn :chord-selection-fun #'violin-chord-selection-fun)
  (cmn-display mini))

; in:
;      LET* ((MINI
;         (MAKE-SLIPPERY-CHICKEN '+MINI+ :ENSEMBLE '((#)) :SET-PALETTE '((1 #))
;                                :SET-MAP '((1 #)) :RTHM-SEQ-PALETTE '((1 #))
;                                :RTHM-SEQ-MAP '((1 #)))))
;     (SLIPPERY-CHICKEN::MAK-CHORD-AUTO SLIPPERY-CHICKEN::MINI 1 1
;      'SLIPPERY-CHICKEN::VN :CHORD-SELECTION-FUN
;      #'SLIPPERY-CHICKEN:VIOLIN-CHORD-SELECTION-FUN)
; 
; caught STYLE-WARNING:
;   undefined function: MAK-CHORD-AUTO
; 
; compilation unit finished
;   Undefined function:
;     MAK-CHORD-AUTO
;   caught 1 STYLE-WARNING condition
******* section (1)
Getting notes for VN
WARNING:
   slippery-chicken::tempo-curve-to-map: No tempo-map or tempo-curve given. 
Using default of crotchet/quarter = 60.
Shortening short, fast leaps...
Shortened 0 large fast leaps; Evaluation aborted on #<UNDEFINED-FUNCTION MAK-CHORD-AUTO {10084D0283}>.
SC> 
;;; from make-chord-auto.lsp
SC> (make-chord-auto mini 1 1 'vn :chord-selection-fun #'violin-chord-selection-fun)
; Evaluation aborted on #<UNBOUND-VARIABLE MINI {1001D6BDE3}>.
SC> 
;;; from make-chord-auto.lsp
SC> (let* ((mini
	(make-slippery-chicken
	 '+mini+
	 :ensemble '(((vn (violin :midi-channel 1))))
	 :set-palette '((1 ((c4 e4 g4))))
	 :set-map '((1 (1)))
	 :rthm-seq-palette '((1 ((((2 4) q e e))
				 :pitch-seq-palette ((1 2 3)))))
	 :rthm-seq-map '((1 ((vn (1))))))))
  (make-chord-auto mini 1 1 'vn :chord-selection-fun #'violin-chord-selection-fun)
  (cmn-display mini))
; in:
                            ;      LET* ((MINI
                            ;         (MAKE-SLIPPERY-CHICKEN '+MINI+ :ENSEMBLE '((#)) :SET-PALETTE '((1 #))
                            ;                                :SET-MAP '((1 #)) :RTHM-SEQ-PALETTE '((1 #))
                            ;                                :RTHM-SEQ-MAP '((1 #)))))
;     (SLIPPERY-CHICKEN::MAKE-CHORD-AUTO SLIPPERY-CHICKEN::MINI 1 1
;                                        'SLIPPERY-CHICKEN::VN :CHORD-SELECTION-FUN
;                                        #'SLIPPERY-CHICKEN:VIOLIN-CHORD-SELECTION-FUN)
; 
; caught STYLE-WARNING:
;   :CHORD-SELECTION-FUN is not a known argument keyword.
; 
; compilation unit finished
;   caught 1 STYLE-WARNING condition
******* section (1)
Getting notes for VN
WARNING:
   slippery-chicken::tempo-curve-to-map: No tempo-map or tempo-curve given. 
Using default of crotchet/quarter = 60.
Shortening short, fast leaps...
Shortened 0 large fast leaps; Evaluation aborted on #<SB-INT:SIMPLE-PROGRAM-ERROR "~@<invalid keyword argument~P: ~
                                   ~{~S~^, ~} (valid keys are ~{~S~^, ~}).~@:>" {1003904E93}>.
SC> 
;;; from make-chord-auto.lsp
SC> (let* ((mini
	(make-slippery-chicken
	 '+mini+
	 :ensemble '(((vn (violin :midi-channel 1))))
	 :set-palette '((1 ((c4 e4 g4))))
	 :set-map '((1 (1)))
	 :rthm-seq-palette '((1 ((((2 4) q e e))
				 :pitch-seq-palette ((1 2 3)))))
	 :rthm-seq-map '((1 ((vn (1))))))))
  (make-chord-auto mini 1 1 'vn :chord-fun #'violin-chord-selection-fun)
  (cmn-display mini))

******* section (1)
Getting notes for VN
WARNING:
   slippery-chicken::tempo-curve-to-map: No tempo-map or tempo-curve given. 
Using default of crotchet/quarter = 60.
Shortening short, fast leaps...
Shortened 0 large fast leaps
Respelling notes...
Inserting automatic clefs....
Generating VN...
Inserting line breaks...
Creating systems...
Calling CMN...
T
SC> 
;;; from make-chord-auto.lsp
SC> (let* ((mini
	(make-slippery-chicken
	 '+mini+
	 :ensemble '(((vn (violin :midi-channel 1))))
	 :set-palette '((1 ((c4 e4 g4))))
	 :set-map '((1 (1)))
	 :rthm-seq-palette '((1 ((((2 4) q e e))
				 :pitch-seq-palette ((1 2 3)))))
	 :rthm-seq-map '((1 ((vn (1))))))))
  (make-chord-auto mini 1 1 'vn :chord-fun #'violin-chord-selection-fun :print t)
  (cmn-display mini))
******* section (1)
Getting notes for VN
WARNING:
   slippery-chicken::tempo-curve-to-map: No tempo-map or tempo-curve given. 
Using default of crotchet/quarter = 60.
Shortening short, fast leaps...
Shortened 0 large fast leaps
make-chord-auto, ins: VN bar: 1, ev: 1, new-pitches: 
CHORD: auto-sort: T, marks: NIL, micro-tone: NIL, micro-tonality: 0.0
centroid: NIL, dissonance: NIL
SCLIST: sclist-length: 2, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: NIL, tag: NIL, 
data: (
PITCH: frequency: 261.626, midi-note: 60, midi-channel: 1 
       pitch-bend: 0.0 
       degree: 120, data-consistent: T, white-note: C4
       nearest-chromatic: C4
       src: 1.0, src-ref-pitch: C4, score-note: C4 
       qtr-sharp: NIL, qtr-flat: NIL, qtr-tone: NIL,  
       micro-tone: NIL, 
       sharp: NIL, flat: NIL, natural: T, 
       octave: 4, c5ths: 0, no-8ve: C, no-8ve-no-acc: C
       show-accidental: T, white-degree: 35, 
       accidental: N, 
       accidental-in-parentheses: NIL, marks: NIL, 
       marks-before: NIL
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: C4, tag: NIL, 
data: C4
**************

       
PITCH: frequency: 329.628, midi-note: 64, midi-channel: 1 
       pitch-bend: 0.0 
       degree: 128, data-consistent: T, white-note: E4
       nearest-chromatic: E4
       src: 1.2599211, src-ref-pitch: C4, score-note: E4 
       qtr-sharp: NIL, qtr-flat: NIL, qtr-tone: NIL,  
       micro-tone: NIL, 
       sharp: NIL, flat: NIL, natural: T, 
       octave: 4, c5ths: 0, no-8ve: E, no-8ve-no-acc: E
       show-accidental: T, white-degree: 37, 
       accidental: N, 
       accidental-in-parentheses: NIL, marks: NIL, 
       marks-before: NIL
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: E4, tag: NIL, 
data: E4
**************
)
**************
Respelling notes...
Inserting automatic clefs....
Generating VN...
Inserting line breaks...
Creating systems...
Calling CMN...
T
SC> 
;;; from make-chord-auto.lsp
SC> (let* ((mini
	(make-slippery-chicken
	 '+mini+
	 :ensemble '(((vn (violin :midi-channel 1))))
	 :set-palette '((1 ((c4 e4 g4))))
	 :set-map '((1 (1)))
	 :rthm-seq-palette '((1 ((((2 4) q e e))
				 :pitch-seq-palette ((1 2 3)))))
	 :rthm-seq-map '((1 ((vn (1))))))))
  (make-chord-auto mini 1 1 'vn :chord-fun #'violin-chord-selection-fun :print t)
  (cmn-display mini))
******* section (1)
Getting notes for VN
WARNING:
   slippery-chicken::tempo-curve-to-map: No tempo-map or tempo-curve given. 
Using default of crotchet/quarter = 60.
Shortening short, fast leaps...
Shortened 0 large fast leapsC4 E4 
make-chord-auto:
ins: VN
bar: 1
ev: 1
new-pitches: NIL
Respelling notes...
Inserting automatic clefs....
Generating VN...
Inserting line breaks...
Creating systems...
Calling CMN...
T
SC> (let* ((mini
	(make-slippery-chicken
	 '+mini+
	 :ensemble '(((vn (violin :midi-channel 1))))
	 :set-palette '((1 ((c4 e4 g4))))
	 :set-map '((1 (1)))
	 :rthm-seq-palette '((1 ((((2 4) q e e))
				 :pitch-seq-palette ((1 2 3)))))
	 :rthm-seq-map '((1 ((vn (1))))))))
  (make-chord-auto mini 1 1 'vn :chord-fun #'violin-chord-selection-fun :print t)
  (cmn-display mini))
******* section (1)
Getting notes for VN
WARNING:
   slippery-chicken::tempo-curve-to-map: No tempo-map or tempo-curve given. 
Using default of crotchet/quarter = 60.
Shortening short, fast leaps...
Shortened 0 large fast leaps
make-chord-auto:
ins: VN
bar: 1
ev: 1
new-pitches: (
PITCH: frequency: 261.626, midi-note: 60, midi-channel: 1 
       pitch-bend: 0.0 
       degree: 120, data-consistent: T, white-note: C4
       nearest-chromatic: C4
       src: 1.0, src-ref-pitch: C4, score-note: C4 
       qtr-sharp: NIL, qtr-flat: NIL, qtr-tone: NIL,  
       micro-tone: NIL, 
       sharp: NIL, flat: NIL, natural: T, 
       octave: 4, c5ths: 0, no-8ve: C, no-8ve-no-acc: C
       show-accidental: T, white-degree: 35, 
       accidental: N, 
       accidental-in-parentheses: NIL, marks: NIL, 
       marks-before: NIL
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: C4, tag: NIL, 
data: C4
**************

              
PITCH: frequency: 329.628, midi-note: 64, midi-channel: 1 
       pitch-bend: 0.0 
       degree: 128, data-consistent: T, white-note: E4
       nearest-chromatic: E4
       src: 1.2599211, src-ref-pitch: C4, score-note: E4 
       qtr-sharp: NIL, qtr-flat: NIL, qtr-tone: NIL,  
       micro-tone: NIL, 
       sharp: NIL, flat: NIL, natural: T, 
       octave: 4, c5ths: 0, no-8ve: E, no-8ve-no-acc: E
       show-accidental: T, white-degree: 37, 
       accidental: N, 
       accidental-in-parentheses: NIL, marks: NIL, 
       marks-before: NIL
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: E4, tag: NIL, 
data: E4
**************
)
Respelling notes...
Inserting automatic clefs....
Generating VN...
Inserting line breaks...
Creating systems...
Calling CMN...
T
SC> (let* ((mini
	(make-slippery-chicken
	 '+mini+
	 :ensemble '(((vn (violin :midi-channel 1))))
	 :set-palette '((1 ((c4 e4 g4))))
	 :set-map '((1 (1)))
	 :rthm-seq-palette '((1 ((((2 4) q e e))
				 :pitch-seq-palette ((1 2 3)))))
	 :rthm-seq-map '((1 ((vn (1))))))))
  (make-chord-auto mini 1 1 'vn :chord-fun #'violin-chord-selection-fun :print t))
******* section (1)
Getting notes for VN
WARNING:
   slippery-chicken::tempo-curve-to-map: No tempo-map or tempo-curve given. 
Using default of crotchet/quarter = 60.
Shortening short, fast leaps...
Shortened 0 large fast leaps
make-chord-auto:
ins: VN
bar: 1
ev: 1
new-pitches: (C4 E4)
T
SC> (let* ((mini
	(make-slippery-chicken
	 '+mini+
	 :ensemble '(((vn (violin :midi-channel 1))))
	 :set-palette '((1 ((c4 e4 g4))))
	 :set-map '((1 (1)))
	 :rthm-seq-palette '((1 ((((2 4) q e e))
				 :pitch-seq-palette ((1 2 3)))))
	 :rthm-seq-map '((1 ((vn (1))))))))
  (make-chord-auto mini 2 1 'vn :chord-fun #'violin-chord-selection-fun :print t))
******* section (1)
Getting notes for VN
WARNING:
   slippery-chicken::tempo-curve-to-map: No tempo-map or tempo-curve given. 
Using default of crotchet/quarter = 60.
Shortening short, fast leaps...
Shortened 0 large fast leaps
WARNING: bar-holder::get-event: couldn't get bar number 2 for player VN
; Evaluation aborted on #<SB-PCL::NO-APPLICABLE-METHOD-ERROR {1003F422C3}>.
SC> (let* ((mini
	(make-slippery-chicken
	 '+mini+
	 :ensemble '(((vn (violin :midi-channel 1))))
	 :set-palette '((1 ((c4 e4 g4))))
	 :set-map '((1 (1)))
	 :rthm-seq-palette '((1 ((((2 4) q e e))
				 :pitch-seq-palette ((1 2 3)))))
	 :rthm-seq-map '((1 ((vn (1))))))))
  (make-chord-auto mini 1 2 'vn :chord-fun #'violin-chord-selection-fun :print t))
******* section (1)
Getting notes for VN
WARNING:
   slippery-chicken::tempo-curve-to-map: No tempo-map or tempo-curve given. 
Using default of crotchet/quarter = 60.
Shortening short, fast leaps...
Shortened 0 large fast leaps
make-chord-auto:
ins: VN
bar: 1
ev: 2
new-pitches: (E4 G4)
T
SC> (let* ((mini
	(make-slippery-chicken
	 '+mini+
	 :ensemble '(((vn (violin :midi-channel 1))))
	 :set-palette '((1 ((c4 e4 g4))))
	 :set-map '((1 (1)))
	 :rthm-seq-palette '((1 ((((2 4) q e e))
				 :pitch-seq-palette ((1 2 3)))))
	 :rthm-seq-map '((1 ((vn (1))))))))
  (make-chord-auto mini 1 2 'vn :chord-fun #'violin-chord-selection-fun :print t))
******* section (1)
Getting notes for VN
WARNING:
   slippery-chicken::tempo-curve-to-map: No tempo-map or tempo-curve given. 
Using default of crotchet/quarter = 60.
Shortening short, fast leaps...
Shortened 0 large fast leaps
make-chord-auto:
ins: VN
bar: 1
ev: 2
new-pitches: (E4 G4)
T
SC> 
;;; from mypop.lsp
SC> (pop-env)
(0 1 11 3 22 2 33 2 44 4 55 6 66 3 77 6 88 6 99 10)
SC> 
