;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File:    copy-bars-sc2sc.lsp
;;;
;;; Author:  Daniel James Ross (mr.danielross@gmail.com)
;;;
;;; Date:    Mon Feb 22 12:00:04 GMT 2016
;;;
;;; Purpose: A hacked version of copy-bars.lsp
;;;          This method takes the bars of one sc object
;;;          and adds them to another.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod copy-bars-sc2sc
    ((sc1 slippery-chicken)	; the original sc object
     from-start-bar	        ; first bar to copy bars from
     (sc2 slippery-chicken)     ; the second sc object
     to-start-bar	        ; first bar copied notes go to
     from-player                ; copy player
     to-player		        ; copy-to player
     num-bars	             	;how many bars to copy in total
     &optional (print-bar-nums nil))
;;; **** 
  (flet ((no-bar (bar-num player)
           (error "slippery-chicken-edit::copy-bars: 
Can't get bar number ~a ~
                   for ~a." bar-num player)))
    (let* ((from-bar (get-bar sc1 from-start-bar from-player))
           (to-bar (get-bar sc2 to-start-bar to-player))
           from-plays-transp
           to-plays-transp
           to-transp)
      (unless from-bar
        (no-bar from-start-bar from-player))
      (unless to-bar
        (no-bar to-start-bar to-player))
      (setf from-plays-transp
	    (plays-transposing-instrument sc1 from-player nil)
            to-plays-transp
	    (plays-transposing-instrument sc2 to-player nil)
            from-bar (clone from-bar))
      (when print-bar-nums
        (format t "~%from ~a to ~a"
		from-plays-transp to-plays-transp))
      (unless num-bars
        (setf num-bars (- (num-bars sc2) (bar-num to-bar) -1)))
      (loop for fbnum from (bar-num from-bar)
         for tbnum from (bar-num to-bar)
         with first-time = t
         with player-section
         with sequenz
         repeat num-bars
         do
       ;; MDE Fri Jun 15 13:59:37 2012 
         (when to-plays-transp
           (setf to-transp
		 (get-transposition-at-bar to-player tbnum sc2)))
         (unless first-time
           (setf from-bar (get-bar sc1 fbnum from-player)
                 to-bar (get-bar sc2 tbnum to-player)))
         (unless from-bar
           (no-bar fbnum from-player))
         (unless to-bar
           (no-bar tbnum to-player))
         (setf from-bar (clone from-bar))
	 ;; MDE Fri Jun 15 14:05:04 2012 -- in case we're
	 ;; copying from a transposing to a non-transposing ins 
         (when (and from-plays-transp (not to-transp))
           (delete-written from-bar))
         (when print-bar-nums
           (format t "~%from ~a to ~a to-transp ~a"
		   from-plays-transp
                   to-plays-transp to-transp))
         ;; MDE Fri Jun 15 13:19:27 2012 -- in case we're
	 ;; copying from a non-transposing to a transposing
	 ;; instrument. 
         (when to-transp
           (set-written from-bar (- to-transp)))
         (setf first-time nil)
         (when print-bar-nums
           (format t "~&Copying bar ~a to bar ~a" fbnum tbnum))
         (unless (eq t (time-sig-equal (get-time-sig from-bar)
                                       (get-time-sig to-bar)))
           (error "piece::copy-bars: Can't replace bars with 
different time ~ signatures: ~a ~a to ~a ~a"
                  from-player fbnum to-player tbnum))
         ;; copy data that should remain the same
	 ;; into the bar we're going to replace with                  
         (setf (write-bar-num from-bar) (write-bar-num to-bar)
               (start-time from-bar) (start-time to-bar)
               (bar-line-type from-bar) (bar-line-type to-bar)
               (write-time-sig from-bar) (write-time-sig to-bar)
               (player-section-ref from-bar)
	       (player-section-ref to-bar)
               (nth-seq from-bar) (nth-seq to-bar)
               (nth-bar from-bar) (nth-bar to-bar))
	   (setf player-section
		 (get-data (player-section-ref to-bar) (piece sc2))
		 sequenz (get-nth (nth-seq to-bar) player-section))
	   (set-nth-bar (nth-bar to-bar) from-bar sequenz))))
  t)
#|

(load "/path/to/folder/copy-bars-sc2sc.lsp") ; Change this

(let* ((sc-one 
	(make-slippery-chicken  
	 '+sc-one+ 
	 :title "Your Title Here" 
	 :composer "Your Name Here"
	 :ensemble '(((flt (flute :midi-channel 1))))
	 :staff-groupings '(1)
	 :tempo-map '((1 (q 60)))
	 :set-palette '((set1 ((fs2 b2 d4 a4 d5 e5 a5 d6))) 
			(set2 ((b2 fs3 d4 e4 a4 d5 e5 a5 d6))))
	 :set-map '((1 (set1 set1 set2 set1 set1 set2)))
	 :rthm-seq-palette
	 '((seq1 ((((4 4) (w))))))
	 :rthm-seq-map
	 '((1 ((flt (seq1 seq1 seq1 seq1 seq1 seq1)))))))

       (sc-two 
	(make-slippery-chicken  
	 '+sc-two+ 
	 :title "Your Title Here" 
	 :composer "Your Name Here"
	 :ensemble '(((flt (flute :midi-channel 1))))
	 :staff-groupings '(1)
	 :tempo-map '((1 (q 60)))
	 :set-palette '((set1 ((fs2 b2 d4 a4 d5 e5 a5 d6))) 
			(set2 ((b2 fs3 d4 e4 a4 d5 e5 a5 d6))))
	 :set-map '((1 (set1 set1 set2 set1 set1 set2)))
	 :rthm-seq-palette
	 '((seq1 ((((4 4) q q q q))   
		  :pitch-seq-palette (1 2 3 4))))
	 :rthm-seq-map
	 '((1 ((flt (seq1 seq1 seq1 seq1 seq1 seq1))))))))
      
      
      ;; Now we copy bar 1 from sc-two to bar 1 of sc-one ;
  (copy-bars-sc2sc sc-two 1 sc-one 1 'flt 'flt 1)

  (cmn-display sc-one))

|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF copy-bars-sc2sc.lsp
