(in-package :bm)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; make-spectr-arr
;;; create a array out of the spectrum analysis from a soundfile
;;; frequency and amplitude values are interleaved
;;; you can #include this file in your csound instrument e.g. for
;;; using with the spectr_synth UDO
(defun make-spectr-arr (name file path &key (num-partials 10) (start-analysis 0.0))
  (with-open-file (orc path :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
    (format orc "~a[] fillarray ~{~5,2f,~1,4f~^,~}~%" name
	    (multiple-value-bind
		  (freqs amps)
		(clm::get-spectrum file
				   :num-partials num-partials
				   :start-analysis start-analysis)
	      (sc::interleave freqs amps)))))
#|
(make-spectr-arr "kSpectrArr1"
		 "/Users/philippneumann/Desktop/test.wav"
		 "/Users/philippneumann/Desktop/fileForCsound.txt"
		 :num-partials 5
		 :start-analysis 0.75)
==>
kSpectrArr1[] fillarray 1324.77,1.0000,1107.70,.1419,3966.24,.1229,120.99,.0899,95.84,.0856
)|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; make-freq-array
;;; create file with a array out of the frequency analysis from a soundfile
;;; the file can be used with #include in your csound instrument
;;; e.g. to be used with the spectr_synth UDO
(defun make-freq-array (name file path &key (num-partials 10) (start-analysis 0.0))
  (with-open-file (orc path :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
    (format orc "~a[] fillarray ~{~5,2f~^,~}~%" name
	    (multiple-value-bind
		  (freqs amps)
		(clm::get-spectrum
		 file :num-partials num-partials
		 :start-analysis start-analysis)
	      freqs))))
#|
(make-freq-array "kFreqArr"
		 "/Users/philippneumann/Desktop/test.wav"
		 "/Users/philippneumann/Desktop/flForCsound.txt"
		 :num-partials 10
		 :start-analysis 0.75)
===>
kFreqArr[] fillarray 1324.77,1107.70,3966.24,120.99,95.84,147.33,2642.59,50.65,8364.85,8388.68
|#


