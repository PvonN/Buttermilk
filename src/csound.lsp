(in-package :bm)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; make-spectr-arr
;;; create a array from the spectrum analysis of a soundfile
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fillarray
;;; fillarray creates a file that can be included in a csound programm
;;; which holds an csound array
;;; - use '#includ FILE' inside of Csound to use the array
;;; ! -> this way to create a array is very limited to the length of
;;; the array; for bigger data collections use 'copyf2array'
(defun fillarray (name lst path)
  (with-open-file (orc path :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
    (format orc "~a[] fillarray ~{~f~^,~}~%" name lst)))
#|
(fillarray "iArray" '(1 2 3 2 3 4 3 2 1 2) "~/Desktop/test.txt")
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; copyf2array
;;; create a list which will be read into a table and then copied to a
;;; k-array
;;; - uses GEN23
;;; - this way of creating a array is nearly unlimited to the amount
;;; of data
;;; - use '#includ PATH-FILE' inside of csound to use the array
(defun copyf2array (name lst path-file path-data)
  (with-open-file (orc path-file :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
    (format orc "i~a ftgen 0,0,0,-23,~s~%k~a[] init tableng:i(i~a)~%copyf2array k~a,i~a~%" name path-data name name name
  name))
  (with-open-file (orc path-data :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
    (format orc "~{~f~^,~}~%" lst)))
#|
(copyf2array "Data" 
	     '(1 2 3 4 5 6)		 
	     "/Users/philippneumann/Desktop/file-to-csound.csd" 
	     "/Users/philippneumann/Desktop/datafile.txt")
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; list-to-gen23
;;; creates a .txt file which can be used with GEN23
;;; - the input data is seperated by ,
(defun list-to-gen23 (lst path)
  (with-open-file (orc path :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
    (format orc "~{~f~^,~}~%" lst)))
#|
(list-to-gen23 '(1 2 3 4 4) "/Users/philippneumann/Desktop/file.txt")
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; linseg-opcode 
;;; takes a nested list in this form ((dur val) (dur val) (...)) and
;;; creates a textfile to be used inside csound as a repleacement for
;;; the linseg opcode
;;; - needs a start value to begin with
(defun seg-op-h (segments)
  (cond ((null segments) nil)
	(t (append (car segments)
		   (linseg-op-h (cdr segments))))))
(defun linseg-opcode (name start segments path)
  (with-open-file (orc path :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
    (format orc "~a linseg ~{~f~^,~}~%" name
	    (append (list start) (seg-op-h segments)))))
#|
(linseg-opcode "aLine" 0 '((10 5) (2 1) (6 9)) "/Users/philippneumann/Desktop/test.csd")
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; expseg-opcode 
;;; takes a nested list in this form ((dur val) (dur val) (...)) and
;;; creates a textfile to be used inside csound as a repleacement for
;;; the expseg opcode
;;; - needs a start value to begin with
(defun expseg-opcode (name start segments path)
  (with-open-file (orc path :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
    (format orc "~a expseg ~{~f~^,~}~%" name
	    (append (list start) (seg-op-h segments)))))
#|
(expseg-opcode "aLine" 0 '((10 5) (2 1) (6 9)) "/Users/philippneumann/Desktop/test.csd")
|#

