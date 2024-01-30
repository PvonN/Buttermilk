(in-package :bm)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; list functions
;;;
;; seperates a list into two lists and gives it out as a nested list
;; in this form ((odd-indeces) (even-indeces))
(defun deinterleave (lst)
  (loop for i from 1 to (length lst)
	for e in lst
	when (oddp i) collect e into lst-1
	  when (evenp i) collect e into lst-2
	    finally (return (list lst-1 lst-2))))
;;(deinterleave '(a b c d e f g)) => ((A C E G) (B D F))

;;; gives back the last value of a list
(defun last-value (lst)
  (car (reverse lst)))
;;; fills a list of size N with value X
(defun fill-list (size val)
  (loop repeat size
	collect val))
;;; creates random values between two numbers
(defun between (mini maxi)
  (+ mini (random (- maxi mini))))
;; deviate the values of a list by a fixed percentage, while 100 means
;; the a value of the list could be doubled or divided by 2
;; (deviate-list '(1 2 3 4 5) 25)
;; (deviate-list '(1 2 3 4 5) 25) => (1.245879 2.3614287 2.6611075 4.459944 5.2167025)
(defun deviate-list (lst percent)
  (let* ((deviation-amount (/ percent 100.0)))
    (loop for i in lst
	  collect (* i (between
			(abs (1- (* deviation-amount 0.5)))
			(1+ deviation-amount))))))

(defun list-without-first-and-last (lst)
  "give back the list without the first and last element"
  (reverse (rest (reverse (rest lst)))))

   
(defun swap-first-last (lst)
  "swap first and last element of a list"
  (append (last lst) (list-without-first-and-last lst)
	  (list (first lst))))

(defun list-up-to-index (lst index)
  "return list up to index"
  (if (> index (1- (length lst)))
      (error "index is bigger then length of list")
      (loop for item in lst
	    for i from 0 to index
	    collect item)))

(defun list-from-i-to-j (lst i j)
  "return the list from index-i to index-j"
  (loop for item in lst
	for index from 0 to (length lst)
	when (and (<= index j) (>= index i))
	  collect item))

(defun list-without-last (lst)
  "return list without last element"
  (loop for item in lst
	for i from 0 to (- (length lst) 2)
	collect item))

(defun list-without (lst index)
  "returns list without the element with given index"
  (append (list-up-to-index lst (1- index))
	  (list-from-i-to-j
	   lst (1+ index) (length lst))))

(defun crop-list (lst leng)
  "shortens a list to a given length"
  (let* ((index (1+ (random (length lst)))))
    (cond
      ((eq (length lst) leng) lst)    
      ((eq (nth index lst)
	   (nth (1+ index) lst))
       (crop-list (list-without lst index) leng))    
      ((eq (nth index lst)
	   (nth (1- index) lst))
       (crop-list (list-without lst index) leng))
      (t (crop-list (list-without-last lst) leng)))))

(defun swap-elements-of-index (lst i j)
  "swap elements of index-i  and index-j"
  (loop for item in lst
	for index from 0 to (length lst)
	when (= index i)
	  collect item into first-element
	when (= index j)
	  collect item into second-element
	when (< index i)
	  collect item into first-list
	when (and (> index i) (< index j))
	  collect item into second-list
	when (> index j)
	  collect item into last-list
	finally (return
		  (append first-list
			  second-element
			  second-list
			  first-element
			  last-list))))

(defun fill-list-with-elements (&rest elem-num-lst)
  "fills a list with a specific number of elements '(e 7) '(a 17)"
  (let* ((result '()))
    (dolist (lst elem-num-lst)
      (destructuring-bind (elem num) lst
	(dotimes (i num)
	  (push elem result))))
    (reverse result)))

(defun repeat-lst (rep lst)
  (loop repeat rep
	collect lst into new-lst
	finally (return (almost-flatten new-lst))))
;(repeat-lst 5 '(1 2 3))

(defun shorten-list-to
    (lst to-index &key (stepsize 1) (repetitions 1) (max-leng 100))
  (if (> to-index (1- (length lst)))
      (error "given index is bigger then lst") 
      (loop for i downfrom (1- (length lst)) by stepsize to to-index
	    collect (repeat-lst repetitions (list-up-to-index lst i))
	      into new-lst
	    until (>= (length (flatten new-lst)) max-leng)
	    finally (return (flatten new-lst)))))
;;(shorten-list-to '(1 2 3 4 5 6 7 8 9 10) 3 :by 2 :repetitions 2)

(defun lengthen-list-from
    (lst from-index &key (stepsize 1) (repetitions 1) (max-leng 100))
  (if (> from-index (1- (length lst)))
      (error "given index is bigger then lst") 
      (loop for i from from-index by stepsize to (1- (length lst))
	    collect (repeat-lst repetitions (list-up-to-index lst i))
	      into new-lst
	    until (>= (length (flatten new-lst)) max-leng)
	    finally (return (flatten new-lst)))))
;;(lengthen-list-from '(1 2 3 4 5 6 7 8 9 10) 9 :by 5 :repetitions 4)


(defun make-segments (lst size)
  (cond ((null lst) nil)
	((< (length lst) size)
	 (list lst))
	(t
	 (loop for i from 0 to (1- size)
	       for e in lst
	       collect e into segment
	       finally
		  (return (cons segment
				(make-segments (nthcdr size lst) size)))))))
;;(make-segments '(1 2 3 4 5 6 7 8 9 10) 6)

(defun my-quick-sort (lst)
  "quick-sort algorithm"
  (let* ((pivot-index (1- (length lst))))
	 (cond ((null lst) nil)
	       ((= pivot-index -1) nil)
	       (t (loop for item in lst
			when (< item (nth pivot-index lst))
			  collect item into left-list
			when (> item (nth pivot-index lst))
			  collect item into right-list
			when (= item (nth pivot-index lst))
			  collect item into middle-list
			finally (return (append 
					 (my-quick-sort left-list)
					 middle-list
					 (my-quick-sort right-list))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; creates a list of values
;; draws a line between two values by n steps
;;(draw-line 10 90 270) => (90.0 110.0 130.0 150.0 170.0 190.0 210.0 230.0 250.0 270)
(defun line (start steps aim)
  (let* ((distance (abs (- aim start)))
	 (stepsize (/ distance (1- steps))))
    (cond ((< start aim)
	   (loop for i from start to aim by stepsize
	      collect (float i) into new-lst
	      until (eq (length new-lst) steps)
	      finally (return (append (list-without-last new-lst)
				      (list aim)))))
	  ((> start aim)
	   (loop for i from start downto aim by stepsize
	      collect (float i) into new-lst
	      until (eq (length new-lst) steps)
	      finally (return (append (list-without-last new-lst)
				      (list aim)))))
	  ((eq start aim)
	   (repeat start steps)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; linseg is creating linear segments from a val-a by n-steps to val-n
;; and so on
;;(linseg '(0 5 1 3 0.5 5 7)) => (0.0 0.25 0.5 0.75 1 1.0 0.75 0.5 0.5 2.125 3.75 5.375 7)
(defun linseg (segments)
  (cond ((< (length segments) 3) nil)
	(t (append (line (first segments)
			 (second segments)
			 (third segments))
		   (linseg (cddr segments))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; csound functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fillarray creates a file that can be included in a csound programm
;; which holds an csound array
(defun fillarray (name lst path)
  (with-open-file (orc path :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
    (format orc "~a[] fillarray ~{~f~^,~}~%" name lst)))

;(fillarray "iArray" '(1 2 3 2 3 4 3 2 1 2) "~/Desktop/test.txt")

;; create a list which will be read into a table and then copied to a array
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

(defun list-to-gen23 (lst path)
  (with-open-file (orc path :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
    (format orc "~{~f~^,~}~%" lst)))

;; takes a nested list in this form ((dur val) (dur val) (...)) and
;; creates a textfile to be used inside csound as a repleacement for
;; the linseg opcode
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
;;(linseg-opcode "aLine" 0 '((10 5) (2 1) (6 9)) "/Users/philippneumann/Desktop/test.csd")list-to-gen23
(defun expseg-opcode (name start segments path)
  (with-open-file (orc path :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
    (format orc "~a expseg ~{~f~^,~}~%" name
	    (append (list start) (seg-op-h segments)))))
